/**
 * Computes RIPEMD-160 hashes of arbitrary data. RIPEMD-160 hashes are 20 byte quantities
 * that are like a checksum or CRC, but are more robust.
 *
$(SCRIPT inhibitQuickIndex = 1;)

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Category) $(TH Functions)
)
$(TR $(TDNW Template API) $(TD $(MYREF RIPEMD160)
)
)
$(TR $(TDNW OOP API) $(TD $(MYREF RIPEMD160Digest))
)
$(TR $(TDNW Helpers) $(TD $(MYREF ripemd160Of))
)
)
)

 * This module conforms to the APIs defined in $(MREF std, digest). To understand the
 * differences between the template and the OOP API, see $(MREF std, digest).
 *
 * This module publicly imports $(D std.digest) and can be used as a stand-alone
 * module.
 *
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * CTFE:
 * Digests do not work in CTFE
 *
 * Authors:
 * Kai Nacke $(BR)
 * The algorithm was designed by Hans Dobbertin, Antoon Bosselaers, and Bart Preneel. $(BR)
 * The D implementation is a direct translation of the ANSI C implementation by Antoon Bosselaers.
 *
 * References:
 * $(UL
 * $(LI $(LINK2 http://homes.esat.kuleuven.be/~bosselae/ripemd160.html, The hash function RIPEMD-160))
 * $(LI $(LINK2 http://en.wikipedia.org/wiki/RIPEMD-160, Wikipedia on RIPEMD-160))
 * )
 *
 * Source: $(PHOBOSSRC std/digest/_ripemd.d)
 *
 */

module std.digest.ripemd;

public import std.digest;

///
@safe unittest
{
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

///
@safe unittest
{
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

//rotateLeft rotates x left n bits
private uint rotateLeft(uint x, uint n) @safe pure nothrow @nogc
{
    // With recently added optimization to DMD (commit 32ea0206 at 07/28/11), this is translated to rol.
    // No assembler required.
    return (x << n) | (x >> (32-n));
}

/**
 * Template API RIPEMD160 implementation.
 * See $(D std.digest) for differences between template and OOP API.
 */
struct RIPEMD160
{
    private:
        // magic initialization constants
        uint[5] _state = [0x67452301,0xefcdab89,0x98badcfe,0x10325476,0xc3d2e1f0]; // state (ABCDE)
        ulong _count; //number of bits, modulo 2^64
        ubyte[64] _buffer; // input buffer

        static immutable ubyte[64] _padding =
        [
          0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        ];

        // F, G, H, I and J are basic RIPEMD160 functions
        static @safe pure nothrow @nogc
        {
            uint F(uint x, uint y, uint z) { return x ^ y ^ z; }
            uint G(uint x, uint y, uint z) { return (x & y) | (~x & z); }
            uint H(uint x, uint y, uint z) { return (x | ~y) ^ z; }
            uint I(uint x, uint y, uint z) { return (x & z) | (y & ~z); }
            uint J(uint x, uint y, uint z) { return x ^ (y | ~z); }
        }

        /*
         * FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
         * Rotation is separate from addition to prevent recomputation.
         */

        /* the ten basic operations FF() through III() */
        static void FF(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += F(b, c, d) + x;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void GG(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += G(b, c, d) + x + 0x5a827999UL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void HH(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += H(b, c, d) + x + 0x6ed9eba1UL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void II(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += I(b, c, d) + x + 0x8f1bbcdcUL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void JJ(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += J(b, c, d) + x + 0xa953fd4eUL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        /*
         * FFF, GGG, HHH, and III transformations for parallel rounds 1, 2, 3, and 4.
         * Rotation is separate from addition to prevent recomputation.
         */

        static void FFF(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += F(b, c, d) + x;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void GGG(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += G(b, c, d) + x + 0x7a6d76e9UL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void HHH(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += H(b, c, d) + x + 0x6d703ef3UL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void III(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += I(b, c, d) + x + 0x5c4dd124UL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        static void JJJ(ref uint a, uint b, ref uint c, uint d, uint e, uint x, uint s)
            @safe pure nothrow @nogc
        {
            a += J(b, c, d) + x + 0x50a28be6UL;
            a = rotateLeft(a, s) + e;
            c = rotateLeft(c, 10);
        }

        /*
         * RIPEMD160 basic transformation. Transforms state based on block.
         */

        private void transform(const(ubyte[64])* block)
            pure nothrow @nogc
        {
            uint aa = _state[0],
                 bb = _state[1],
                 cc = _state[2],
                 dd = _state[3],
                 ee = _state[4];
            uint aaa = _state[0],
                 bbb = _state[1],
                 ccc = _state[2],
                 ddd = _state[3],
                 eee = _state[4];

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

            /* round 1 */
            FF(aa, bb, cc, dd, ee, x[ 0], 11);
            FF(ee, aa, bb, cc, dd, x[ 1], 14);
            FF(dd, ee, aa, bb, cc, x[ 2], 15);
            FF(cc, dd, ee, aa, bb, x[ 3], 12);
            FF(bb, cc, dd, ee, aa, x[ 4],  5);
            FF(aa, bb, cc, dd, ee, x[ 5],  8);
            FF(ee, aa, bb, cc, dd, x[ 6],  7);
            FF(dd, ee, aa, bb, cc, x[ 7],  9);
            FF(cc, dd, ee, aa, bb, x[ 8], 11);
            FF(bb, cc, dd, ee, aa, x[ 9], 13);
            FF(aa, bb, cc, dd, ee, x[10], 14);
            FF(ee, aa, bb, cc, dd, x[11], 15);
            FF(dd, ee, aa, bb, cc, x[12],  6);
            FF(cc, dd, ee, aa, bb, x[13],  7);
            FF(bb, cc, dd, ee, aa, x[14],  9);
            FF(aa, bb, cc, dd, ee, x[15],  8);

            /* round 2 */
            GG(ee, aa, bb, cc, dd, x[ 7],  7);
            GG(dd, ee, aa, bb, cc, x[ 4],  6);
            GG(cc, dd, ee, aa, bb, x[13],  8);
            GG(bb, cc, dd, ee, aa, x[ 1], 13);
            GG(aa, bb, cc, dd, ee, x[10], 11);
            GG(ee, aa, bb, cc, dd, x[ 6],  9);
            GG(dd, ee, aa, bb, cc, x[15],  7);
            GG(cc, dd, ee, aa, bb, x[ 3], 15);
            GG(bb, cc, dd, ee, aa, x[12],  7);
            GG(aa, bb, cc, dd, ee, x[ 0], 12);
            GG(ee, aa, bb, cc, dd, x[ 9], 15);
            GG(dd, ee, aa, bb, cc, x[ 5],  9);
            GG(cc, dd, ee, aa, bb, x[ 2], 11);
            GG(bb, cc, dd, ee, aa, x[14],  7);
            GG(aa, bb, cc, dd, ee, x[11], 13);
            GG(ee, aa, bb, cc, dd, x[ 8], 12);

            /* round 3 */
            HH(dd, ee, aa, bb, cc, x[ 3], 11);
            HH(cc, dd, ee, aa, bb, x[10], 13);
            HH(bb, cc, dd, ee, aa, x[14],  6);
            HH(aa, bb, cc, dd, ee, x[ 4],  7);
            HH(ee, aa, bb, cc, dd, x[ 9], 14);
            HH(dd, ee, aa, bb, cc, x[15],  9);
            HH(cc, dd, ee, aa, bb, x[ 8], 13);
            HH(bb, cc, dd, ee, aa, x[ 1], 15);
            HH(aa, bb, cc, dd, ee, x[ 2], 14);
            HH(ee, aa, bb, cc, dd, x[ 7],  8);
            HH(dd, ee, aa, bb, cc, x[ 0], 13);
            HH(cc, dd, ee, aa, bb, x[ 6],  6);
            HH(bb, cc, dd, ee, aa, x[13],  5);
            HH(aa, bb, cc, dd, ee, x[11], 12);
            HH(ee, aa, bb, cc, dd, x[ 5],  7);
            HH(dd, ee, aa, bb, cc, x[12],  5);

            /* round 4 */
            II(cc, dd, ee, aa, bb, x[ 1], 11);
            II(bb, cc, dd, ee, aa, x[ 9], 12);
            II(aa, bb, cc, dd, ee, x[11], 14);
            II(ee, aa, bb, cc, dd, x[10], 15);
            II(dd, ee, aa, bb, cc, x[ 0], 14);
            II(cc, dd, ee, aa, bb, x[ 8], 15);
            II(bb, cc, dd, ee, aa, x[12],  9);
            II(aa, bb, cc, dd, ee, x[ 4],  8);
            II(ee, aa, bb, cc, dd, x[13],  9);
            II(dd, ee, aa, bb, cc, x[ 3], 14);
            II(cc, dd, ee, aa, bb, x[ 7],  5);
            II(bb, cc, dd, ee, aa, x[15],  6);
            II(aa, bb, cc, dd, ee, x[14],  8);
            II(ee, aa, bb, cc, dd, x[ 5],  6);
            II(dd, ee, aa, bb, cc, x[ 6],  5);
            II(cc, dd, ee, aa, bb, x[ 2], 12);

            /* round 5 */
            JJ(bb, cc, dd, ee, aa, x[ 4],  9);
            JJ(aa, bb, cc, dd, ee, x[ 0], 15);
            JJ(ee, aa, bb, cc, dd, x[ 5],  5);
            JJ(dd, ee, aa, bb, cc, x[ 9], 11);
            JJ(cc, dd, ee, aa, bb, x[ 7],  6);
            JJ(bb, cc, dd, ee, aa, x[12],  8);
            JJ(aa, bb, cc, dd, ee, x[ 2], 13);
            JJ(ee, aa, bb, cc, dd, x[10], 12);
            JJ(dd, ee, aa, bb, cc, x[14],  5);
            JJ(cc, dd, ee, aa, bb, x[ 1], 12);
            JJ(bb, cc, dd, ee, aa, x[ 3], 13);
            JJ(aa, bb, cc, dd, ee, x[ 8], 14);
            JJ(ee, aa, bb, cc, dd, x[11], 11);
            JJ(dd, ee, aa, bb, cc, x[ 6],  8);
            JJ(cc, dd, ee, aa, bb, x[15],  5);
            JJ(bb, cc, dd, ee, aa, x[13],  6);

            /* parallel round 1 */
            JJJ(aaa, bbb, ccc, ddd, eee, x[ 5],  8);
            JJJ(eee, aaa, bbb, ccc, ddd, x[14],  9);
            JJJ(ddd, eee, aaa, bbb, ccc, x[ 7],  9);
            JJJ(ccc, ddd, eee, aaa, bbb, x[ 0], 11);
            JJJ(bbb, ccc, ddd, eee, aaa, x[ 9], 13);
            JJJ(aaa, bbb, ccc, ddd, eee, x[ 2], 15);
            JJJ(eee, aaa, bbb, ccc, ddd, x[11], 15);
            JJJ(ddd, eee, aaa, bbb, ccc, x[ 4],  5);
            JJJ(ccc, ddd, eee, aaa, bbb, x[13],  7);
            JJJ(bbb, ccc, ddd, eee, aaa, x[ 6],  7);
            JJJ(aaa, bbb, ccc, ddd, eee, x[15],  8);
            JJJ(eee, aaa, bbb, ccc, ddd, x[ 8], 11);
            JJJ(ddd, eee, aaa, bbb, ccc, x[ 1], 14);
            JJJ(ccc, ddd, eee, aaa, bbb, x[10], 14);
            JJJ(bbb, ccc, ddd, eee, aaa, x[ 3], 12);
            JJJ(aaa, bbb, ccc, ddd, eee, x[12],  6);

            /* parallel round 2 */
            III(eee, aaa, bbb, ccc, ddd, x[ 6],  9);
            III(ddd, eee, aaa, bbb, ccc, x[11], 13);
            III(ccc, ddd, eee, aaa, bbb, x[ 3], 15);
            III(bbb, ccc, ddd, eee, aaa, x[ 7],  7);
            III(aaa, bbb, ccc, ddd, eee, x[ 0], 12);
            III(eee, aaa, bbb, ccc, ddd, x[13],  8);
            III(ddd, eee, aaa, bbb, ccc, x[ 5],  9);
            III(ccc, ddd, eee, aaa, bbb, x[10], 11);
            III(bbb, ccc, ddd, eee, aaa, x[14],  7);
            III(aaa, bbb, ccc, ddd, eee, x[15],  7);
            III(eee, aaa, bbb, ccc, ddd, x[ 8], 12);
            III(ddd, eee, aaa, bbb, ccc, x[12],  7);
            III(ccc, ddd, eee, aaa, bbb, x[ 4],  6);
            III(bbb, ccc, ddd, eee, aaa, x[ 9], 15);
            III(aaa, bbb, ccc, ddd, eee, x[ 1], 13);
            III(eee, aaa, bbb, ccc, ddd, x[ 2], 11);

            /* parallel round 3 */
            HHH(ddd, eee, aaa, bbb, ccc, x[15],  9);
            HHH(ccc, ddd, eee, aaa, bbb, x[ 5],  7);
            HHH(bbb, ccc, ddd, eee, aaa, x[ 1], 15);
            HHH(aaa, bbb, ccc, ddd, eee, x[ 3], 11);
            HHH(eee, aaa, bbb, ccc, ddd, x[ 7],  8);
            HHH(ddd, eee, aaa, bbb, ccc, x[14],  6);
            HHH(ccc, ddd, eee, aaa, bbb, x[ 6],  6);
            HHH(bbb, ccc, ddd, eee, aaa, x[ 9], 14);
            HHH(aaa, bbb, ccc, ddd, eee, x[11], 12);
            HHH(eee, aaa, bbb, ccc, ddd, x[ 8], 13);
            HHH(ddd, eee, aaa, bbb, ccc, x[12],  5);
            HHH(ccc, ddd, eee, aaa, bbb, x[ 2], 14);
            HHH(bbb, ccc, ddd, eee, aaa, x[10], 13);
            HHH(aaa, bbb, ccc, ddd, eee, x[ 0], 13);
            HHH(eee, aaa, bbb, ccc, ddd, x[ 4],  7);
            HHH(ddd, eee, aaa, bbb, ccc, x[13],  5);

            /* parallel round 4 */
            GGG(ccc, ddd, eee, aaa, bbb, x[ 8], 15);
            GGG(bbb, ccc, ddd, eee, aaa, x[ 6],  5);
            GGG(aaa, bbb, ccc, ddd, eee, x[ 4],  8);
            GGG(eee, aaa, bbb, ccc, ddd, x[ 1], 11);
            GGG(ddd, eee, aaa, bbb, ccc, x[ 3], 14);
            GGG(ccc, ddd, eee, aaa, bbb, x[11], 14);
            GGG(bbb, ccc, ddd, eee, aaa, x[15],  6);
            GGG(aaa, bbb, ccc, ddd, eee, x[ 0], 14);
            GGG(eee, aaa, bbb, ccc, ddd, x[ 5],  6);
            GGG(ddd, eee, aaa, bbb, ccc, x[12],  9);
            GGG(ccc, ddd, eee, aaa, bbb, x[ 2], 12);
            GGG(bbb, ccc, ddd, eee, aaa, x[13],  9);
            GGG(aaa, bbb, ccc, ddd, eee, x[ 9], 12);
            GGG(eee, aaa, bbb, ccc, ddd, x[ 7],  5);
            GGG(ddd, eee, aaa, bbb, ccc, x[10], 15);
            GGG(ccc, ddd, eee, aaa, bbb, x[14],  8);

            /* parallel round 5 */
            FFF(bbb, ccc, ddd, eee, aaa, x[12] ,  8);
            FFF(aaa, bbb, ccc, ddd, eee, x[15] ,  5);
            FFF(eee, aaa, bbb, ccc, ddd, x[10] , 12);
            FFF(ddd, eee, aaa, bbb, ccc, x[ 4] ,  9);
            FFF(ccc, ddd, eee, aaa, bbb, x[ 1] , 12);
            FFF(bbb, ccc, ddd, eee, aaa, x[ 5] ,  5);
            FFF(aaa, bbb, ccc, ddd, eee, x[ 8] , 14);
            FFF(eee, aaa, bbb, ccc, ddd, x[ 7] ,  6);
            FFF(ddd, eee, aaa, bbb, ccc, x[ 6] ,  8);
            FFF(ccc, ddd, eee, aaa, bbb, x[ 2] , 13);
            FFF(bbb, ccc, ddd, eee, aaa, x[13] ,  6);
            FFF(aaa, bbb, ccc, ddd, eee, x[14] ,  5);
            FFF(eee, aaa, bbb, ccc, ddd, x[ 0] , 15);
            FFF(ddd, eee, aaa, bbb, ccc, x[ 3] , 13);
            FFF(ccc, ddd, eee, aaa, bbb, x[ 9] , 11);
            FFF(bbb, ccc, ddd, eee, aaa, x[11] , 11);

            /* combine results */
            ddd += cc + _state[1];               /* final result for _state[0] */
            _state[1] = _state[2] + dd + eee;
            _state[2] = _state[3] + ee + aaa;
            _state[3] = _state[4] + aa + bbb;
            _state[4] = _state[0] + bb + ccc;
            _state[0] = ddd;

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
         * RIPEMD160 dig;
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
         * Used to (re)initialize the RIPEMD160 digest.
         *
         * Note:
         * For this RIPEMD160 Digest implementation calling start after default construction
         * is not necessary. Calling start is only necessary to reset the Digest.
         *
         * Generic code which deals with different Digest types should always call start though.
         *
         * Example:
         * --------
         * RIPEMD160 digest;
         * //digest.start(); //Not necessary
         * digest.put(0);
         * --------
         */
        void start() @safe pure nothrow @nogc
        {
            this = RIPEMD160.init;
        }

        /**
         * Returns the finished RIPEMD160 hash. This also calls $(LREF start) to
         * reset the internal state.
         *
         * Example:
         * --------
         * //Simple example
         * RIPEMD160 hash;
         * hash.start();
         * hash.put(cast(ubyte) 0);
         * ubyte[20] result = hash.finish();
         * assert(toHexString(result) == "C81B94933420221A7AC004A90242D8B1D3E5070D");
         * --------
         */
        ubyte[20] finish() @trusted pure nothrow @nogc
        {
            import std.bitmanip : nativeToLittleEndian;

            ubyte[20] data = void;
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
            data[16 .. 20] = nativeToLittleEndian(_state[4])[];

            /* Zeroize sensitive information. */
            start();
            return data;
        }
}

///
@safe unittest
{
    //Simple example, hashing a string using ripemd160Of helper function
    ubyte[20] hash = ripemd160Of("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC");
}

///
@safe unittest
{
    //Using the basic API
    RIPEMD160 hash;
    hash.start();
    ubyte[1024] data;
    //Initialize data here...
    hash.put(data);
    ubyte[20] result = hash.finish();
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
    RIPEMD160 md;
    md.start();
    doSomething(md);
    assert(toHexString(md.finish()) == "C81B94933420221A7AC004A90242D8B1D3E5070D");
}

///
@safe unittest
{
    //Simple example
    RIPEMD160 hash;
    hash.start();
    hash.put(cast(ubyte) 0);
    ubyte[20] result = hash.finish();
    assert(toHexString(result) == "C81B94933420221A7AC004A90242D8B1D3E5070D");
}

@safe unittest
{
    assert(isDigest!RIPEMD160);
}

@system unittest
{
    import std.range;

    ubyte[20] digest;

    RIPEMD160 md;
    md.put(cast(ubyte[])"abcdef");
    md.start();
    md.put(cast(ubyte[])"");
    assert(md.finish() == cast(ubyte[]) x"9c1185a5c5e9fc54612808977ee8f548b2258d31");

    digest = ripemd160Of("");
    assert(digest == cast(ubyte[]) x"9c1185a5c5e9fc54612808977ee8f548b2258d31");

    digest = ripemd160Of("a");
    assert(digest == cast(ubyte[]) x"0bdc9d2d256b3ee9daae347be6f4dc835a467ffe");

    digest = ripemd160Of("abc");
    assert(digest == cast(ubyte[]) x"8eb208f7e05d987a9b044a8e98c6b087f15a0bfc");

    digest = ripemd160Of("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(digest == cast(ubyte[]) x"12a053384a9c0c88e405a06c27dcf49ada62eb2b");

    digest = ripemd160Of("message digest");
    assert(digest == cast(ubyte[]) x"5d0689ef49d2fae572b881b123a85ffa21595f36");

    digest = ripemd160Of("abcdefghijklmnopqrstuvwxyz");
    assert(digest == cast(ubyte[]) x"f71c27109c692c1b56bbdceb5b9d2865b3708dbc");

    digest = ripemd160Of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    assert(digest == cast(ubyte[]) x"b0e20b6e3116640286ed3a87a5713079b21f5189");

    digest = ripemd160Of("1234567890123456789012345678901234567890"~
                    "1234567890123456789012345678901234567890");
    assert(digest == cast(ubyte[]) x"9b752e45573d4b39f4dbd3323cab82bf63326bfb");

    assert(toHexString(cast(ubyte[20]) x"f71c27109c692c1b56bbdceb5b9d2865b3708dbc")
        == "F71C27109C692C1B56BBDCEB5B9D2865B3708DBC");

    ubyte[] onemilliona = new ubyte[1000000];
    onemilliona[] = 'a';
    digest = ripemd160Of(onemilliona);
    assert(digest == cast(ubyte[]) x"52783243c1697bdbe16d37f97f68f08325dc1528");

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    digest = ripemd160Of(oneMillionRange);
    assert(digest == cast(ubyte[]) x"52783243c1697bdbe16d37f97f68f08325dc1528");
}

/**
 * This is a convenience alias for $(REF digest, std,digest) using the
 * RIPEMD160 implementation.
 */
//simple alias doesn't work here, hope this gets inlined...
auto ripemd160Of(T...)(T data)
{
    return digest!(RIPEMD160, T)(data);
}

///
@safe unittest
{
    ubyte[20] hash = ripemd160Of("abc");
    assert(hash == digest!RIPEMD160("abc"));
}

/**
 * OOP API RIPEMD160 implementation.
 * See $(D std.digest) for differences between template and OOP API.
 *
 * This is an alias for $(D $(REF WrapperDigest, std,digest)!RIPEMD160),
 * see there for more information.
 */
alias RIPEMD160Digest = WrapperDigest!RIPEMD160;

///
@safe unittest
{
    //Simple example, hashing a string using Digest.digest helper function
    auto md = new RIPEMD160Digest();
    ubyte[] hash = md.digest("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC");
}

///
@system unittest
{
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

@system unittest
{
    auto md = new RIPEMD160Digest();

    md.put(cast(ubyte[])"abcdef");
    md.reset();
    md.put(cast(ubyte[])"");
    assert(md.finish() == cast(ubyte[]) x"9c1185a5c5e9fc54612808977ee8f548b2258d31");

    md.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    ubyte[20] result;
    auto result2 = md.finish(result[]);
    assert(result[0 .. 20] == result2 && result2 == cast(ubyte[]) x"f71c27109c692c1b56bbdceb5b9d2865b3708dbc");

    debug
    {
        import std.exception;
        assertThrown!Error(md.finish(result[0 .. 19]));
    }

    assert(md.length == 20);

    assert(md.digest("") == cast(ubyte[]) x"9c1185a5c5e9fc54612808977ee8f548b2258d31");

    assert(md.digest("a") == cast(ubyte[]) x"0bdc9d2d256b3ee9daae347be6f4dc835a467ffe");

    assert(md.digest("abc") == cast(ubyte[]) x"8eb208f7e05d987a9b044a8e98c6b087f15a0bfc");

    assert(md.digest("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
           == cast(ubyte[]) x"12a053384a9c0c88e405a06c27dcf49ada62eb2b");

    assert(md.digest("message digest") == cast(ubyte[]) x"5d0689ef49d2fae572b881b123a85ffa21595f36");

    assert(md.digest("abcdefghijklmnopqrstuvwxyz")
           == cast(ubyte[]) x"f71c27109c692c1b56bbdceb5b9d2865b3708dbc");

    assert(md.digest("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
           == cast(ubyte[]) x"b0e20b6e3116640286ed3a87a5713079b21f5189");

    assert(md.digest("1234567890123456789012345678901234567890",
                                   "1234567890123456789012345678901234567890")
           == cast(ubyte[]) x"9b752e45573d4b39f4dbd3323cab82bf63326bfb");

    assert(md.digest(new ubyte[160/8]) // 160 zero bits
           == cast(ubyte[]) x"5c00bd4aca04a9057c09b20b05f723f2e23deb65");
}

// Written in the D programming language.
/**
 * Computes SHA1 and SHA2 hashes of arbitrary data. SHA hashes are 20 to 64 byte
 * quantities (depending on the SHA algorithm) that are like a checksum or CRC,
 * but are more robust.
 *
$(SCRIPT inhibitQuickIndex = 1;)

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Category) $(TH Functions)
)
$(TR $(TDNW Template API) $(TD $(MYREF SHA1)
)
)
$(TR $(TDNW OOP API) $(TD $(MYREF SHA1Digest))
)
$(TR $(TDNW Helpers) $(TD $(MYREF sha1Of))
)
)
)

 * SHA2 comes in several different versions, all supported by this module:
 * SHA-224, SHA-256, SHA-384, SHA-512, SHA-512/224 and SHA-512/256.
 *
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
 * The routines and algorithms are derived from the
 * $(I Secure Hash Signature Standard (SHS) (FIPS PUB 180-2)). $(BR )
 * Kai Nacke, Johannes Pfau, Nick Sabalausky
 *
 * References:
 * $(UL
 * $(LI $(LINK2 http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf, FIPS PUB180-2))
 * $(LI $(LINK2 http://software.intel.com/en-us/articles/improving-the-performance-of-the-secure-hash-algorithm-1/, Fast implementation of SHA1))
 * $(LI $(LINK2 http://en.wikipedia.org/wiki/Secure_Hash_Algorithm, Wikipedia article about SHA))
 * )
 *
 * Source: $(PHOBOSSRC std/digest/_sha.d)
 *
 */

/*          Copyright Kai Nacke 2012.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.digest.sha;

///
@safe unittest
{
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

///
@safe unittest
{
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

version (Win64)
{
    // wrong calling convention
}
else version (D_InlineAsm_X86)
{
    version (D_PIC) {} // Bugzilla 9378
    else private version = USE_SSSE3;
}
else version (D_InlineAsm_X86_64)
{
    private version = USE_SSSE3;
}

version (LittleEndian) import core.bitop : bswap;


version (unittest)
{
    import std.exception;
}


public import std.digest;

/*
 * Helper methods for encoding the buffer.
 * Can be removed if the optimizer can inline the methods from std.bitmanip.
 */
private ubyte[8] nativeToBigEndian(ulong val) @trusted pure nothrow @nogc
{
    version (LittleEndian)
        immutable ulong res = (cast(ulong)  bswap(cast(uint) val)) << 32 | bswap(cast(uint) (val >> 32));
    else
        immutable ulong res = val;
    return *cast(ubyte[8]*) &res;
}

private ubyte[4] nativeToBigEndian(uint val) @trusted pure nothrow @nogc
{
    version (LittleEndian)
        immutable uint res = bswap(val);
    else
        immutable uint res = val;
    return *cast(ubyte[4]*) &res;
}

private ulong bigEndianToNative(ubyte[8] val) @trusted pure nothrow @nogc
{
    version (LittleEndian)
    {
        import std.bitmanip : bigEndianToNative;
        return bigEndianToNative!ulong(val);
    }
    else
        return *cast(ulong*) &val;
}

private uint bigEndianToNative(ubyte[4] val) @trusted pure nothrow @nogc
{
    version (LittleEndian)
        return bswap(*cast(uint*) &val);
    else
        return *cast(uint*) &val;
}

//rotateLeft rotates x left n bits
private uint rotateLeft(uint x, uint n) @safe pure nothrow @nogc
{
    // With recently added optimization to DMD (commit 32ea0206 at 07/28/11), this is translated to rol.
    // No assembler required.
    return (x << n) | (x >> (32-n));
}

//rotateRight rotates x right n bits
private uint rotateRight(uint x, uint n) @safe pure nothrow @nogc
{
    return (x >> n) | (x << (32-n));
}
private ulong rotateRight(ulong x, uint n) @safe pure nothrow @nogc
{
    return (x >> n) | (x << (64-n));
}

/**
 * Template API SHA1/SHA2 implementation. Supports: SHA-1, SHA-224, SHA-256,
 * SHA-384, SHA-512, SHA-512/224 and SHA-512/256.
 *
 * The hashBlockSize and digestSize are in bits. However, it's likely easier to
 * simply use the convenience aliases: SHA1, SHA224, SHA256, SHA384, SHA512,
 * SHA512_224 and SHA512_256.
 *
 * See $(D std.digest) for differences between template and OOP API.
 */
struct SHA(uint hashBlockSize, uint digestSize)
{
    enum blockSize = hashBlockSize;

    static assert(blockSize == 512 || blockSize == 1024,
        "Invalid SHA blockSize, must be 512 or 1024");
    static assert(digestSize == 160 || digestSize == 224 || digestSize == 256 || digestSize == 384 || digestSize == 512,
        "Invalid SHA digestSize, must be 224, 256, 384 or 512");
    static assert(!(blockSize == 512 && digestSize > 256),
        "Invalid SHA digestSize for a blockSize of 512. The digestSize must be 160, 224 or 256.");
    static assert(!(blockSize == 1024 && digestSize < 224),
        "Invalid SHA digestSize for a blockSize of 1024. The digestSize must be 224, 256, 384 or 512.");

    static if (digestSize == 160) /* SHA-1 */
    {
        version (USE_SSSE3)
        {
            import core.cpuid : ssse3;
            import std.internal.digest.sha_SSSE3 : sse3_constants=constants, transformSSSE3;

            static void transform(uint[5]* state, const(ubyte[64])* block) pure nothrow @nogc
            {
                if (ssse3)
                {
                    version (D_InlineAsm_X86_64)
                        // constants as extra argument for PIC, see Bugzilla 9378
                        transformSSSE3(state, block, &sse3_constants);
                    else
                        transformSSSE3(state, block);
                }
                else
                    transformX86(state, block);
            }
        }
        else
        {
            alias transform = transformX86;
        }
    }
    else static if (blockSize == 512) /* SHA-224, SHA-256 */
        alias transform = transformSHA2!uint;
    else static if (blockSize == 1024) /* SHA-384, SHA-512, SHA-512/224, SHA-512/256 */
        alias transform = transformSHA2!ulong;
    else
        static assert(0);

    private:
        /* magic initialization constants - state (ABCDEFGH) */
        static if (blockSize == 512 && digestSize == 160) /* SHA-1 */
        {
            uint[5] state =
            [0x67452301,0xefcdab89,0x98badcfe,0x10325476,0xc3d2e1f0];
        }
        else static if (blockSize == 512 && digestSize == 224) /* SHA-224 */
        {
            uint[8] state = [
                0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939,
                0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4,
            ];
        }
        else static if (blockSize == 512 && digestSize == 256) /* SHA-256 */
        {
            uint[8] state = [
                0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
                0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
            ];
        }
        else static if (blockSize == 1024 && digestSize == 224) /* SHA-512/224 */
        {
            ulong[8] state = [
                0x8C3D37C8_19544DA2, 0x73E19966_89DCD4D6,
                0x1DFAB7AE_32FF9C82, 0x679DD514_582F9FCF,
                0x0F6D2B69_7BD44DA8, 0x77E36F73_04C48942,
                0x3F9D85A8_6A1D36C8, 0x1112E6AD_91D692A1,
            ];
        }
        else static if (blockSize == 1024 && digestSize == 256) /* SHA-512/256 */
        {
            ulong[8] state = [
                0x22312194_FC2BF72C, 0x9F555FA3_C84C64C2,
                0x2393B86B_6F53B151, 0x96387719_5940EABD,
                0x96283EE2_A88EFFE3, 0xBE5E1E25_53863992,
                0x2B0199FC_2C85B8AA, 0x0EB72DDC_81C52CA2,
            ];
        }
        else static if (blockSize == 1024 && digestSize == 384) /* SHA-384 */
        {
            ulong[8] state = [
                0xcbbb9d5d_c1059ed8, 0x629a292a_367cd507,
                0x9159015a_3070dd17, 0x152fecd8_f70e5939,
                0x67332667_ffc00b31, 0x8eb44a87_68581511,
                0xdb0c2e0d_64f98fa7, 0x47b5481d_befa4fa4,
            ];
        }
        else static if (blockSize == 1024 && digestSize == 512) /* SHA-512 */
        {
            ulong[8] state = [
                0x6a09e667_f3bcc908, 0xbb67ae85_84caa73b,
                0x3c6ef372_fe94f82b, 0xa54ff53a_5f1d36f1,
                0x510e527f_ade682d1, 0x9b05688c_2b3e6c1f,
                0x1f83d9ab_fb41bd6b, 0x5be0cd19_137e2179,
            ];
        }
        else
            static assert(0);

        /* constants */
        static if (blockSize == 512)
        {
            static immutable uint[64] constants = [
                0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
                0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
                0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
                0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
                0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
                0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
                0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
                0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
            ];
        }
        else static if (blockSize == 1024)
        {
            static immutable ulong[80] constants = [
                0x428a2f98_d728ae22, 0x71374491_23ef65cd, 0xb5c0fbcf_ec4d3b2f, 0xe9b5dba5_8189dbbc,
                0x3956c25b_f348b538, 0x59f111f1_b605d019, 0x923f82a4_af194f9b, 0xab1c5ed5_da6d8118,
                0xd807aa98_a3030242, 0x12835b01_45706fbe, 0x243185be_4ee4b28c, 0x550c7dc3_d5ffb4e2,
                0x72be5d74_f27b896f, 0x80deb1fe_3b1696b1, 0x9bdc06a7_25c71235, 0xc19bf174_cf692694,
                0xe49b69c1_9ef14ad2, 0xefbe4786_384f25e3, 0x0fc19dc6_8b8cd5b5, 0x240ca1cc_77ac9c65,
                0x2de92c6f_592b0275, 0x4a7484aa_6ea6e483, 0x5cb0a9dc_bd41fbd4, 0x76f988da_831153b5,
                0x983e5152_ee66dfab, 0xa831c66d_2db43210, 0xb00327c8_98fb213f, 0xbf597fc7_beef0ee4,
                0xc6e00bf3_3da88fc2, 0xd5a79147_930aa725, 0x06ca6351_e003826f, 0x14292967_0a0e6e70,
                0x27b70a85_46d22ffc, 0x2e1b2138_5c26c926, 0x4d2c6dfc_5ac42aed, 0x53380d13_9d95b3df,
                0x650a7354_8baf63de, 0x766a0abb_3c77b2a8, 0x81c2c92e_47edaee6, 0x92722c85_1482353b,
                0xa2bfe8a1_4cf10364, 0xa81a664b_bc423001, 0xc24b8b70_d0f89791, 0xc76c51a3_0654be30,
                0xd192e819_d6ef5218, 0xd6990624_5565a910, 0xf40e3585_5771202a, 0x106aa070_32bbd1b8,
                0x19a4c116_b8d2d0c8, 0x1e376c08_5141ab53, 0x2748774c_df8eeb99, 0x34b0bcb5_e19b48a8,
                0x391c0cb3_c5c95a63, 0x4ed8aa4a_e3418acb, 0x5b9cca4f_7763e373, 0x682e6ff3_d6b2b8a3,
                0x748f82ee_5defb2fc, 0x78a5636f_43172f60, 0x84c87814_a1f0ab72, 0x8cc70208_1a6439ec,
                0x90befffa_23631e28, 0xa4506ceb_de82bde9, 0xbef9a3f7_b2c67915, 0xc67178f2_e372532b,
                0xca273ece_ea26619c, 0xd186b8c7_21c0c207, 0xeada7dd6_cde0eb1e, 0xf57d4f7f_ee6ed178,
                0x06f067aa_72176fba, 0x0a637dc5_a2c898a6, 0x113f9804_bef90dae, 0x1b710b35_131c471b,
                0x28db77f5_23047d84, 0x32caab7b_40c72493, 0x3c9ebe0a_15c9bebc, 0x431d67c4_9c100d4c,
                0x4cc5d4be_cb3e42b6, 0x597f299c_fc657e2a, 0x5fcb6fab_3ad6faec, 0x6c44198c_4a475817,
            ];
        }
        else
            static assert(0);

        /*
         * number of bits, modulo 2^64 (ulong[1]) or 2^128 (ulong[2]),
         * should just use ucent instead of ulong[2] once it's available
         */
        ulong[blockSize/512] count;
        ubyte[blockSize/8]   buffer; /* input buffer */

        static immutable ubyte[128] padding =
        [
          0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

          0x00, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        ];

        /*
         * Basic SHA1/SHA2 functions.
         */
        static @safe pure nothrow @nogc
        {
            /* All SHA1/SHA2 */
            T Ch(T)(T x, T y, T z) { return z ^ (x & (y ^ z)); }
            T Maj(T)(T x, T y, T z) { return (x & y) | (z & (x ^ y)); }

            /* SHA-1 */
            uint Parity(uint x, uint y, uint z) { return x ^ y ^ z; }

            /* SHA-224, SHA-256 */
            uint BigSigma0(uint x) { return rotateRight(x, 2) ^ rotateRight(x, 13) ^ rotateRight(x, 22); }
            uint BigSigma1(uint x) { return rotateRight(x, 6) ^ rotateRight(x, 11) ^ rotateRight(x, 25); }
            uint SmSigma0(uint x) { return rotateRight(x, 7) ^ rotateRight(x, 18) ^ x >> 3; }
            uint SmSigma1(uint x) { return rotateRight(x, 17) ^ rotateRight(x, 19) ^ x >> 10; }

            /* SHA-384, SHA-512, SHA-512/224, SHA-512/256 */
            ulong BigSigma0(ulong x) { return rotateRight(x, 28) ^ rotateRight(x, 34) ^ rotateRight(x, 39); }
            ulong BigSigma1(ulong x) { return rotateRight(x, 14) ^ rotateRight(x, 18) ^ rotateRight(x, 41); }
            ulong SmSigma0(ulong x) { return rotateRight(x, 1) ^ rotateRight(x, 8) ^ x >> 7; }
            ulong SmSigma1(ulong x) { return rotateRight(x, 19) ^ rotateRight(x, 61) ^ x >> 6; }
        }

        /*
         * SHA1 basic transformation. Transforms state based on block.
         */
        static void T_0_15(int i, const(ubyte[64])* input, ref uint[16] W, uint A, ref uint B, uint C, uint D,
            uint E, ref uint T) pure nothrow @nogc
        {
            uint Wi = W[i] = bigEndianToNative(*cast(ubyte[4]*)&((*input)[i*4]));
            T = Ch(B, C, D) + E + rotateLeft(A, 5) + Wi + 0x5a827999;
            B = rotateLeft(B, 30);
        }

        static void T_16_19(int i, ref uint[16] W, uint A, ref uint B, uint C, uint D, uint E, ref uint T)
            pure nothrow @nogc
        {
            W[i&15] = rotateLeft(W[(i-3)&15] ^ W[(i-8)&15] ^ W[(i-14)&15] ^ W[(i-16)&15], 1);
            T = Ch(B, C, D) + E + rotateLeft(A, 5) + W[i&15] + 0x5a827999;
            B = rotateLeft(B, 30);
        }

        static void T_20_39(int i, ref uint[16] W, uint A, ref uint B, uint C, uint D, uint E,
            ref uint T) pure nothrow @nogc
        {
            W[i&15] = rotateLeft(W[(i-3)&15] ^ W[(i-8)&15] ^ W[(i-14)&15] ^ W[(i-16)&15], 1);
            T = Parity(B, C, D) + E + rotateLeft(A, 5) + W[i&15] + 0x6ed9eba1;
            B = rotateLeft(B, 30);
        }

        static void T_40_59(int i, ref uint[16] W, uint A, ref uint B, uint C, uint D, uint E,
            ref uint T) pure nothrow @nogc
        {
            W[i&15] = rotateLeft(W[(i-3)&15] ^ W[(i-8)&15] ^ W[(i-14)&15] ^ W[(i-16)&15], 1);
            T = Maj(B, C, D) + E + rotateLeft(A, 5) + W[i&15] + 0x8f1bbcdc;
            B = rotateLeft(B, 30);
        }

        static void T_60_79(int i, ref uint[16] W, uint A, ref uint B, uint C, uint D, uint E,
            ref uint T) pure nothrow @nogc
        {
            W[i&15] = rotateLeft(W[(i-3)&15] ^ W[(i-8)&15] ^ W[(i-14)&15] ^ W[(i-16)&15], 1);
            T = Parity(B, C, D) + E + rotateLeft(A, 5) + W[i&15] + 0xca62c1d6;
            B = rotateLeft(B, 30);
        }

        private static void transformX86(uint[5]* state, const(ubyte[64])* block) pure nothrow @nogc
        {
            uint A, B, C, D, E, T;
            uint[16] W = void;

            A = (*state)[0];
            B = (*state)[1];
            C = (*state)[2];
            D = (*state)[3];
            E = (*state)[4];

            T_0_15 ( 0, block, W, A, B, C, D, E, T);
            T_0_15 ( 1, block, W, T, A, B, C, D, E);
            T_0_15 ( 2, block, W, E, T, A, B, C, D);
            T_0_15 ( 3, block, W, D, E, T, A, B, C);
            T_0_15 ( 4, block, W, C, D, E, T, A, B);
            T_0_15 ( 5, block, W, B, C, D, E, T, A);
            T_0_15 ( 6, block, W, A, B, C, D, E, T);
            T_0_15 ( 7, block, W, T, A, B, C, D, E);
            T_0_15 ( 8, block, W, E, T, A, B, C, D);
            T_0_15 ( 9, block, W, D, E, T, A, B, C);
            T_0_15 (10, block, W, C, D, E, T, A, B);
            T_0_15 (11, block, W, B, C, D, E, T, A);
            T_0_15 (12, block, W, A, B, C, D, E, T);
            T_0_15 (13, block, W, T, A, B, C, D, E);
            T_0_15 (14, block, W, E, T, A, B, C, D);
            T_0_15 (15, block, W, D, E, T, A, B, C);
            T_16_19(16, W, C, D, E, T, A, B);
            T_16_19(17, W, B, C, D, E, T, A);
            T_16_19(18, W, A, B, C, D, E, T);
            T_16_19(19, W, T, A, B, C, D, E);
            T_20_39(20, W, E, T, A, B, C, D);
            T_20_39(21, W, D, E, T, A, B, C);
            T_20_39(22, W, C, D, E, T, A, B);
            T_20_39(23, W, B, C, D, E, T, A);
            T_20_39(24, W, A, B, C, D, E, T);
            T_20_39(25, W, T, A, B, C, D, E);
            T_20_39(26, W, E, T, A, B, C, D);
            T_20_39(27, W, D, E, T, A, B, C);
            T_20_39(28, W, C, D, E, T, A, B);
            T_20_39(29, W, B, C, D, E, T, A);
            T_20_39(30, W, A, B, C, D, E, T);
            T_20_39(31, W, T, A, B, C, D, E);
            T_20_39(32, W, E, T, A, B, C, D);
            T_20_39(33, W, D, E, T, A, B, C);
            T_20_39(34, W, C, D, E, T, A, B);
            T_20_39(35, W, B, C, D, E, T, A);
            T_20_39(36, W, A, B, C, D, E, T);
            T_20_39(37, W, T, A, B, C, D, E);
            T_20_39(38, W, E, T, A, B, C, D);
            T_20_39(39, W, D, E, T, A, B, C);
            T_40_59(40, W, C, D, E, T, A, B);
            T_40_59(41, W, B, C, D, E, T, A);
            T_40_59(42, W, A, B, C, D, E, T);
            T_40_59(43, W, T, A, B, C, D, E);
            T_40_59(44, W, E, T, A, B, C, D);
            T_40_59(45, W, D, E, T, A, B, C);
            T_40_59(46, W, C, D, E, T, A, B);
            T_40_59(47, W, B, C, D, E, T, A);
            T_40_59(48, W, A, B, C, D, E, T);
            T_40_59(49, W, T, A, B, C, D, E);
            T_40_59(50, W, E, T, A, B, C, D);
            T_40_59(51, W, D, E, T, A, B, C);
            T_40_59(52, W, C, D, E, T, A, B);
            T_40_59(53, W, B, C, D, E, T, A);
            T_40_59(54, W, A, B, C, D, E, T);
            T_40_59(55, W, T, A, B, C, D, E);
            T_40_59(56, W, E, T, A, B, C, D);
            T_40_59(57, W, D, E, T, A, B, C);
            T_40_59(58, W, C, D, E, T, A, B);
            T_40_59(59, W, B, C, D, E, T, A);
            T_60_79(60, W, A, B, C, D, E, T);
            T_60_79(61, W, T, A, B, C, D, E);
            T_60_79(62, W, E, T, A, B, C, D);
            T_60_79(63, W, D, E, T, A, B, C);
            T_60_79(64, W, C, D, E, T, A, B);
            T_60_79(65, W, B, C, D, E, T, A);
            T_60_79(66, W, A, B, C, D, E, T);
            T_60_79(67, W, T, A, B, C, D, E);
            T_60_79(68, W, E, T, A, B, C, D);
            T_60_79(69, W, D, E, T, A, B, C);
            T_60_79(70, W, C, D, E, T, A, B);
            T_60_79(71, W, B, C, D, E, T, A);
            T_60_79(72, W, A, B, C, D, E, T);
            T_60_79(73, W, T, A, B, C, D, E);
            T_60_79(74, W, E, T, A, B, C, D);
            T_60_79(75, W, D, E, T, A, B, C);
            T_60_79(76, W, C, D, E, T, A, B);
            T_60_79(77, W, B, C, D, E, T, A);
            T_60_79(78, W, A, B, C, D, E, T);
            T_60_79(79, W, T, A, B, C, D, E);

            (*state)[0] += E;
            (*state)[1] += T;
            (*state)[2] += A;
            (*state)[3] += B;
            (*state)[4] += C;

            /* Zeroize sensitive information. */
            W[] = 0;
        }

        /*
         * SHA2 basic transformation. Transforms state based on block.
         */
        static void T_SHA2_0_15(Word)(int i, const(ubyte[blockSize/8])* input, ref Word[16] W,
            Word A, Word B, Word C, ref Word D, Word E, Word F, Word G, ref Word H, Word K)
            pure nothrow @nogc
        {
            Word Wi = W[i] = bigEndianToNative(*cast(ubyte[Word.sizeof]*)&((*input)[i*Word.sizeof]));
            Word T1 = H + BigSigma1(E) + Ch(E, F, G) + K + Wi;
            Word T2 = BigSigma0(A) + Maj(A, B, C);
            D += T1;
            H = T1 + T2;
        }

        static void T_SHA2_16_79(Word)(int i, ref Word[16] W,
            Word A, Word B, Word C, ref Word D, Word E, Word F, Word G, ref Word H, Word K)
            pure nothrow @nogc
        {
            W[i&15] = SmSigma1(W[(i-2)&15]) + W[(i-7)&15] + SmSigma0(W[(i-15)&15]) + W[i&15];
            Word T1 = H + BigSigma1(E) + Ch(E, F, G) + K + W[i&15];
            Word T2 = BigSigma0(A) + Maj(A, B, C);
            D += T1;
            H = T1 + T2;
        }

        private static void transformSHA2(Word)(Word[8]* state, const(ubyte[blockSize/8])* block)
            pure nothrow @nogc
        {
            Word A, B, C, D, E, F, G, H;
            Word[16] W = void;

            A = (*state)[0];
            B = (*state)[1];
            C = (*state)[2];
            D = (*state)[3];
            E = (*state)[4];
            F = (*state)[5];
            G = (*state)[6];
            H = (*state)[7];

            T_SHA2_0_15!Word ( 0, block, W, A, B, C, D, E, F, G, H, constants[ 0]);
            T_SHA2_0_15!Word ( 1, block, W, H, A, B, C, D, E, F, G, constants[ 1]);
            T_SHA2_0_15!Word ( 2, block, W, G, H, A, B, C, D, E, F, constants[ 2]);
            T_SHA2_0_15!Word ( 3, block, W, F, G, H, A, B, C, D, E, constants[ 3]);
            T_SHA2_0_15!Word ( 4, block, W, E, F, G, H, A, B, C, D, constants[ 4]);
            T_SHA2_0_15!Word ( 5, block, W, D, E, F, G, H, A, B, C, constants[ 5]);
            T_SHA2_0_15!Word ( 6, block, W, C, D, E, F, G, H, A, B, constants[ 6]);
            T_SHA2_0_15!Word ( 7, block, W, B, C, D, E, F, G, H, A, constants[ 7]);
            T_SHA2_0_15!Word ( 8, block, W, A, B, C, D, E, F, G, H, constants[ 8]);
            T_SHA2_0_15!Word ( 9, block, W, H, A, B, C, D, E, F, G, constants[ 9]);
            T_SHA2_0_15!Word (10, block, W, G, H, A, B, C, D, E, F, constants[10]);
            T_SHA2_0_15!Word (11, block, W, F, G, H, A, B, C, D, E, constants[11]);
            T_SHA2_0_15!Word (12, block, W, E, F, G, H, A, B, C, D, constants[12]);
            T_SHA2_0_15!Word (13, block, W, D, E, F, G, H, A, B, C, constants[13]);
            T_SHA2_0_15!Word (14, block, W, C, D, E, F, G, H, A, B, constants[14]);
            T_SHA2_0_15!Word (15, block, W, B, C, D, E, F, G, H, A, constants[15]);
            T_SHA2_16_79!Word(16, W, A, B, C, D, E, F, G, H, constants[16]);
            T_SHA2_16_79!Word(17, W, H, A, B, C, D, E, F, G, constants[17]);
            T_SHA2_16_79!Word(18, W, G, H, A, B, C, D, E, F, constants[18]);
            T_SHA2_16_79!Word(19, W, F, G, H, A, B, C, D, E, constants[19]);
            T_SHA2_16_79!Word(20, W, E, F, G, H, A, B, C, D, constants[20]);
            T_SHA2_16_79!Word(21, W, D, E, F, G, H, A, B, C, constants[21]);
            T_SHA2_16_79!Word(22, W, C, D, E, F, G, H, A, B, constants[22]);
            T_SHA2_16_79!Word(23, W, B, C, D, E, F, G, H, A, constants[23]);
            T_SHA2_16_79!Word(24, W, A, B, C, D, E, F, G, H, constants[24]);
            T_SHA2_16_79!Word(25, W, H, A, B, C, D, E, F, G, constants[25]);
            T_SHA2_16_79!Word(26, W, G, H, A, B, C, D, E, F, constants[26]);
            T_SHA2_16_79!Word(27, W, F, G, H, A, B, C, D, E, constants[27]);
            T_SHA2_16_79!Word(28, W, E, F, G, H, A, B, C, D, constants[28]);
            T_SHA2_16_79!Word(29, W, D, E, F, G, H, A, B, C, constants[29]);
            T_SHA2_16_79!Word(30, W, C, D, E, F, G, H, A, B, constants[30]);
            T_SHA2_16_79!Word(31, W, B, C, D, E, F, G, H, A, constants[31]);
            T_SHA2_16_79!Word(32, W, A, B, C, D, E, F, G, H, constants[32]);
            T_SHA2_16_79!Word(33, W, H, A, B, C, D, E, F, G, constants[33]);
            T_SHA2_16_79!Word(34, W, G, H, A, B, C, D, E, F, constants[34]);
            T_SHA2_16_79!Word(35, W, F, G, H, A, B, C, D, E, constants[35]);
            T_SHA2_16_79!Word(36, W, E, F, G, H, A, B, C, D, constants[36]);
            T_SHA2_16_79!Word(37, W, D, E, F, G, H, A, B, C, constants[37]);
            T_SHA2_16_79!Word(38, W, C, D, E, F, G, H, A, B, constants[38]);
            T_SHA2_16_79!Word(39, W, B, C, D, E, F, G, H, A, constants[39]);
            T_SHA2_16_79!Word(40, W, A, B, C, D, E, F, G, H, constants[40]);
            T_SHA2_16_79!Word(41, W, H, A, B, C, D, E, F, G, constants[41]);
            T_SHA2_16_79!Word(42, W, G, H, A, B, C, D, E, F, constants[42]);
            T_SHA2_16_79!Word(43, W, F, G, H, A, B, C, D, E, constants[43]);
            T_SHA2_16_79!Word(44, W, E, F, G, H, A, B, C, D, constants[44]);
            T_SHA2_16_79!Word(45, W, D, E, F, G, H, A, B, C, constants[45]);
            T_SHA2_16_79!Word(46, W, C, D, E, F, G, H, A, B, constants[46]);
            T_SHA2_16_79!Word(47, W, B, C, D, E, F, G, H, A, constants[47]);
            T_SHA2_16_79!Word(48, W, A, B, C, D, E, F, G, H, constants[48]);
            T_SHA2_16_79!Word(49, W, H, A, B, C, D, E, F, G, constants[49]);
            T_SHA2_16_79!Word(50, W, G, H, A, B, C, D, E, F, constants[50]);
            T_SHA2_16_79!Word(51, W, F, G, H, A, B, C, D, E, constants[51]);
            T_SHA2_16_79!Word(52, W, E, F, G, H, A, B, C, D, constants[52]);
            T_SHA2_16_79!Word(53, W, D, E, F, G, H, A, B, C, constants[53]);
            T_SHA2_16_79!Word(54, W, C, D, E, F, G, H, A, B, constants[54]);
            T_SHA2_16_79!Word(55, W, B, C, D, E, F, G, H, A, constants[55]);
            T_SHA2_16_79!Word(56, W, A, B, C, D, E, F, G, H, constants[56]);
            T_SHA2_16_79!Word(57, W, H, A, B, C, D, E, F, G, constants[57]);
            T_SHA2_16_79!Word(58, W, G, H, A, B, C, D, E, F, constants[58]);
            T_SHA2_16_79!Word(59, W, F, G, H, A, B, C, D, E, constants[59]);
            T_SHA2_16_79!Word(60, W, E, F, G, H, A, B, C, D, constants[60]);
            T_SHA2_16_79!Word(61, W, D, E, F, G, H, A, B, C, constants[61]);
            T_SHA2_16_79!Word(62, W, C, D, E, F, G, H, A, B, constants[62]);
            T_SHA2_16_79!Word(63, W, B, C, D, E, F, G, H, A, constants[63]);

            static if (is(Word == ulong))
            {
                T_SHA2_16_79!Word(64, W, A, B, C, D, E, F, G, H, constants[64]);
                T_SHA2_16_79!Word(65, W, H, A, B, C, D, E, F, G, constants[65]);
                T_SHA2_16_79!Word(66, W, G, H, A, B, C, D, E, F, constants[66]);
                T_SHA2_16_79!Word(67, W, F, G, H, A, B, C, D, E, constants[67]);
                T_SHA2_16_79!Word(68, W, E, F, G, H, A, B, C, D, constants[68]);
                T_SHA2_16_79!Word(69, W, D, E, F, G, H, A, B, C, constants[69]);
                T_SHA2_16_79!Word(70, W, C, D, E, F, G, H, A, B, constants[70]);
                T_SHA2_16_79!Word(71, W, B, C, D, E, F, G, H, A, constants[71]);
                T_SHA2_16_79!Word(72, W, A, B, C, D, E, F, G, H, constants[72]);
                T_SHA2_16_79!Word(73, W, H, A, B, C, D, E, F, G, constants[73]);
                T_SHA2_16_79!Word(74, W, G, H, A, B, C, D, E, F, constants[74]);
                T_SHA2_16_79!Word(75, W, F, G, H, A, B, C, D, E, constants[75]);
                T_SHA2_16_79!Word(76, W, E, F, G, H, A, B, C, D, constants[76]);
                T_SHA2_16_79!Word(77, W, D, E, F, G, H, A, B, C, constants[77]);
                T_SHA2_16_79!Word(78, W, C, D, E, F, G, H, A, B, constants[78]);
                T_SHA2_16_79!Word(79, W, B, C, D, E, F, G, H, A, constants[79]);
            }

            (*state)[0] += A;
            (*state)[1] += B;
            (*state)[2] += C;
            (*state)[3] += D;
            (*state)[4] += E;
            (*state)[5] += F;
            (*state)[6] += G;
            (*state)[7] += H;

            /* Zeroize sensitive information. */
            W[] = 0;
        }

    public:
        /**
         * SHA initialization. Begins an SHA1/SHA2 operation.
         *
         * Note:
         * For this SHA Digest implementation calling start after default construction
         * is not necessary. Calling start is only necessary to reset the Digest.
         *
         * Generic code which deals with different Digest types should always call start though.
         *
         * Example:
         * --------
         * SHA1 digest;
         * //digest.start(); //Not necessary
         * digest.put(0);
         * --------
         */
        void start() @safe pure nothrow @nogc
        {
            this = typeof(this).init;
        }

        /**
         * Use this to feed the digest with data.
         * Also implements the $(REF isOutputRange, std,range,primitives)
         * interface for $(D ubyte) and $(D const(ubyte)[]).
         */
        void put(scope const(ubyte)[] input...) @trusted pure nothrow @nogc
        {
            enum blockSizeInBytes = blockSize/8;
            uint i, index, partLen;
            auto inputLen = input.length;

            /* Compute number of bytes mod block size (64 or 128 bytes) */
            index = (cast(uint) count[0] >> 3) & (blockSizeInBytes - 1);

            /* Update number of bits */
            static if (blockSize == 512)
                count[0] += inputLen * 8;
            else static if (blockSize == 1024)
            {
                /* ugly hack to work around lack of ucent */
                auto oldCount0 = count[0];
                count[0] += inputLen * 8;
                if (count[0] < oldCount0)
                    count[1]++;
            }
            else
                static assert(0);

            partLen = blockSizeInBytes - index;

            /* Transform as many times as possible. */
            if (inputLen >= partLen)
            {
                (&buffer[index])[0 .. partLen] = input.ptr[0 .. partLen];
                transform (&state, &buffer);

                for (i = partLen; i + blockSizeInBytes-1 < inputLen; i += blockSizeInBytes)
                   transform(&state, cast(ubyte[blockSizeInBytes]*)(input.ptr + i));

                index = 0;
            }
            else
                i = 0;

            /* Buffer remaining input */
            if (inputLen - i)
                (&buffer[index])[0 .. inputLen-i] = (&input[i])[0 .. inputLen-i];
        }

        @safe unittest
        {
            typeof(this) dig;
            dig.put(cast(ubyte) 0); //single ubyte
            dig.put(cast(ubyte) 0, cast(ubyte) 0); //variadic
            ubyte[10] buf;
            dig.put(buf); //buffer
        }


        /**
         * Returns the finished SHA hash. This also calls $(LREF start) to
         * reset the internal state.
         */
        ubyte[digestSize/8] finish() @trusted pure nothrow @nogc
        {
            static if (blockSize == 512)
            {
                ubyte[32] data = void;
                uint index, padLen;

                /* Save number of bits */
                ubyte[8] bits = nativeToBigEndian(count[0]);

                /* Pad out to 56 mod 64. */
                index = (cast(uint) count[0] >> 3) & (64 - 1);
                padLen = (index < 56) ? (56 - index) : (120 - index);
                put(padding[0 .. padLen]);

                /* Append length (before padding) */
                put(bits);

                /* Store state in digest */
                for (auto i = 0; i < ((digestSize == 160)? 5 : 8); i++)
                    data[i*4..(i+1)*4] = nativeToBigEndian(state[i])[];

                /* Zeroize sensitive information. */
                start();
                return data[0 .. digestSize/8];
            }
            else static if (blockSize == 1024)
            {
                ubyte[64] data = void;
                uint index, padLen;

                /* Save number of bits */
                ubyte[16] bits;
                bits[ 0 .. 8] = nativeToBigEndian(count[1]);
                bits[8 .. 16] = nativeToBigEndian(count[0]);

                /* Pad out to 112 mod 128. */
                index = (cast(uint) count[0] >> 3) & (128 - 1);
                padLen = (index < 112) ? (112 - index) : (240 - index);
                put(padding[0 .. padLen]);

                /* Append length (before padding) */
                put(bits);

                /* Store state in digest */
                for (auto i = 0; i < 8; i++)
                    data[i*8..(i+1)*8] = nativeToBigEndian(state[i])[];

                /* Zeroize sensitive information. */
                start();
                return data[0 .. digestSize/8];
            }
            else
                static assert(0);
        }
        ///
        @safe unittest
        {
            //Simple example
            SHA1 hash;
            hash.start();
            hash.put(cast(ubyte) 0);
            ubyte[20] result = hash.finish();
        }
}

alias SHA1 = SHA!(512, 160);  /// SHA alias for SHA-1, hash is ubyte[20]
alias SHA224 = SHA!(512, 224);  /// SHA alias for SHA-224, hash is ubyte[28]
alias SHA256 = SHA!(512, 256);  /// SHA alias for SHA-256, hash is ubyte[32]
alias SHA384 = SHA!(1024, 384); /// SHA alias for SHA-384, hash is ubyte[48]
alias SHA512 = SHA!(1024, 512); /// SHA alias for SHA-512, hash is ubyte[64]
alias SHA512_224 = SHA!(1024, 224); /// SHA alias for SHA-512/224, hash is ubyte[28]
alias SHA512_256 = SHA!(1024, 256); /// SHA alias for SHA-512/256, hash is ubyte[32]

///
@safe unittest
{
    //Simple example, hashing a string using sha1Of helper function
    ubyte[20] hash = sha1Of("abc");
    //Let's get a hash string
    assert(toHexString(hash) == "A9993E364706816ABA3E25717850C26C9CD0D89D");

    //The same, but using SHA-224
    ubyte[28] hash224 = sha224Of("abc");
    assert(toHexString(hash224) == "23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7");
}

///
@safe unittest
{
    //Using the basic API
    SHA1 hash;
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
    assert(isDigest!SHA1);
    assert(isDigest!SHA224);
    assert(isDigest!SHA256);
    assert(isDigest!SHA384);
    assert(isDigest!SHA512);
    assert(isDigest!SHA512_224);
    assert(isDigest!SHA512_256);
}

@system unittest
{
    import std.conv : hexString;
    import std.range;

    ubyte[20] digest;
    ubyte[28] digest224;
    ubyte[32] digest256;
    ubyte[48] digest384;
    ubyte[64] digest512;
    ubyte[28] digest512_224;
    ubyte[32] digest512_256;

    SHA1 sha;
    sha.put(cast(ubyte[])"abcdef");
    sha.start();
    sha.put(cast(ubyte[])"");
    assert(sha.finish() == cast(ubyte[]) x"da39a3ee5e6b4b0d3255bfef95601890afd80709");

    SHA224 sha224;
    sha224.put(cast(ubyte[])"abcdef");
    sha224.start();
    sha224.put(cast(ubyte[])"");
    assert(sha224.finish() == cast(ubyte[]) x"d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f");

    SHA256 sha256;
    sha256.put(cast(ubyte[])"abcdef");
    sha256.start();
    sha256.put(cast(ubyte[])"");
    assert(sha256.finish() == cast(ubyte[]) x"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");

    SHA384 sha384;
    sha384.put(cast(ubyte[])"abcdef");
    sha384.start();
    sha384.put(cast(ubyte[])"");
    assert(sha384.finish() == cast(ubyte[]) hexString!("38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c"
        ~"0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"));

    SHA512 sha512;
    sha512.put(cast(ubyte[])"abcdef");
    sha512.start();
    sha512.put(cast(ubyte[])"");
    assert(sha512.finish() == cast(ubyte[]) hexString!("cf83e1357eefb8bdf1542850d66d8007d620e4050b571"
        ~"5dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"));

    SHA512_224 sha512_224;
    sha512_224.put(cast(ubyte[])"abcdef");
    sha512_224.start();
    sha512_224.put(cast(ubyte[])"");
    assert(sha512_224.finish() == cast(ubyte[]) x"6ed0dd02806fa89e25de060c19d3ac86cabb87d6a0ddd05c333b84f4");

    SHA512_256 sha512_256;
    sha512_256.put(cast(ubyte[])"abcdef");
    sha512_256.start();
    sha512_256.put(cast(ubyte[])"");
    assert(sha512_256.finish() == cast(ubyte[]) x"c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a");

    digest        = sha1Of      ("");
    digest224     = sha224Of    ("");
    digest256     = sha256Of    ("");
    digest384     = sha384Of    ("");
    digest512     = sha512Of    ("");
    digest512_224 = sha512_224Of("");
    digest512_256 = sha512_256Of("");
    assert(digest == cast(ubyte[]) x"da39a3ee5e6b4b0d3255bfef95601890afd80709");
    assert(digest224 == cast(ubyte[]) x"d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f");
    assert(digest256 == cast(ubyte[]) x"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    assert(digest384 == cast(ubyte[]) hexString!("38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c"
        ~"0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"));
    assert(digest512 == cast(ubyte[]) hexString!("cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83"
        ~"f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"));
    assert(digest512_224 == cast(ubyte[]) x"6ed0dd02806fa89e25de060c19d3ac86cabb87d6a0ddd05c333b84f4");
    assert(digest512_256 == cast(ubyte[]) x"c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a");

    digest        = sha1Of      ("a");
    digest224     = sha224Of    ("a");
    digest256     = sha256Of    ("a");
    digest384     = sha384Of    ("a");
    digest512     = sha512Of    ("a");
    digest512_224 = sha512_224Of("a");
    digest512_256 = sha512_256Of("a");
    assert(digest == cast(ubyte[]) x"86f7e437faa5a7fce15d1ddcb9eaeaea377667b8");
    assert(digest224 == cast(ubyte[]) x"abd37534c7d9a2efb9465de931cd7055ffdb8879563ae98078d6d6d5");
    assert(digest256 == cast(ubyte[]) x"ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb");
    assert(digest384 == cast(ubyte[]) hexString!("54a59b9f22b0b80880d8427e548b7c23abd873486e1f035dce9"
        ~"cd697e85175033caa88e6d57bc35efae0b5afd3145f31"));
    assert(digest512 == cast(ubyte[]) hexString!("1f40fc92da241694750979ee6cf582f2d5d7d28e18335de05ab"
        ~"c54d0560e0f5302860c652bf08d560252aa5e74210546f369fbbbce8c12cfc7957b2652fe9a75"));
    assert(digest512_224 == cast(ubyte[]) x"d5cdb9ccc769a5121d4175f2bfdd13d6310e0d3d361ea75d82108327");
    assert(digest512_256 == cast(ubyte[]) x"455e518824bc0601f9fb858ff5c37d417d67c2f8e0df2babe4808858aea830f8");

    digest        = sha1Of      ("abc");
    digest224     = sha224Of    ("abc");
    digest256     = sha256Of    ("abc");
    digest384     = sha384Of    ("abc");
    digest512     = sha512Of    ("abc");
    digest512_224 = sha512_224Of("abc");
    digest512_256 = sha512_256Of("abc");
    assert(digest == cast(ubyte[]) x"a9993e364706816aba3e25717850c26c9cd0d89d");
    assert(digest224 == cast(ubyte[]) x"23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7");
    assert(digest256 == cast(ubyte[]) x"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    assert(digest384 == cast(ubyte[]) hexString!("cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a"
        ~"8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"));
    assert(digest512 == cast(ubyte[]) hexString!("ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9"
        ~"eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"));
    assert(digest512_224 == cast(ubyte[]) x"4634270f707b6a54daae7530460842e20e37ed265ceee9a43e8924aa");
    assert(digest512_256 == cast(ubyte[]) x"53048e2681941ef99b2e29b76b4c7dabe4c2d0c634fc6d46e0e2f13107e7af23");

    digest        = sha1Of      ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    digest224     = sha224Of    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    digest256     = sha256Of    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    digest384     = sha384Of    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    digest512     = sha512Of    ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    digest512_224 = sha512_224Of("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    digest512_256 = sha512_256Of("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
    assert(digest == cast(ubyte[]) x"84983e441c3bd26ebaae4aa1f95129e5e54670f1");
    assert(digest224 == cast(ubyte[]) x"75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525");
    assert(digest256 == cast(ubyte[]) x"248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1");
    assert(digest384 == cast(ubyte[]) hexString!("3391fdddfc8dc7393707a65b1b4709397cf8b1d162af05abfe"
        ~"8f450de5f36bc6b0455a8520bc4e6f5fe95b1fe3c8452b"));
    assert(digest512 == cast(ubyte[]) hexString!("204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a827"
        ~"9be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"));
    assert(digest512_224 == cast(ubyte[]) x"e5302d6d54bb242275d1e7622d68df6eb02dedd13f564c13dbda2174");
    assert(digest512_256 == cast(ubyte[]) x"bde8e1f9f19bb9fd3406c90ec6bc47bd36d8ada9f11880dbc8a22a7078b6a461");

    digest        = sha1Of      ("message digest");
    digest224     = sha224Of    ("message digest");
    digest256     = sha256Of    ("message digest");
    digest384     = sha384Of    ("message digest");
    digest512     = sha512Of    ("message digest");
    digest512_224 = sha512_224Of("message digest");
    digest512_256 = sha512_256Of("message digest");
    assert(digest == cast(ubyte[]) x"c12252ceda8be8994d5fa0290a47231c1d16aae3");
    assert(digest224 == cast(ubyte[]) x"2cb21c83ae2f004de7e81c3c7019cbcb65b71ab656b22d6d0c39b8eb");
    assert(digest256 == cast(ubyte[]) x"f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650");
    assert(digest384 == cast(ubyte[]) hexString!("473ed35167ec1f5d8e550368a3db39be54639f828868e9454c"
        ~"239fc8b52e3c61dbd0d8b4de1390c256dcbb5d5fd99cd5"));
    assert(digest512 == cast(ubyte[]) hexString!("107dbf389d9e9f71a3a95f6c055b9251bc5268c2be16d6c134"
        ~"92ea45b0199f3309e16455ab1e96118e8a905d5597b72038ddb372a89826046de66687bb420e7c"));
    assert(digest512_224 == cast(ubyte[]) x"ad1a4db188fe57064f4f24609d2a83cd0afb9b398eb2fcaeaae2c564");
    assert(digest512_256 == cast(ubyte[]) x"0cf471fd17ed69d990daf3433c89b16d63dec1bb9cb42a6094604ee5d7b4e9fb");

    digest        = sha1Of      ("abcdefghijklmnopqrstuvwxyz");
    digest224     = sha224Of    ("abcdefghijklmnopqrstuvwxyz");
    digest256     = sha256Of    ("abcdefghijklmnopqrstuvwxyz");
    digest384     = sha384Of    ("abcdefghijklmnopqrstuvwxyz");
    digest512     = sha512Of    ("abcdefghijklmnopqrstuvwxyz");
    digest512_224 = sha512_224Of("abcdefghijklmnopqrstuvwxyz");
    digest512_256 = sha512_256Of("abcdefghijklmnopqrstuvwxyz");
    assert(digest == cast(ubyte[]) x"32d10c7b8cf96570ca04ce37f2a19d84240d3a89");
    assert(digest224 == cast(ubyte[]) x"45a5f72c39c5cff2522eb3429799e49e5f44b356ef926bcf390dccc2");
    assert(digest256 == cast(ubyte[]) x"71c480df93d6ae2f1efad1447c66c9525e316218cf51fc8d9ed832f2daf18b73");
    assert(digest384 == cast(ubyte[]) hexString!("feb67349df3db6f5924815d6c3dc133f091809213731fe5c7b5"
        ~"f4999e463479ff2877f5f2936fa63bb43784b12f3ebb4"));
    assert(digest512 == cast(ubyte[]) hexString!("4dbff86cc2ca1bae1e16468a05cb9881c97f1753bce3619034"
        ~"898faa1aabe429955a1bf8ec483d7421fe3c1646613a59ed5441fb0f321389f77f48a879c7b1f1"));
    assert(digest512_224 == cast(ubyte[]) x"ff83148aa07ec30655c1b40aff86141c0215fe2a54f767d3f38743d8");
    assert(digest512_256 == cast(ubyte[]) x"fc3189443f9c268f626aea08a756abe7b726b05f701cb08222312ccfd6710a26");

    digest        = sha1Of      ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    digest224     = sha224Of    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    digest256     = sha256Of    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    digest384     = sha384Of    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    digest512     = sha512Of    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    digest512_224 = sha512_224Of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    digest512_256 = sha512_256Of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
    assert(digest == cast(ubyte[]) x"761c457bf73b14d27e9e9265c46f4b4dda11f940");
    assert(digest224 == cast(ubyte[]) x"bff72b4fcb7d75e5632900ac5f90d219e05e97a7bde72e740db393d9");
    assert(digest256 == cast(ubyte[]) x"db4bfcbd4da0cd85a60c3c37d3fbd8805c77f15fc6b1fdfe614ee0a7c8fdb4c0");
    assert(digest384 == cast(ubyte[]) hexString!("1761336e3f7cbfe51deb137f026f89e01a448e3b1fafa64039"
        ~"c1464ee8732f11a5341a6f41e0c202294736ed64db1a84"));
    assert(digest512 == cast(ubyte[]) hexString!("1e07be23c26a86ea37ea810c8ec7809352515a970e9253c26f"
        ~"536cfc7a9996c45c8370583e0a78fa4a90041d71a4ceab7423f19c71b9d5a3e01249f0bebd5894"));
    assert(digest512_224 == cast(ubyte[]) x"a8b4b9174b99ffc67d6f49be9981587b96441051e16e6dd036b140d3");
    assert(digest512_256 == cast(ubyte[]) x"cdf1cc0effe26ecc0c13758f7b4a48e000615df241284185c39eb05d355bb9c8");

    digest        = sha1Of      ("1234567890123456789012345678901234567890"~
                                 "1234567890123456789012345678901234567890");
    digest224     = sha224Of    ("1234567890123456789012345678901234567890"~
                                 "1234567890123456789012345678901234567890");
    digest256     = sha256Of    ("1234567890123456789012345678901234567890"~
                                 "1234567890123456789012345678901234567890");
    digest384     = sha384Of    ("1234567890123456789012345678901234567890"~
                                 "1234567890123456789012345678901234567890");
    digest512     = sha512Of    ("1234567890123456789012345678901234567890"~
                                 "1234567890123456789012345678901234567890");
    digest512_224 = sha512_224Of("1234567890123456789012345678901234567890"~
                                 "1234567890123456789012345678901234567890");
    digest512_256 = sha512_256Of("1234567890123456789012345678901234567890"~
                                 "1234567890123456789012345678901234567890");
    assert(digest == cast(ubyte[]) x"50abf5706a150990a08b2c5ea40fa0e585554732");
    assert(digest224 == cast(ubyte[]) x"b50aecbe4e9bb0b57bc5f3ae760a8e01db24f203fb3cdcd13148046e");
    assert(digest256 == cast(ubyte[]) x"f371bc4a311f2b009eef952dd83ca80e2b60026c8e935592d0f9c308453c813e");
    assert(digest384 == cast(ubyte[]) hexString!("b12932b0627d1c060942f5447764155655bd4da0c9afa6dd9b"
        ~"9ef53129af1b8fb0195996d2de9ca0df9d821ffee67026"));
    assert(digest512 == cast(ubyte[]) hexString!("72ec1ef1124a45b047e8b7c75a932195135bb61de24ec0d191"
        ~"4042246e0aec3a2354e093d76f3048b456764346900cb130d2a4fd5dd16abb5e30bcb850dee843"));
    assert(digest512_224 == cast(ubyte[]) x"ae988faaa47e401a45f704d1272d99702458fea2ddc6582827556dd2");
    assert(digest512_256 == cast(ubyte[]) x"2c9fdbc0c90bdd87612ee8455474f9044850241dc105b1e8b94b8ddf5fac9148");

    ubyte[] onemilliona = new ubyte[1000000];
    onemilliona[] = 'a';
    digest = sha1Of(onemilliona);
    digest224 = sha224Of(onemilliona);
    digest256 = sha256Of(onemilliona);
    digest384 = sha384Of(onemilliona);
    digest512 = sha512Of(onemilliona);
    digest512_224 = sha512_224Of(onemilliona);
    digest512_256 = sha512_256Of(onemilliona);
    assert(digest == cast(ubyte[]) x"34aa973cd4c4daa4f61eeb2bdbad27316534016f");
    assert(digest224 == cast(ubyte[]) x"20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67");
    assert(digest256 == cast(ubyte[]) x"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0");
    assert(digest384 == cast(ubyte[]) hexString!("9d0e1809716474cb086e834e310a4a1ced149e9c00f2485279"
        ~"72cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985"));
    assert(digest512 == cast(ubyte[]) hexString!("e718483d0ce769644e2e42c7bc15b4638e1f98b13b20442856"
        ~"32a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"));
    assert(digest512_224 == cast(ubyte[]) x"37ab331d76f0d36de422bd0edeb22a28accd487b7a8453ae965dd287");
    assert(digest512_256 == cast(ubyte[]) x"9a59a052930187a97038cae692f30708aa6491923ef5194394dc68d56c74fb21");

    auto oneMillionRange = repeat!ubyte(cast(ubyte)'a', 1000000);
    digest = sha1Of(oneMillionRange);
    digest224 = sha224Of(oneMillionRange);
    digest256 = sha256Of(oneMillionRange);
    digest384 = sha384Of(oneMillionRange);
    digest512 = sha512Of(oneMillionRange);
    digest512_224 = sha512_224Of(oneMillionRange);
    digest512_256 = sha512_256Of(oneMillionRange);
    assert(digest == cast(ubyte[]) x"34aa973cd4c4daa4f61eeb2bdbad27316534016f");
    assert(digest224 == cast(ubyte[]) x"20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67");
    assert(digest256 == cast(ubyte[]) x"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0");
    assert(digest384 == cast(ubyte[]) hexString!("9d0e1809716474cb086e834e310a4a1ced149e9c00f2485279"
        ~"72cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985"));
    assert(digest512 == cast(ubyte[]) hexString!("e718483d0ce769644e2e42c7bc15b4638e1f98b13b20442856"
        ~"32a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"));
    assert(digest512_224 == cast(ubyte[]) x"37ab331d76f0d36de422bd0edeb22a28accd487b7a8453ae965dd287");
    assert(digest512_256 == cast(ubyte[]) x"9a59a052930187a97038cae692f30708aa6491923ef5194394dc68d56c74fb21");

    assert(toHexString(cast(ubyte[20]) x"a9993e364706816aba3e25717850c26c9cd0d89d")
        == "A9993E364706816ABA3E25717850C26C9CD0D89D");
}

/**
 * These are convenience aliases for $(REF digest, std,digest) using the
 * SHA implementation.
 */
//simple alias doesn't work here, hope this gets inlined...
auto sha1Of(T...)(T data)
{
    return digest!(SHA1, T)(data);
}
///ditto
auto sha224Of(T...)(T data)
{
    return digest!(SHA224, T)(data);
}
///ditto
auto sha256Of(T...)(T data)
{
    return digest!(SHA256, T)(data);
}
///ditto
auto sha384Of(T...)(T data)
{
    return digest!(SHA384, T)(data);
}
///ditto
auto sha512Of(T...)(T data)
{
    return digest!(SHA512, T)(data);
}
///ditto
auto sha512_224Of(T...)(T data)
{
    return digest!(SHA512_224, T)(data);
}
///ditto
auto sha512_256Of(T...)(T data)
{
    return digest!(SHA512_256, T)(data);
}

///
@safe unittest
{
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
    string a = "Mary has ", b = "a little lamb";
    int[] c = [ 1, 2, 3, 4, 5 ];
    string d = toHexString(sha1Of(a, b, c));
    version (LittleEndian)
        assert(d == "CDBB611D00AC2387B642D3D7BDF4C3B342237110", d);
    else
        assert(d == "A0F1196C7A379C09390476D9CA4AA11B71FD11C8", d);
}

/**
 * OOP API SHA1 and SHA2 implementations.
 * See $(D std.digest) for differences between template and OOP API.
 *
 * This is an alias for $(D $(REF WrapperDigest, std,digest)!SHA1), see
 * there for more information.
 */
alias SHA1Digest = WrapperDigest!SHA1;
alias SHA224Digest = WrapperDigest!SHA224; ///ditto
alias SHA256Digest = WrapperDigest!SHA256; ///ditto
alias SHA384Digest = WrapperDigest!SHA384; ///ditto
alias SHA512Digest = WrapperDigest!SHA512; ///ditto
alias SHA512_224Digest = WrapperDigest!SHA512_224; ///ditto
alias SHA512_256Digest = WrapperDigest!SHA512_256; ///ditto

///
@safe unittest
{
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

///
@system unittest
{
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

@system unittest
{
    auto sha = new SHA1Digest();

    sha.put(cast(ubyte[])"abcdef");
    sha.reset();
    sha.put(cast(ubyte[])"");
    assert(sha.finish() == cast(ubyte[]) x"da39a3ee5e6b4b0d3255bfef95601890afd80709");

    sha.put(cast(ubyte[])"abcdefghijklmnopqrstuvwxyz");
    ubyte[22] result;
    auto result2 = sha.finish(result[]);
    assert(result[0 .. 20] == result2 && result2 == cast(ubyte[]) x"32d10c7b8cf96570ca04ce37f2a19d84240d3a89");

    debug
        assertThrown!Error(sha.finish(result[0 .. 15]));

    assert(sha.length == 20);

    assert(sha.digest("") == cast(ubyte[]) x"da39a3ee5e6b4b0d3255bfef95601890afd80709");

    assert(sha.digest("a") == cast(ubyte[]) x"86f7e437faa5a7fce15d1ddcb9eaeaea377667b8");

    assert(sha.digest("abc") == cast(ubyte[]) x"a9993e364706816aba3e25717850c26c9cd0d89d");

    assert(sha.digest("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
           == cast(ubyte[]) x"84983e441c3bd26ebaae4aa1f95129e5e54670f1");

    assert(sha.digest("message digest") == cast(ubyte[]) x"c12252ceda8be8994d5fa0290a47231c1d16aae3");

    assert(sha.digest("abcdefghijklmnopqrstuvwxyz")
           == cast(ubyte[]) x"32d10c7b8cf96570ca04ce37f2a19d84240d3a89");

    assert(sha.digest("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
           == cast(ubyte[]) x"761c457bf73b14d27e9e9265c46f4b4dda11f940");

    assert(sha.digest("1234567890123456789012345678901234567890",
                                   "1234567890123456789012345678901234567890")
           == cast(ubyte[]) x"50abf5706a150990a08b2c5ea40fa0e585554732");

    ubyte[] onemilliona = new ubyte[1000000];
    onemilliona[] = 'a';
    assert(sha.digest(onemilliona) == cast(ubyte[]) x"34aa973cd4c4daa4f61eeb2bdbad27316534016f");
}

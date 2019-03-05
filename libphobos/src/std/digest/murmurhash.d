/**
Computes $(LINK2 https://en.wikipedia.org/wiki/MurmurHash, MurmurHash) hashes
of arbitrary data. MurmurHash is a non-cryptographic hash function suitable
for general hash-based lookup. It is optimized for x86 but can be used on
all architectures.

The current version is MurmurHash3, which yields a 32-bit or 128-bit hash value.
The older MurmurHash 1 and 2 are currently not supported.

MurmurHash3 comes in three flavors, listed in increasing order of throughput:
$(UL
$(LI `MurmurHash3!32` produces a 32-bit value and is optimized for 32-bit architectures)
$(LI $(D MurmurHash3!(128, 32)) produces a 128-bit value and is optimized for 32-bit architectures)
$(LI $(D MurmurHash3!(128, 64)) produces a 128-bit value and is optimized for 64-bit architectures)
)

Note:
$(UL
$(LI $(D MurmurHash3!(128, 32)) and $(D MurmurHash3!(128, 64)) produce different values.)
$(LI The current implementation is optimized for little endian architectures.
  It will exhibit different results on big endian architectures and a slightly
  less uniform distribution.)
)

This module conforms to the APIs defined in $(MREF std, digest).

This module publicly imports $(MREF std, digest) and can be used as a stand-alone module.

Source: $(PHOBOSSRC std/digest/murmurhash.d)
License: $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Guillaume Chatelet
References: $(LINK2 https://github.com/aappleby/smhasher, Reference implementation)
$(BR) $(LINK2 https://en.wikipedia.org/wiki/MurmurHash, Wikipedia)
*/
/* Copyright Guillaume Chatelet 2016.
 * Distributed under the Boost Software License, Version 1.0.
 * (See LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
module std.digest.murmurhash;

version (X86)
    version = HaveUnalignedLoads;
else version (X86_64)
    version = HaveUnalignedLoads;

///
@safe unittest
{
    // MurmurHash3!32, MurmurHash3!(128, 32) and MurmurHash3!(128, 64) implement
    // the std.digest Template API.
    static assert(isDigest!(MurmurHash3!32));
    // The convenient digest template allows for quick hashing of any data.
    ubyte[4] hashed = digest!(MurmurHash3!32)([1, 2, 3, 4]);
    assert(hashed == [0, 173, 69, 68]);
}

///
@safe unittest
{
    // One can also hash ubyte data piecewise by instanciating a hasher and call
    // the 'put' method.
    const(ubyte)[] data1 = [1, 2, 3];
    const(ubyte)[] data2 = [4, 5, 6, 7];
    // The incoming data will be buffered and hashed element by element.
    MurmurHash3!32 hasher;
    hasher.put(data1);
    hasher.put(data2);
    // The call to 'finish' ensures:
    // - the remaining bits are processed
    // - the hash gets finalized
    auto hashed = hasher.finish();
    assert(hashed == [181, 151, 88, 252]);
}

///
@safe unittest
{
    // Using `putElements`, `putRemainder` and `finalize` you gain full
    // control over which part of the algorithm to run.
    // This allows for maximum throughput but needs extra care.

    // Data type must be the same as the hasher's element type:
    // - uint for MurmurHash3!32
    // - uint[4] for MurmurHash3!(128, 32)
    // - ulong[2] for MurmurHash3!(128, 64)
    const(uint)[] data = [1, 2, 3, 4];
    // Note the hasher starts with 'Fast'.
    MurmurHash3!32 hasher;
    // Push as many array of elements as you need. The less calls the better.
    hasher.putElements(data);
    // Put remainder bytes if needed. This method can be called only once.
    hasher.putRemainder(ubyte(1), ubyte(1), ubyte(1));
    // Call finalize to incorporate data length in the hash.
    hasher.finalize();
    // Finally get the hashed value.
    auto hashed = hasher.getBytes();
    assert(hashed == [188, 165, 108, 2]);
}

public import std.digest;

@safe:

/*
Performance notes:
 - To help a bit with the performance when compiling with DMD some
   functions have been rewritten to pass by value instead of by reference.
 - GDC and LDC are on par with their C++ counterpart.
 - DMD is typically between 20% to 50% of the GCC version.
*/

/++
 + Implements the MurmurHash3 functions. You can specify the `size` of the
 + hash in bit. For 128 bit hashes you can specify whether to optimize for 32
 + or 64 bit architectures. If you don't specify the `opt` value it will select
 + the fastest version of the host platform.
 +
 + This hasher is compatible with the `Digest` API:
 + $(UL
 + $(LI `void start()`)
 + $(LI `void put(scope const(ubyte)[] data...)`)
 + $(LI `ubyte[Element.sizeof] finish()`)
 + )
 +
 + It also provides a faster, low level API working with data of size
 + `Element.sizeof`:
 + $(UL
 + $(LI `void putElements(scope const(Element[]) elements...)`)
 + $(LI `void putRemainder(scope const(ubyte[]) data...)`)
 + $(LI `void finalize()`)
 + $(LI `Element get()`)
 + $(LI `ubyte[Element.sizeof] getBytes()`)
 + )
 +/
struct MurmurHash3(uint size /* 32 or 128 */ , uint opt = size_t.sizeof == 8 ? 64 : 32)
{
    enum blockSize = size; // Number of bits of the hashed value.
    size_t element_count; // The number of full elements pushed, this is used for finalization.

    static if (size == 32)
    {
        private enum uint c1 = 0xcc9e2d51;
        private enum uint c2 = 0x1b873593;
        private uint h1;
        alias Element = uint; /// The element type for 32-bit implementation.

        this(uint seed)
        {
            h1 = seed;
        }
        /++
        Adds a single Element of data without increasing `element_count`.
        Make sure to increase `element_count` by `Element.sizeof` for each call to `putElement`.
        +/
        void putElement(uint block) pure nothrow @nogc
        {
            h1 = update(h1, block, 0, c1, c2, 15, 13, 0xe6546b64U);
        }

        /// Put remainder bytes. This must be called only once after `putElement` and before `finalize`.
        void putRemainder(scope const(ubyte[]) data...) pure nothrow @nogc
        {
            assert(data.length < Element.sizeof);
            assert(data.length >= 0);
            element_count += data.length;
            uint k1 = 0;
            final switch (data.length & 3)
            {
            case 3:
                k1 ^= data[2] << 16;
                goto case;
            case 2:
                k1 ^= data[1] << 8;
                goto case;
            case 1:
                k1 ^= data[0];
                h1 ^= shuffle(k1, c1, c2, 15);
                goto case;
            case 0:
            }
        }

        /// Incorporate `element_count` and finalizes the hash.
        void finalize() pure nothrow @nogc
        {
            h1 ^= element_count;
            h1 = fmix(h1);
        }

        /// Returns the hash as an uint value.
        Element get() pure nothrow @nogc
        {
            return h1;
        }

        /// Returns the current hashed value as an ubyte array.
        ubyte[4] getBytes() pure nothrow @nogc
        {
            return cast(typeof(return)) cast(uint[1])[get()];
        }
    }
    else static if (size == 128 && opt == 32)
    {
        private enum uint c1 = 0x239b961b;
        private enum uint c2 = 0xab0e9789;
        private enum uint c3 = 0x38b34ae5;
        private enum uint c4 = 0xa1e38b93;
        private uint h4, h3, h2, h1;

        alias Element = uint[4]; /// The element type for 128-bit implementation.

        this(uint seed4, uint seed3, uint seed2, uint seed1) pure nothrow @nogc
        {
            h4 = seed4;
            h3 = seed3;
            h2 = seed2;
            h1 = seed1;
        }

        this(uint seed) pure nothrow @nogc
        {
            h4 = h3 = h2 = h1 = seed;
        }

        /++
        Adds a single Element of data without increasing element_count.
        Make sure to increase `element_count` by `Element.sizeof` for each call to `putElement`.
        +/
        void putElement(Element block) pure nothrow @nogc
        {
            h1 = update(h1, block[0], h2, c1, c2, 15, 19, 0x561ccd1bU);
            h2 = update(h2, block[1], h3, c2, c3, 16, 17, 0x0bcaa747U);
            h3 = update(h3, block[2], h4, c3, c4, 17, 15, 0x96cd1c35U);
            h4 = update(h4, block[3], h1, c4, c1, 18, 13, 0x32ac3b17U);
        }

        /// Put remainder bytes. This must be called only once after `putElement` and before `finalize`.
        void putRemainder(scope const(ubyte[]) data...) pure nothrow @nogc
        {
            assert(data.length < Element.sizeof);
            assert(data.length >= 0);
            element_count += data.length;
            uint k1 = 0;
            uint k2 = 0;
            uint k3 = 0;
            uint k4 = 0;

            final switch (data.length & 15)
            {
            case 15:
                k4 ^= data[14] << 16;
                goto case;
            case 14:
                k4 ^= data[13] << 8;
                goto case;
            case 13:
                k4 ^= data[12] << 0;
                h4 ^= shuffle(k4, c4, c1, 18);
                goto case;
            case 12:
                k3 ^= data[11] << 24;
                goto case;
            case 11:
                k3 ^= data[10] << 16;
                goto case;
            case 10:
                k3 ^= data[9] << 8;
                goto case;
            case 9:
                k3 ^= data[8] << 0;
                h3 ^= shuffle(k3, c3, c4, 17);
                goto case;
            case 8:
                k2 ^= data[7] << 24;
                goto case;
            case 7:
                k2 ^= data[6] << 16;
                goto case;
            case 6:
                k2 ^= data[5] << 8;
                goto case;
            case 5:
                k2 ^= data[4] << 0;
                h2 ^= shuffle(k2, c2, c3, 16);
                goto case;
            case 4:
                k1 ^= data[3] << 24;
                goto case;
            case 3:
                k1 ^= data[2] << 16;
                goto case;
            case 2:
                k1 ^= data[1] << 8;
                goto case;
            case 1:
                k1 ^= data[0] << 0;
                h1 ^= shuffle(k1, c1, c2, 15);
                goto case;
            case 0:
            }
        }

        /// Incorporate `element_count` and finalizes the hash.
        void finalize() pure nothrow @nogc
        {
            h1 ^= element_count;
            h2 ^= element_count;
            h3 ^= element_count;
            h4 ^= element_count;

            h1 += h2;
            h1 += h3;
            h1 += h4;
            h2 += h1;
            h3 += h1;
            h4 += h1;

            h1 = fmix(h1);
            h2 = fmix(h2);
            h3 = fmix(h3);
            h4 = fmix(h4);

            h1 += h2;
            h1 += h3;
            h1 += h4;
            h2 += h1;
            h3 += h1;
            h4 += h1;
        }

        /// Returns the hash as an uint[4] value.
        Element get() pure nothrow @nogc
        {
            return [h1, h2, h3, h4];
        }

        /// Returns the current hashed value as an ubyte array.
        ubyte[16] getBytes() pure nothrow @nogc
        {
            return cast(typeof(return)) get();
        }
    }
    else static if (size == 128 && opt == 64)
    {
        private enum ulong c1 = 0x87c37b91114253d5;
        private enum ulong c2 = 0x4cf5ad432745937f;
        private ulong h2, h1;

        alias Element = ulong[2]; /// The element type for 128-bit implementation.

        this(ulong seed) pure nothrow @nogc
        {
            h2 = h1 = seed;
        }

        this(ulong seed2, ulong seed1) pure nothrow @nogc
        {
            h2 = seed2;
            h1 = seed1;
        }

        /++
        Adds a single Element of data without increasing `element_count`.
        Make sure to increase `element_count` by `Element.sizeof` for each call to `putElement`.
        +/
        void putElement(Element block) pure nothrow @nogc
        {
            h1 = update(h1, block[0], h2, c1, c2, 31, 27, 0x52dce729U);
            h2 = update(h2, block[1], h1, c2, c1, 33, 31, 0x38495ab5U);
        }

        /// Put remainder bytes. This must be called only once after `putElement` and before `finalize`.
        void putRemainder(scope const(ubyte[]) data...) pure nothrow @nogc
        {
            assert(data.length < Element.sizeof);
            assert(data.length >= 0);
            element_count += data.length;
            ulong k1 = 0;
            ulong k2 = 0;
            final switch (data.length & 15)
            {
            case 15:
                k2 ^= ulong(data[14]) << 48;
                goto case;
            case 14:
                k2 ^= ulong(data[13]) << 40;
                goto case;
            case 13:
                k2 ^= ulong(data[12]) << 32;
                goto case;
            case 12:
                k2 ^= ulong(data[11]) << 24;
                goto case;
            case 11:
                k2 ^= ulong(data[10]) << 16;
                goto case;
            case 10:
                k2 ^= ulong(data[9]) << 8;
                goto case;
            case 9:
                k2 ^= ulong(data[8]) << 0;
                h2 ^= shuffle(k2, c2, c1, 33);
                goto case;
            case 8:
                k1 ^= ulong(data[7]) << 56;
                goto case;
            case 7:
                k1 ^= ulong(data[6]) << 48;
                goto case;
            case 6:
                k1 ^= ulong(data[5]) << 40;
                goto case;
            case 5:
                k1 ^= ulong(data[4]) << 32;
                goto case;
            case 4:
                k1 ^= ulong(data[3]) << 24;
                goto case;
            case 3:
                k1 ^= ulong(data[2]) << 16;
                goto case;
            case 2:
                k1 ^= ulong(data[1]) << 8;
                goto case;
            case 1:
                k1 ^= ulong(data[0]) << 0;
                h1 ^= shuffle(k1, c1, c2, 31);
                goto case;
            case 0:
            }
        }

        /// Incorporate `element_count` and finalizes the hash.
        void finalize() pure nothrow @nogc
        {
            h1 ^= element_count;
            h2 ^= element_count;

            h1 += h2;
            h2 += h1;
            h1 = fmix(h1);
            h2 = fmix(h2);
            h1 += h2;
            h2 += h1;
        }

        /// Returns the hash as an ulong[2] value.
        Element get() pure nothrow @nogc
        {
            return [h1, h2];
        }

        /// Returns the current hashed value as an ubyte array.
        ubyte[16] getBytes() pure nothrow @nogc
        {
            return cast(typeof(return)) get();
        }
    }
    else
    {
        alias Element = char; // This is needed to trigger the following error message.
        static assert(false, "MurmurHash3(" ~ size.stringof ~ ", " ~ opt.stringof ~ ") is not implemented");
    }

    /++
    Pushes an array of elements at once. It is more efficient to push as much data as possible in a single call.
    On platforms that do not support unaligned reads (MIPS or old ARM chips), the compiler may produce slower code to ensure correctness.
    +/
    void putElements(scope const(Element[]) elements...) pure nothrow @nogc
    {
        foreach (const block; elements)
        {
            putElement(block);
        }
        element_count += elements.length * Element.sizeof;
    }

    //-------------------------------------------------------------------------
    // Implementation of the Digest API.
    //-------------------------------------------------------------------------

    private union BufferUnion
    {
        Element block;
        ubyte[Element.sizeof] data;
    }

    private BufferUnion buffer;
    private size_t bufferSize;

    @disable this(this);

    // Initialize
    void start()
    {
        this = this.init;
    }

    /++
    Adds data to the digester. This function can be called many times in a row
    after start but before finish.
    +/
    void put(scope const(ubyte)[] data...) pure nothrow
    {
        // Buffer should never be full while entering this function.
        assert(bufferSize < Element.sizeof);

        // Check if the incoming data doesn't fill up a whole block buffer.
        if (bufferSize + data.length < Element.sizeof)
        {
            buffer.data[bufferSize .. bufferSize + data.length] = data[];
            bufferSize += data.length;
            return;
        }

        // Check if there's some leftover data in the first block buffer, and
        // fill the remaining space first.
        if (bufferSize != 0)
        {
            const bufferLeeway = Element.sizeof - bufferSize;
            buffer.data[bufferSize .. $] = data[0 .. bufferLeeway];
            putElement(buffer.block);
            element_count += Element.sizeof;
            data = data[bufferLeeway .. $];
        }

        // Do main work: process chunks of `Element.sizeof` bytes.
        const numElements = data.length / Element.sizeof;
        const remainderStart = numElements * Element.sizeof;
        version (HaveUnalignedLoads)
        {
            foreach (ref const Element block; cast(const(Element[])) data[0 .. remainderStart])
            {
                putElement(block);
            }
        }
        else
        {
            void processChunks(T)() @trusted
            {
                alias TChunk = T[Element.sizeof / T.sizeof];
                foreach (ref const chunk; cast(const(TChunk[])) data[0 .. remainderStart])
                {
                    static if (T.alignof >= Element.alignof)
                    {
                        putElement(*cast(const(Element)*) chunk.ptr);
                    }
                    else
                    {
                        Element[1] alignedCopy = void;
                        (cast(T[]) alignedCopy)[] = chunk[];
                        putElement(alignedCopy[0]);
                    }
                }
            }

            const startAddress = cast(size_t) data.ptr;
            static if (size >= 64)
            {
                if ((startAddress & 7) == 0)
                {
                    processChunks!ulong();
                    goto L_end;
                }
            }
            static assert(size >= 32);
            if ((startAddress & 3) == 0)
                processChunks!uint();
            else if ((startAddress & 1) == 0)
                processChunks!ushort();
            else
                processChunks!ubyte();

L_end:
        }
        element_count += numElements * Element.sizeof;
        data = data[remainderStart .. $];

        // Now add remaining data to buffer.
        assert(data.length < Element.sizeof);
        bufferSize = data.length;
        buffer.data[0 .. data.length] = data[];
    }

    /++
    Finalizes the computation of the hash and returns the computed value.
    Note that `finish` can be called only once and that no subsequent calls
    to `put` is allowed.
    +/
    ubyte[Element.sizeof] finish() pure nothrow
    {
        auto tail = buffer.data[0 .. bufferSize];
        if (tail.length > 0)
        {
            putRemainder(tail);
        }
        finalize();
        return getBytes();
    }

    //-------------------------------------------------------------------------
    // MurmurHash3 utils
    //-------------------------------------------------------------------------

    private T rotl(T)(T x, uint y)
    in
    {
        import std.traits : isUnsigned;

        static assert(isUnsigned!T);
        debug assert(y >= 0 && y <= (T.sizeof * 8));
    }
    do
    {
        return ((x << y) | (x >> ((T.sizeof * 8) - y)));
    }

    private T shuffle(T)(T k, T c1, T c2, ubyte r1)
    {
        import std.traits : isUnsigned;

        static assert(isUnsigned!T);
        k *= c1;
        k = rotl(k, r1);
        k *= c2;
        return k;
    }

    private T update(T)(ref T h, T k, T mixWith, T c1, T c2, ubyte r1, ubyte r2, T n)
    {
        import std.traits : isUnsigned;

        static assert(isUnsigned!T);
        h ^= shuffle(k, c1, c2, r1);
        h = rotl(h, r2);
        h += mixWith;
        return h * 5 + n;
    }

    private uint fmix(uint h) pure nothrow @nogc
    {
        h ^= h >> 16;
        h *= 0x85ebca6b;
        h ^= h >> 13;
        h *= 0xc2b2ae35;
        h ^= h >> 16;
        return h;
    }

    private ulong fmix(ulong k) pure nothrow @nogc
    {
        k ^= k >> 33;
        k *= 0xff51afd7ed558ccd;
        k ^= k >> 33;
        k *= 0xc4ceb9fe1a85ec53;
        k ^= k >> 33;
        return k;
    }
}


/// The convenient digest template allows for quick hashing of any data.
@safe unittest
{
    ubyte[4] hashed = digest!(MurmurHash3!32)([1, 2, 3, 4]);
    assert(hashed == [0, 173, 69, 68]);
}

/**
One can also hash ubyte data piecewise by instanciating a hasher and call
the 'put' method.
*/
@safe unittest
{
    const(ubyte)[] data1 = [1, 2, 3];
    const(ubyte)[] data2 = [4, 5, 6, 7];
    // The incoming data will be buffered and hashed element by element.
    MurmurHash3!32 hasher;
    hasher.put(data1);
    hasher.put(data2);
    // The call to 'finish' ensures:
    // - the remaining bits are processed
    // - the hash gets finalized
    auto hashed = hasher.finish();
    assert(hashed == [181, 151, 88, 252]);
}

version (unittest)
{
    private auto hash(H, Element = H.Element)(string data)
    {
        H hasher;
        immutable elements = data.length / Element.sizeof;
        hasher.putElements(cast(const(Element)[]) data[0 .. elements * Element.sizeof]);
        hasher.putRemainder(cast(const(ubyte)[]) data[elements * Element.sizeof .. $]);
        hasher.finalize();
        return hasher.getBytes();
    }

    private void checkResult(H)(in string[string] groundtruth)
    {
        foreach (data, expectedHash; groundtruth)
        {
            assert(data.digest!H.toHexString() == expectedHash);
            assert(data.hash!H.toHexString() == expectedHash);
            H hasher;
            foreach (element; data)
            {
                hasher.put(element);
            }
            assert(hasher.finish.toHexString() == expectedHash);
        }
    }
}

@safe unittest
{
    // dfmt off
    checkResult!(MurmurHash3!32)([
        "" : "00000000",
        "a" : "B269253C",
        "ab" : "5FD7BF9B",
        "abc" : "FA93DDB3",
        "abcd" : "6A67ED43",
        "abcde" : "F69A9BE8",
        "abcdef" : "85C08161",
        "abcdefg" : "069B3C88",
        "abcdefgh" : "C4CCDD49",
        "abcdefghi" : "F0061442",
        "abcdefghij" : "91779288",
        "abcdefghijk" : "DF253B5F",
        "abcdefghijkl" : "273D6FA3",
        "abcdefghijklm" : "1B1612F2",
        "abcdefghijklmn" : "F06D52F8",
        "abcdefghijklmno" : "D2F7099D",
        "abcdefghijklmnop" : "ED9162E7",
        "abcdefghijklmnopq" : "4A5E65B6",
        "abcdefghijklmnopqr" : "94A819C2",
        "abcdefghijklmnopqrs" : "C15BBF85",
        "abcdefghijklmnopqrst" : "9A711CBE",
        "abcdefghijklmnopqrstu" : "ABE7195A",
        "abcdefghijklmnopqrstuv" : "C73CB670",
        "abcdefghijklmnopqrstuvw" : "1C4D1EA5",
        "abcdefghijklmnopqrstuvwx" : "3939F9B0",
        "abcdefghijklmnopqrstuvwxy" : "1A568338",
        "abcdefghijklmnopqrstuvwxyz" : "6D034EA3"]);
    // dfmt on
}

@safe unittest
{
    // dfmt off
    checkResult!(MurmurHash3!(128,32))([
        "" : "00000000000000000000000000000000",
        "a" : "3C9394A71BB056551BB056551BB05655",
        "ab" : "DF5184151030BE251030BE251030BE25",
        "abc" : "D1C6CD75A506B0A2A506B0A2A506B0A2",
        "abcd" : "AACCB6962EC6AF452EC6AF452EC6AF45",
        "abcde" : "FB2E40C5BCC5245D7701725A7701725A",
        "abcdef" : "0AB97CE12127AFA1F9DFBEA9F9DFBEA9",
        "abcdefg" : "D941B590DE3A86092869774A2869774A",
        "abcdefgh" : "3611F4AE8714B1AD92806CFA92806CFA",
        "abcdefghi" : "1C8C05AD6F590622107DD2147C4194DD",
        "abcdefghij" : "A72ED9F50E90379A2AAA92C77FF12F69",
        "abcdefghijk" : "DDC9C8A01E111FCA2DF1FE8257975EBD",
        "abcdefghijkl" : "FE038573C02482F4ADDFD42753E58CD2",
        "abcdefghijklm" : "15A23AC1ECA1AEDB66351CF470DE2CD9",
        "abcdefghijklmn" : "8E11EC75D71F5D60F4456F944D89D4F1",
        "abcdefghijklmno" : "691D6DEEAED51A4A5714CE84A861A7AD",
        "abcdefghijklmnop" : "2776D29F5612B990218BCEE445BA93D1",
        "abcdefghijklmnopq" : "D3A445046F5C51642ADC6DD99D07111D",
        "abcdefghijklmnopqr" : "AA5493A0DA291D966A9E7128585841D9",
        "abcdefghijklmnopqrs" : "281B6A4F9C45B9BFC3B77850930F2C20",
        "abcdefghijklmnopqrst" : "19342546A8216DB62873B49E545DCB1F",
        "abcdefghijklmnopqrstu" : "A6C0F30D6C738620E7B9590D2E088D99",
        "abcdefghijklmnopqrstuv" : "A7D421D9095CDCEA393CBBA908342384",
        "abcdefghijklmnopqrstuvw" : "C3A93D572B014949317BAD7EE809158F",
        "abcdefghijklmnopqrstuvwx" : "802381D77956833791F87149326E4801",
        "abcdefghijklmnopqrstuvwxy" : "0AC619A5302315755A80D74ADEFAA842",
        "abcdefghijklmnopqrstuvwxyz" : "1306343E662F6F666E56F6172C3DE344"]);
    // dfmt on
}

@safe unittest
{
    // dfmt off
    checkResult!(MurmurHash3!(128,64))([
        "" : "00000000000000000000000000000000",
        "a" : "897859F6655555855A890E51483AB5E6",
        "ab" : "2E1BED16EA118B93ADD4529B01A75EE6",
        "abc" : "6778AD3F3F3F96B4522DCA264174A23B",
        "abcd" : "4FCD5646D6B77BB875E87360883E00F2",
        "abcde" : "B8BB96F491D036208CECCF4BA0EEC7C5",
        "abcdef" : "55BFA3ACBF867DE45C842133990971B0",
        "abcdefg" : "99E49EC09F2FCDA6B6BB55B13AA23A1C",
        "abcdefgh" : "028CEF37B00A8ACCA14069EB600D8948",
        "abcdefghi" : "64793CF1CFC0470533E041B7F53DB579",
        "abcdefghij" : "998C2F770D5BC1B6C91A658CDC854DA2",
        "abcdefghijk" : "029D78DFB8D095A871E75A45E2317CBB",
        "abcdefghijkl" : "94E17AE6B19BF38E1C62FF7232309E1F",
        "abcdefghijklm" : "73FAC0A78D2848167FCCE70DFF7B652E",
        "abcdefghijklmn" : "E075C3F5A794D09124336AD2276009EE",
        "abcdefghijklmno" : "FB2F0C895124BE8A612A969C2D8C546A",
        "abcdefghijklmnop" : "23B74C22A33CCAC41AEB31B395D63343",
        "abcdefghijklmnopq" : "57A6BD887F746475E40D11A19D49DAEC",
        "abcdefghijklmnopqr" : "508A7F90EC8CF0776BC7005A29A8D471",
        "abcdefghijklmnopqrs" : "886D9EDE23BC901574946FB62A4D8AA6",
        "abcdefghijklmnopqrst" : "F1E237F926370B314BD016572AF40996",
        "abcdefghijklmnopqrstu" : "3CC9FF79E268D5C9FB3C9BE9C148CCD7",
        "abcdefghijklmnopqrstuv" : "56F8ABF430E388956DA9F4A8741FDB46",
        "abcdefghijklmnopqrstuvw" : "8E234F9DBA0A4840FFE9541CEBB7BE83",
        "abcdefghijklmnopqrstuvwx" : "F72CDED40F96946408F22153A3CF0F79",
        "abcdefghijklmnopqrstuvwxy" : "0F96072FA4CBE771DBBD9E398115EEED",
        "abcdefghijklmnopqrstuvwxyz" : "A94A6F517E9D9C7429D5A7B6899CADE9"]);
    // dfmt on
}

@safe unittest
{
    // Pushing unaligned data and making sure the result is still coherent.
    void testUnalignedHash(H)()
    {
        immutable ubyte[1028] data = 0xAC;
        immutable alignedHash = digest!H(data[0 .. 1024]);
        foreach (i; 1 .. 5)
        {
            immutable unalignedHash = digest!H(data[i .. 1024 + i]);
            assert(alignedHash == unalignedHash);
        }
    }

    testUnalignedHash!(MurmurHash3!32)();
    testUnalignedHash!(MurmurHash3!(128, 32))();
    testUnalignedHash!(MurmurHash3!(128, 64))();
}

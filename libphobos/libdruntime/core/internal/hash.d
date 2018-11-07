/**
 * Written in the D programming language.
 * This module provides functions to uniform calculating hash values for different types
 *
 * Copyright: Copyright Igor Stepanov 2013-2013.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Igor Stepanov
 * Source: $(DRUNTIMESRC core/internal/_hash.d)
 */
module core.internal.hash;

import core.internal.convert;

//enum hash. CTFE depends on base type
size_t hashOf(T)(auto ref T val, size_t seed = 0) if (is(T == enum))
{
    static if (is(T EType == enum)) //for EType
    {
        EType e_val = cast(EType)val;
        return hashOf(e_val, seed);
    }
    else
    {
        static assert(0);
    }
}

//CTFE ready (depends on base type). Can be merged with dynamic array hash
size_t hashOf(T)(auto ref T val, size_t seed = 0) if (!is(T == enum) && __traits(isStaticArray, T))
{
    size_t cur_hash = seed;
    foreach (ref cur; val)
    {
        cur_hash = hashOf(cur, cur_hash);
    }
    return cur_hash;
}

//dynamic array hash
size_t hashOf(T)(auto ref T val, size_t seed = 0)
if (!is(T == enum) && !is(T : typeof(null)) && is(T S: S[]) && !__traits(isStaticArray, T)
    && !is(T == struct) && !is(T == class) && !is(T == union))
{
    alias ElementType = typeof(val[0]);
    static if (is(ElementType == interface) || is(ElementType == class) ||
                   ((is(ElementType == struct) || is(ElementType == union))
                       && is(typeof(val[0].toHash()) == size_t)))
    //class or interface array or struct array with toHash(); CTFE depend on toHash() method
    {
        size_t hash = seed;
        foreach (o; val)
        {
            hash = hashOf(o, hash);
        }
        return hash;
    }
    else static if (is(typeof(toUbyte(val)) == const(ubyte)[]))
    //ubyteble array (arithmetic types and structs without toHash) CTFE ready for arithmetic types and structs without reference fields
    {
        auto bytes = toUbyte(val);
        return bytesHash(bytes.ptr, bytes.length, seed);
    }
    else //Other types. CTFE unsupported
    {
        assert(!__ctfe, "unable to compute hash of "~T.stringof);
        return bytesHash(val.ptr, ElementType.sizeof*val.length, seed);
    }
}

//arithmetic type hash
@trusted nothrow pure
size_t hashOf(T)(auto ref T val, size_t seed = 0) if (!is(T == enum) && __traits(isArithmetic, T))
{
    static if (__traits(isFloating, val))
    {
        T data = (val != val) ? T.nan : val;
        auto bytes = toUbyte(data);
        return bytesHash(bytes.ptr, bytes.length, seed);
    }
    else
    {
        auto bytes = toUbyte(val);
        return bytesHash(bytes.ptr, bytes.length, seed);
    }
}

//typeof(null) hash. CTFE supported
@trusted nothrow pure
size_t hashOf(T)(auto ref T val, size_t seed = 0) if (!is(T == enum) && is(T : typeof(null)))
{
    return hashOf(cast(void*)null);
}

//Pointers hash. CTFE unsupported if not null
@trusted nothrow pure
size_t hashOf(T)(auto ref T val, size_t seed = 0)
if (!is(T == enum) && is(T V : V*) && !is(T : typeof(null))
    && !is(T == struct) && !is(T == class) && !is(T == union))
{
    if (__ctfe)
    {
        if (val is null)
        {
            return hashOf(cast(size_t)0);
        }
        else
        {
            assert(0, "Unable to calculate hash of non-null pointer at compile time");
        }

    }
    return hashOf(cast(size_t)val);
}

//struct or union hash
size_t hashOf(T)(auto ref T val, size_t seed = 0) if (!is(T == enum) && (is(T == struct) || is(T == union)))
{
    static if (is(typeof(val.toHash()) == size_t)) //CTFE depends on toHash()
    {
        return hashOf(val.toHash(), seed);
    }
    else
    {
        static if (__traits(hasMember, T, "toHash") && is(typeof(T.toHash) == function))
        {
            pragma(msg, "Warning: struct "~__traits(identifier, T)~" has method toHash, however it cannot be called with "~T.stringof~" this.");
        }

        static if (is(typeof(toUbyte(val)) == const(ubyte)[]))//CTFE ready for structs without reference fields
        {
            auto bytes = toUbyte(val);
            return bytesHash(bytes.ptr, bytes.length, seed);
        }
        else // CTFE unsupproreted for structs with reference fields
        {
            assert(!__ctfe, "unable to compute hash of "~T.stringof);
            const(ubyte)[] bytes = (cast(const(ubyte)*)&val)[0 .. T.sizeof];
            return bytesHash(bytes.ptr, bytes.length, seed);
        }
    }
}

//delegate hash. CTFE unsupported
@trusted nothrow pure
size_t hashOf(T)(auto ref T val, size_t seed = 0) if (!is(T == enum) && is(T == delegate))
{
    assert(!__ctfe, "unable to compute hash of "~T.stringof);
    const(ubyte)[] bytes = (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    return bytesHash(bytes.ptr, bytes.length, seed);
}

//class or interface hash. CTFE depends on toHash
size_t hashOf(T)(auto ref T val, size_t seed = 0) if (!is(T == enum) && is(T == interface) || is(T == class))
{
    return hashOf(val ? (cast(Object)val).toHash() : 0, seed);
}

//associative array hash. CTFE depends on base types
size_t hashOf(T)(auto ref T aa, size_t seed = 0) if (!is(T == enum) && __traits(isAssociativeArray, T))
{
    if (!aa.length) return hashOf(0, seed);
    size_t h = 0;

    // The computed hash is independent of the foreach traversal order.
    foreach (key, ref val; aa)
    {
        size_t[2] hpair;
        hpair[0] = key.hashOf();
        hpair[1] = val.hashOf();
        h ^= hpair.hashOf();
    }
    return h.hashOf(seed);
}

unittest
{
    static struct Foo
    {
        int a = 99;
        float b = 4.0;
        size_t toHash() const pure @safe nothrow
        {
            return a;
        }
    }

    static struct Bar
    {
        char c = 'x';
        int a = 99;
        float b = 4.0;
        void* d = null;
    }

    static struct Boom
    {
        char c = 'M';
        int* a = null;
    }

    interface IBoo
    {
        void boo();
    }

    static class Boo: IBoo
    {
        override void boo()
        {
        }

        override size_t toHash()
        {
            return 1;
        }
    }

    static struct Goo
    {
        size_t toHash() pure @safe nothrow
        {
            return 1;
        }
    }

    enum Gun: long
    {
        A = 99,
        B = 17
    }

    enum double dexpr = 3.14;
    enum float fexpr = 2.71;
    enum wstring wsexpr = "abcdef"w;
    enum string csexpr = "abcdef";
    enum int iexpr = 7;
    enum long lexpr = 42;
    enum int[2][3] saexpr = [[1, 2], [3, 4], [5, 6]];
    enum int[] daexpr = [7,8,9];
    enum Foo thsexpr = Foo();
    enum Bar vsexpr = Bar();
    enum int[int] aaexpr = [99:2, 12:6, 45:4];
    enum Gun eexpr = Gun.A;
    enum cdouble cexpr = 7+4i;
    enum Foo[] staexpr = [Foo(), Foo(), Foo()];
    enum Bar[] vsaexpr = [Bar(), Bar(), Bar()];
    enum realexpr = 7.88;
    enum raexpr = [8.99L+86i, 3.12L+99i, 5.66L+12i];
    enum nullexpr = null;

    //No CTFE:
    Boom rstructexpr = Boom();
    Boom[] rstrarrexpr = [Boom(), Boom(), Boom()];
    int delegate() dgexpr  = (){return 78;};
    void* ptrexpr = &dgexpr;


    //CTFE hashes
    enum h1 = dexpr.hashOf();
    enum h2 = fexpr.hashOf();
    enum h3 = wsexpr.hashOf();
    enum h4 = csexpr.hashOf();
    enum h5 = iexpr.hashOf();
    enum h6 = lexpr.hashOf();
    enum h7 = saexpr.hashOf();
    enum h8 = daexpr.hashOf();
    enum h9 = thsexpr.hashOf();
    enum h10 = vsexpr.hashOf();
    enum h11 = aaexpr.hashOf();
    enum h12 = eexpr.hashOf();
    enum h13 = cexpr.hashOf();
    enum h14 = hashOf(new Boo);
    enum h15 = staexpr.hashOf();
    enum h16 = hashOf([new Boo, new Boo, new Boo]);
    enum h17 = hashOf([cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo]);
    enum h18 = hashOf(cast(IBoo)new Boo);
    enum h19 = vsaexpr.hashOf();
    enum h20 = hashOf(cast(Foo[3])staexpr);

    //BUG: cannot cast [Boo(), Boo(), Boo()][0] to object.Object at compile time
    auto h21 = hashOf(cast(Boo[3])[new Boo, new Boo, new Boo]);
    auto h22 = hashOf(cast(IBoo[3])[cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo]);
    enum h23 = hashOf(cast(Bar[3])vsaexpr);

    //NO CTFE (Compute, but don't check correctness):
    auto h24 = rstructexpr.hashOf();
    auto h25 = rstrarrexpr.hashOf();
    auto h26 = dgexpr.hashOf();
    auto h27 = ptrexpr.hashOf();

    enum h28 = realexpr.hashOf();
    enum h29 = raexpr.hashOf();
    enum h30 = nullexpr.hashOf();

    auto v1 = dexpr;
    auto v2 = fexpr;
    auto v3 = wsexpr;
    auto v4 = csexpr;
    auto v5 = iexpr;
    auto v6 = lexpr;
    auto v7 = saexpr;
    auto v8 = daexpr;
    auto v9 = thsexpr;
    auto v10 = vsexpr;
    auto v11 = aaexpr;
    auto v12 = eexpr;
    auto v13 = cexpr;
    auto v14 = new Boo;
    auto v15 = staexpr;
    auto v16 = [new Boo, new Boo, new Boo];
    auto v17 = [cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo];
    auto v18 = cast(IBoo)new Boo;
    auto v19 = vsaexpr;
    auto v20 = cast(Foo[3])staexpr;
    auto v21 = cast(Boo[3])[new Boo, new Boo, new Boo];
    auto v22 = cast(IBoo[3])[cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo];
    auto v23 = cast(Bar[3])vsaexpr;
    auto v30 = null;

    //NO CTFE:
    /*auto v24 = rstructexpr;
    auto v25 = rstrarrexpr;
    auto v26 = dgexpr;
    auto v27 = ptrexpr;
    auto v28 = realexpr;
    auto v29 = raexpr;*/

    //runtime hashes
    auto rth1 = hashOf(v1);
    auto rth2 = hashOf(v2);
    auto rth3 = hashOf(v3);
    auto rth4 = hashOf(v4);
    auto rth5 = hashOf(v5);
    auto rth6 = hashOf(v6);
    auto rth7 = hashOf(v7);
    auto rth8 = hashOf(v8);
    auto rth9 = hashOf(v9);
    auto rth10 = hashOf(v10);
    auto rth11 = hashOf(v11);
    auto rth12 = hashOf(v12);
    auto rth13 = hashOf(v13);
    auto rth14 = hashOf(v14);
    auto rth15 = hashOf(v15);
    auto rth16 = hashOf(v16);
    auto rth17 = hashOf(v17);
    auto rth18 = hashOf(v18);
    auto rth19 = hashOf(v19);
    auto rth20 = hashOf(v20);
    auto rth21 = hashOf(v21);
    auto rth22 = hashOf(v22);
    auto rth23 = hashOf(v23);
    auto rth30 = hashOf(v30);
    /*//NO CTFE:
    auto rth24 = hashOf(v24);
    auto rth25 = hashOf(v25);
    auto rth26 = hashOf(v26);
    auto rth27 = hashOf(v27);
    auto rth28 = hashOf(v28);
    auto rth29 = hashOf(v29);*/

    assert(h1 == rth1);
    assert(h2 == rth2);
    assert(h3 == rth3);
    assert(h4 == rth4);
    assert(h5 == rth5);
    assert(h6 == rth6);
    assert(h7 == rth7);
    assert(h8 == rth8);
    assert(h9 == rth9);
    assert(h10 == rth10);
    assert(h11 == rth11);
    assert(h12 == rth12);
    assert(h13 == rth13);
    assert(h14 == rth14);
    assert(h15 == rth15);
    assert(h16 == rth16);
    assert(h17 == rth17);
    assert(h18 == rth18);
    assert(h19 == rth19);
    assert(h20 == rth20);
    assert(h21 == rth21);
    assert(h22 == rth22);
    assert(h23 == rth23);
    /*assert(h24 == rth24);
    assert(h25 == rth25);
    assert(h26 == rth26);
    assert(h27 == rth27);
    assert(h28 == rth28);
    assert(h29 == rth29);*/
    assert(h30 == rth30);
}


unittest // issue 15111
{
    void testAlias(T)()
    {
        static struct Foo
        {
            T t;
            alias t this;
        }
        Foo foo;
        static assert(is(typeof(hashOf(foo))));
    }
    // was fixed
    testAlias!(int[]);
    testAlias!(int*);
    // was not affected
    testAlias!int;
    testAlias!(void delegate());
    testAlias!(string[string]);
    testAlias!(int[8]);
}

// MurmurHash3 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.

version (X86)
    version = AnyX86;
version (X86_64)
    version = AnyX86;

version (AnyX86)
{
    version (DigitalMars)
    {
    }
    else
    {
        version = HasUnalignedOps;
    }
}


@system pure nothrow @nogc
size_t bytesHash(const(void)* buf, size_t len, size_t seed)
{
    static uint rotl32(uint n)(in uint x) pure nothrow @safe @nogc
    {
        return (x << n) | (x >> (32 - n));
    }

    //-----------------------------------------------------------------------------
    // Block read - if your platform needs to do endian-swapping or can only
    // handle aligned reads, do the conversion here
    static uint get32bits(const (ubyte)* x) pure nothrow @nogc
    {
        //Compiler can optimize this code to simple *cast(uint*)x if it possible.
        version (HasUnalignedOps)
        {
            if (!__ctfe)
                return *cast(uint*)x; //BUG: Can't be inlined by DMD
        }
        version (BigEndian)
        {
            return ((cast(uint) x[0]) << 24) | ((cast(uint) x[1]) << 16) | ((cast(uint) x[2]) << 8) | (cast(uint) x[3]);
        }
        else
        {
            return ((cast(uint) x[3]) << 24) | ((cast(uint) x[2]) << 16) | ((cast(uint) x[1]) << 8) | (cast(uint) x[0]);
        }
    }

    //-----------------------------------------------------------------------------
    // Finalization mix - force all bits of a hash block to avalanche
    static uint fmix32(uint h) pure nothrow @safe @nogc
    {
        h ^= h >> 16;
        h *= 0x85ebca6b;
        h ^= h >> 13;
        h *= 0xc2b2ae35;
        h ^= h >> 16;

        return h;
    }

    auto data = cast(const(ubyte)*)buf;
    auto nblocks = len / 4;

    uint h1 = cast(uint)seed;

    enum uint c1 = 0xcc9e2d51;
    enum uint c2 = 0x1b873593;
    enum uint c3 = 0xe6546b64;

    //----------
    // body
    auto end_data = data+nblocks*uint.sizeof;
    for (; data!=end_data; data += uint.sizeof)
    {
        uint k1 = get32bits(data);
        k1 *= c1;
        k1 = rotl32!15(k1);
        k1 *= c2;

        h1 ^= k1;
        h1 = rotl32!13(h1);
        h1 = h1*5+c3;
    }

    //----------
    // tail
    uint k1 = 0;

    switch (len & 3)
    {
        case 3: k1 ^= data[2] << 16; goto case;
        case 2: k1 ^= data[1] << 8;  goto case;
        case 1: k1 ^= data[0];
                k1 *= c1; k1 = rotl32!15(k1); k1 *= c2; h1 ^= k1;
                goto default;
        default:
    }

    //----------
    // finalization
    h1 ^= len;
    h1 = fmix32(h1);
    return h1;
}

//  Check that bytesHash works with CTFE
pure nothrow @system @nogc unittest
{
    size_t ctfeHash(string x)
    {
        return bytesHash(x.ptr, x.length, 0);
    }

    enum test_str = "Sample string";
    enum size_t hashVal = ctfeHash(test_str);
    assert(hashVal == bytesHash(&test_str[0], test_str.length, 0));
}

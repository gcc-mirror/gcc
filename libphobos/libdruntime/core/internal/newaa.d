/**
   Turn an Associative Array into a binary compatible struct for static initialization.

   This does not implement all the pieces of
   the associative array type in druntime, just enough to create an AA from an
   existing range of key/value pairs.

   Copyright: Copyright Digital Mars 2000 - 2015, Steven Schveighoffer 2022.
   License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
   Authors:   Martin Nowak, Steven Schveighoffer
*/
module core.internal.newaa;

import core.memory;

// grow threshold
private enum GROW_NUM = 4;
private enum GROW_DEN = 5;
// shrink threshold
private enum SHRINK_NUM = 1;
private enum SHRINK_DEN = 8;
// grow factor
private enum GROW_FAC = 4;
// growing the AA doubles it's size, so the shrink threshold must be
// smaller than half the grow threshold to have a hysteresis
static assert(GROW_FAC * SHRINK_NUM * GROW_DEN < GROW_NUM * SHRINK_DEN);
// initial load factor (for literals), mean of both thresholds
private enum INIT_NUM = (GROW_DEN * SHRINK_NUM + GROW_NUM * SHRINK_DEN) / 2;
private enum INIT_DEN = SHRINK_DEN * GROW_DEN;

private enum INIT_NUM_BUCKETS = 8;
// magic hash constants to distinguish empty, deleted, and filled buckets
private enum HASH_EMPTY = 0;
private enum HASH_FILLED_MARK = size_t(1) << 8 * size_t.sizeof - 1;

private struct Bucket
{
    size_t hash;
    void *entry;
}

struct Impl
{
    Bucket[] buckets;
    uint used;
    uint deleted;
    TypeInfo_Struct entryTI;
    uint firstUsed;
    immutable uint keysz;
    immutable uint valsz;
    immutable uint valoff;
    Flags flags;
    size_t delegate(scope const void*) nothrow hashFn;

    enum Flags : ubyte
    {
        none = 0x0,
        keyHasPostblit = 0x1,
        hasPointers = 0x2,
    }
}

private struct AAShell
{
    Impl *impl;
}

private size_t mix(size_t h) @safe pure nothrow @nogc
{
    // final mix function of MurmurHash2
    enum m = 0x5bd1e995;
    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;
    return h;
}

struct Entry(K, V)
{
    // can make this const, because we aren't really going to use it aside from
    // construction.
    const K key;
    V value;
}


// create a binary-compatible AA structure that can be used directly as an
// associative array.
// NOTE: this must only be called during CTFE
AAShell makeAA(K, V)(V[K] src) @trusted
{
    assert(__ctfe, "makeAA Must only be called at compile time");
    immutable srclen = src.length;
    assert(srclen <= uint.max);
    alias E = Entry!(K, V);
    if (srclen == 0)
        return AAShell.init;
    // first, determine the size that would be used if we grew the bucket list
    // one element at a time using the standard AA algorithm.
    size_t dim = INIT_NUM_BUCKETS;
    while (srclen * GROW_DEN > dim * GROW_NUM)
        dim = dim * GROW_FAC;

    // used during runtime.
    size_t delegate(scope const void *) nothrow hashFn = (scope const void* val) {
        auto x = cast(K*)val;
        return hashOf(*x);
    };

    Bucket[] buckets;
    // Allocate and fill the buckets
    if (__ctfe)
        buckets = new Bucket[dim];
    else
        assert(0);

    assert(buckets.length >= dim);

    immutable mask = dim - 1;
    assert((dim & mask) == 0); // must be a power of 2

    Bucket* findSlotInsert(immutable size_t hash)
    {
        for (size_t i = hash & mask, j = 1;; ++j)
        {
            if (buckets[i].hash == HASH_EMPTY)
                return &buckets[i];
            i = (i + j) & mask;
        }
    }

    uint firstUsed = cast(uint) buckets.length;
    foreach (k, v; src)
    {
        immutable h = hashOf(k).mix | HASH_FILLED_MARK;
        auto location = findSlotInsert(h);
        immutable nfu = cast(uint) (location - &buckets[0]);
        if (nfu < firstUsed)
            firstUsed = nfu;
        *location = Bucket(h, new E(k, v));
    }

    enum flags = () {
        import core.internal.traits;
        Impl.Flags flags;
        static if (__traits(hasPostblit, K))
            flags |= flags.keyHasPostblit;
        static if (hasIndirections!E)
            flags |= flags.hasPointers;
        return flags;
    } ();
    // return the new implementation
    return AAShell(new Impl(buckets, cast(uint)srclen, 0, typeid(E), firstUsed,
            K.sizeof, V.sizeof, E.value.offsetof, flags, hashFn));
}

unittest
{
    static struct Foo
    {
        ubyte x;
        double d;
    }
    static int[Foo] utaa = [Foo(1, 2.0) : 5];
    auto k = Foo(1, 2.0);
    // verify that getHash doesn't match hashOf for Foo
    assert(typeid(Foo).getHash(&k) != hashOf(k));
    assert(utaa[Foo(1, 2.0)] == 5);
}

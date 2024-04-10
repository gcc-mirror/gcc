/**
 * Implementation of associative arrays.
 *
 * Copyright: Copyright Digital Mars 2000 - 2015.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 * Source: $(DRUNTIMESRC rt/_aaA.d)
 */
module rt.aaA;

/// AA version for debuggers, bump whenever changing the layout
extern (C) immutable int _aaVersion = 1;

import core.memory : GC;
import core.internal.util.math : min, max;

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
private enum HASH_DELETED = 0x1;
private enum HASH_FILLED_MARK = size_t(1) << 8 * size_t.sizeof - 1;

/// Opaque AA wrapper
struct AA
{
    Impl* impl;
    alias impl this;

    private @property bool empty() const pure nothrow @nogc @safe
    {
        return impl is null || !impl.length;
    }
}

private struct Impl
{
private:
    this(scope const TypeInfo_AssociativeArray ti, size_t sz = INIT_NUM_BUCKETS) nothrow
    {
        keysz = cast(uint) ti.key.tsize;
        valsz = cast(uint) ti.value.tsize;
        buckets = allocBuckets(sz);
        firstUsed = cast(uint) buckets.length;
        valoff = cast(uint) talign(keysz, ti.value.talign);
        hashFn = &ti.key.getHash;

        import rt.lifetime : hasPostblit, unqualify;

        if (hasPostblit(unqualify(ti.key)))
            flags |= Flags.keyHasPostblit;
        if ((ti.key.flags | ti.value.flags) & 1)
            flags |= Flags.hasPointers;

        entryTI = fakeEntryTI(this, ti.key, ti.value);
    }

    Bucket[] buckets;
    uint used;
    uint deleted;
    TypeInfo_Struct entryTI;
    uint firstUsed;
    immutable uint keysz;
    immutable uint valsz;
    immutable uint valoff;
    Flags flags;

    // function that calculates hash of a key. Set on creation
    // the parameter is a pointer to the key.
    size_t delegate(scope const void*) nothrow hashFn;

    enum Flags : ubyte
    {
        none = 0x0,
        keyHasPostblit = 0x1,
        hasPointers = 0x2,
    }

    @property size_t length() const pure nothrow @nogc @safe
    {
        assert(used >= deleted);
        return used - deleted;
    }

    @property size_t dim() const pure nothrow @nogc @safe
    {
        return buckets.length;
    }

    @property size_t mask() const pure nothrow @nogc
    {
        return dim - 1;
    }

    // find the first slot to insert a value with hash
    inout(Bucket)* findSlotInsert(size_t hash) inout pure nothrow @nogc
    {
        for (size_t i = hash & mask, j = 1;; ++j)
        {
            if (!buckets[i].filled)
                return &buckets[i];
            i = (i + j) & mask;
        }
    }

    // lookup a key
    inout(Bucket)* findSlotLookup(size_t hash, scope const void* pkey, scope const TypeInfo keyti) inout
    {
        for (size_t i = hash & mask, j = 1;; ++j)
        {
            if (buckets[i].hash == hash && keyti.equals(pkey, buckets[i].entry))
                return &buckets[i];
            else if (buckets[i].empty)
                return null;
            i = (i + j) & mask;
        }
    }

    void grow(scope const TypeInfo keyti) pure nothrow
    {
        // If there are so many deleted entries, that growing would push us
        // below the shrink threshold, we just purge deleted entries instead.
        if (length * SHRINK_DEN < GROW_FAC * dim * SHRINK_NUM)
            resize(dim);
        else
            resize(GROW_FAC * dim);
    }

    void shrink(scope const TypeInfo keyti) pure nothrow
    {
        if (dim > INIT_NUM_BUCKETS)
            resize(dim / GROW_FAC);
    }

    void resize(size_t ndim) pure nothrow
    {
        auto obuckets = buckets;
        buckets = allocBuckets(ndim);

        foreach (ref b; obuckets[firstUsed .. $])
            if (b.filled)
                *findSlotInsert(b.hash) = b;

        firstUsed = 0;
        used -= deleted;
        deleted = 0;
        GC.free(obuckets.ptr); // safe to free b/c impossible to reference
    }

    void clear() pure nothrow @trusted
    {
        import core.stdc.string : memset;
        // clear all data, but don't change bucket array length
        memset(&buckets[firstUsed], 0, (buckets.length - firstUsed) * Bucket.sizeof);
        deleted = used = 0;
        firstUsed = cast(uint) dim;
    }
}

//==============================================================================
// Bucket
//------------------------------------------------------------------------------

private struct Bucket
{
private pure nothrow @nogc:
    size_t hash;
    void* entry;

    @property bool empty() const
    {
        return hash == HASH_EMPTY;
    }

    @property bool deleted() const
    {
        return hash == HASH_DELETED;
    }

    @property bool filled() const @safe
    {
        return cast(ptrdiff_t) hash < 0;
    }
}

Bucket[] allocBuckets(size_t dim) @trusted pure nothrow
{
    enum attr = GC.BlkAttr.NO_INTERIOR;
    immutable sz = dim * Bucket.sizeof;
    return (cast(Bucket*) GC.calloc(sz, attr))[0 .. dim];
}

//==============================================================================
// Entry
//------------------------------------------------------------------------------

private void* allocEntry(scope const Impl* aa, scope const void* pkey)
{
    import rt.lifetime : _d_newitemU;
    import core.stdc.string : memcpy, memset;

    immutable akeysz = aa.valoff;
    void* res = void;
    if (aa.entryTI)
        res = _d_newitemU(aa.entryTI);
    else
    {
        auto flags = (aa.flags & Impl.Flags.hasPointers) ? 0 : GC.BlkAttr.NO_SCAN;
        res = GC.malloc(akeysz + aa.valsz, flags);
    }

    memcpy(res, pkey, aa.keysz); // copy key
    memset(res + akeysz, 0, aa.valsz); // zero value

    return res;
}

package void entryDtor(void* p, const TypeInfo_Struct sti)
{
    // key and value type info stored after the TypeInfo_Struct by tiEntry()
    auto sizeti = __traits(classInstanceSize, TypeInfo_Struct);
    auto extra = cast(const(TypeInfo)*)(cast(void*) sti + sizeti);
    extra[0].destroy(p);
    extra[1].destroy(p + talign(extra[0].tsize, extra[1].talign));
}

private bool hasDtor(const TypeInfo ti) pure nothrow
{
    import rt.lifetime : unqualify;

    if (typeid(ti) is typeid(TypeInfo_Struct))
        if ((cast(TypeInfo_Struct) cast(void*) ti).xdtor)
            return true;
    if (typeid(ti) is typeid(TypeInfo_StaticArray))
        return hasDtor(unqualify(ti.next));

    return false;
}

private immutable(void)* getRTInfo(const TypeInfo ti) pure nothrow
{
    // classes are references
    const isNoClass = ti && typeid(ti) !is typeid(TypeInfo_Class);
    return isNoClass ? ti.rtInfo() : rtinfoHasPointers;
}

// build type info for Entry with additional key and value fields
TypeInfo_Struct fakeEntryTI(ref Impl aa, const TypeInfo keyti, const TypeInfo valti) nothrow
{
    import rt.lifetime : unqualify;

    auto kti = unqualify(keyti);
    auto vti = unqualify(valti);

    // figure out whether RTInfo has to be generated (indicated by rtisize > 0)
    enum pointersPerWord = 8 * (void*).sizeof * (void*).sizeof;
    auto rtinfo = rtinfoNoPointers;
    size_t rtisize = 0;
    immutable(size_t)* keyinfo = void;
    immutable(size_t)* valinfo = void;
    if (aa.flags & Impl.Flags.hasPointers)
    {
        // classes are references
        keyinfo = cast(immutable(size_t)*) getRTInfo(keyti);
        valinfo = cast(immutable(size_t)*) getRTInfo(valti);

        if (keyinfo is rtinfoHasPointers && valinfo is rtinfoHasPointers)
            rtinfo = rtinfoHasPointers;
        else
            rtisize = 1 + (aa.valoff + aa.valsz + pointersPerWord - 1) / pointersPerWord;
    }
    bool entryHasDtor = hasDtor(kti) || hasDtor(vti);
    if (rtisize == 0 && !entryHasDtor)
        return null;

    // save kti and vti after type info for struct
    enum sizeti = __traits(classInstanceSize, TypeInfo_Struct);
    void* p = GC.malloc(sizeti + (2 + rtisize) * (void*).sizeof);
    import core.stdc.string : memcpy;

    memcpy(p, __traits(initSymbol, TypeInfo_Struct).ptr, sizeti);

    auto ti = cast(TypeInfo_Struct) p;
    auto extra = cast(TypeInfo*)(p + sizeti);
    extra[0] = cast() kti;
    extra[1] = cast() vti;

    static immutable tiMangledName = "S2rt3aaA__T5EntryZ";
    ti.mangledName = tiMangledName;

    ti.m_RTInfo = rtisize > 0 ? rtinfoEntry(aa, keyinfo, valinfo, cast(size_t*)(extra + 2), rtisize) : rtinfo;
    ti.m_flags = ti.m_RTInfo is rtinfoNoPointers ? cast(TypeInfo_Struct.StructFlags)0 : TypeInfo_Struct.StructFlags.hasPointers;

    // we don't expect the Entry objects to be used outside of this module, so we have control
    // over the non-usage of the callback methods and other entries and can keep these null
    // xtoHash, xopEquals, xopCmp, xtoString and xpostblit
    immutable entrySize = aa.valoff + aa.valsz;
    ti.m_init = (cast(ubyte*) null)[0 .. entrySize]; // init length, but not ptr

    if (entryHasDtor)
    {
        // xdtor needs to be built from the dtors of key and value for the GC
        ti.xdtorti = &entryDtor;
        ti.m_flags |= TypeInfo_Struct.StructFlags.isDynamicType;
    }

    ti.m_align = cast(uint) max(kti.talign, vti.talign);

    return ti;
}

// build appropriate RTInfo at runtime
immutable(void)* rtinfoEntry(ref Impl aa, immutable(size_t)* keyinfo,
    immutable(size_t)* valinfo, size_t* rtinfoData, size_t rtinfoSize) pure nothrow
{
    enum bitsPerWord = 8 * size_t.sizeof;

    rtinfoData[0] = aa.valoff + aa.valsz;
    rtinfoData[1..rtinfoSize] = 0;

    void copyKeyInfo(string src)()
    {
        size_t pos = 1;
        size_t keybits = aa.keysz / (void*).sizeof;
        while (keybits >= bitsPerWord)
        {
            rtinfoData[pos] = mixin(src);
            keybits -= bitsPerWord;
            pos++;
        }
        if (keybits > 0)
            rtinfoData[pos] = mixin(src) & ((cast(size_t) 1 << keybits) - 1);
    }

    if (keyinfo is rtinfoHasPointers)
        copyKeyInfo!"~cast(size_t) 0"();
    else if (keyinfo !is rtinfoNoPointers)
        copyKeyInfo!"keyinfo[pos]"();

    void copyValInfo(string src)()
    {
        size_t bitpos = aa.valoff / (void*).sizeof;
        size_t pos = 1;
        size_t dstpos = 1 + bitpos / bitsPerWord;
        size_t begoff = bitpos % bitsPerWord;
        size_t valbits = aa.valsz / (void*).sizeof;
        size_t endoff = (bitpos + valbits) % bitsPerWord;
        for (;;)
        {
            const bits = bitsPerWord - begoff;
            size_t s = mixin(src);
            rtinfoData[dstpos] |= s << begoff;
            if (begoff > 0 && valbits > bits)
                rtinfoData[dstpos+1] |= s >> bits;
            if (valbits < bitsPerWord)
                break;
            valbits -= bitsPerWord;
            dstpos++;
            pos++;
        }
        if (endoff > 0)
            rtinfoData[dstpos] &= ((cast(size_t) 1 << endoff) - 1);
    }

    if (valinfo is rtinfoHasPointers)
        copyValInfo!"~cast(size_t) 0"();
    else if (valinfo !is rtinfoNoPointers)
        copyValInfo!"valinfo[pos]"();

    return cast(immutable(void)*) rtinfoData;
}

unittest
{
    void test(K, V)()
    {
        static struct Entry
        {
            K key;
            V val;
        }
        auto keyti = typeid(K);
        auto valti = typeid(V);
        auto valrti = getRTInfo(valti);
        auto keyrti = getRTInfo(keyti);

        auto impl = new Impl(typeid(V[K]));
        if (valrti is rtinfoNoPointers && keyrti is rtinfoNoPointers)
        {
            assert(!(impl.flags & Impl.Flags.hasPointers));
            assert(impl.entryTI is null);
        }
        else if (valrti is rtinfoHasPointers && keyrti is rtinfoHasPointers)
        {
            assert(impl.flags & Impl.Flags.hasPointers);
            assert(impl.entryTI is null);
        }
        else
        {
            auto rtInfo  = cast(size_t*) impl.entryTI.rtInfo();
            auto refInfo = cast(size_t*) typeid(Entry).rtInfo();
            assert(rtInfo[0] == refInfo[0]); // size
            enum bytesPerWord = 8 * size_t.sizeof * (void*).sizeof;
            size_t words = (rtInfo[0] + bytesPerWord - 1) / bytesPerWord;
            foreach (i; 0 .. words)
                assert(rtInfo[1 + i] == refInfo[i + 1]);
        }
    }
    test!(long, int)();
    test!(string, string);
    test!(ubyte[16], Object);

    static struct Small
    {
        ubyte[16] guid;
        string name;
    }
    test!(string, Small);

    static struct Large
    {
        ubyte[1024] data;
        string[412] names;
        ubyte[1024] moredata;
    }
    version (OnlyLowMemUnittests) {} else
    test!(Large, Large);
}

//==============================================================================
// Helper functions
//------------------------------------------------------------------------------

private size_t talign(size_t tsize, size_t algn) @safe pure nothrow @nogc
{
    immutable mask = algn - 1;
    assert(!(mask & algn));
    return (tsize + mask) & ~mask;
}

// mix hash to "fix" bad hash functions
private size_t mix(size_t h) @safe pure nothrow @nogc
{
    // final mix function of MurmurHash2
    enum m = 0x5bd1e995;
    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;
    return h;
}

private size_t calcHash(scope const void *pkey, scope const Impl* impl) nothrow
{
    immutable hash = impl.hashFn(pkey);
    // highest bit is set to distinguish empty/deleted from filled buckets
    return mix(hash) | HASH_FILLED_MARK;
}

private size_t nextpow2(const size_t n) pure nothrow @nogc
{
    import core.bitop : bsr;

    if (!n)
        return 1;

    const isPowerOf2 = !((n - 1) & n);
    return 1 << (bsr(n) + !isPowerOf2);
}

pure nothrow @nogc unittest
{
    //                            0, 1, 2, 3, 4, 5, 6, 7, 8,  9
    foreach (const n, const pow2; [1, 1, 2, 4, 4, 8, 8, 8, 8, 16])
        assert(nextpow2(n) == pow2);
}

//==============================================================================
// API Implementation
//------------------------------------------------------------------------------

/** Allocate associative array data.
 * Called for `new SomeAA` expression.
 * Params:
 *      ti = TypeInfo for the associative array
 * Returns:
 *      A new associative array.
 */
extern (C) Impl* _aaNew(const TypeInfo_AssociativeArray ti)
{
    return new Impl(ti);
}

/// Determine number of entries in associative array.
extern (C) size_t _aaLen(scope const AA aa) pure nothrow @nogc
{
    return aa ? aa.length : 0;
}

/******************************
 * Lookup *pkey in aa.
 * Called only from implementation of (aa[key]) expressions when value is mutable.
 * Params:
 *      paa = associative array opaque pointer
 *      ti = TypeInfo for the associative array
 *      valsz = ignored
 *      pkey = pointer to the key value
 * Returns:
 *      if key was in the aa, a mutable pointer to the existing value.
 *      If key was not in the aa, a mutable pointer to newly inserted value which
 *      is set to all zeros
 */
extern (C) void* _aaGetY(scope AA* paa, const TypeInfo_AssociativeArray ti,
    const size_t valsz, scope const void* pkey)
{
    bool found;
    return _aaGetX(paa, ti, valsz, pkey, found);
}

/******************************
 * Lookup *pkey in aa.
 * Called only from implementation of require
 * Params:
 *      paa = associative array opaque pointer
 *      ti = TypeInfo for the associative array
 *      valsz = ignored
 *      pkey = pointer to the key value
 *      found = true if the value was found
 * Returns:
 *      if key was in the aa, a mutable pointer to the existing value.
 *      If key was not in the aa, a mutable pointer to newly inserted value which
 *      is set to all zeros
 */
extern (C) void* _aaGetX(scope AA* paa, const TypeInfo_AssociativeArray ti,
    const size_t valsz, scope const void* pkey, out bool found)
{
    // lazily alloc implementation
    AA aa = *paa;
    if (aa is null)
    {
        aa = new Impl(ti);
        *paa = aa;
    }

    // get hash and bucket for key
    immutable hash = calcHash(pkey, aa);

    // found a value => return it
    if (auto p = aa.findSlotLookup(hash, pkey, ti.key))
    {
        found = true;
        return p.entry + aa.valoff;
    }

    auto p = aa.findSlotInsert(hash);
    if (p.deleted)
        --aa.deleted;
    // check load factor and possibly grow
    else if (++aa.used * GROW_DEN > aa.dim * GROW_NUM)
    {
        aa.grow(ti.key);
        p = aa.findSlotInsert(hash);
        assert(p.empty);
    }

    // update search cache and allocate entry
    aa.firstUsed = min(aa.firstUsed, cast(uint)(p - aa.buckets.ptr));
    p.hash = hash;
    p.entry = allocEntry(aa, pkey);
    // postblit for key
    if (aa.flags & Impl.Flags.keyHasPostblit)
    {
        import rt.lifetime : __doPostblit, unqualify;

        __doPostblit(p.entry, aa.keysz, unqualify(ti.key));
    }
    // return pointer to value
    return p.entry + aa.valoff;
}

/******************************
 * Lookup *pkey in aa.
 * Called only from implementation of (aa[key]) expressions when value is not mutable.
 * Params:
 *      aa = associative array opaque pointer
 *      keyti = TypeInfo for the key
 *      valsz = ignored
 *      pkey = pointer to the key value
 * Returns:
 *      pointer to value if present, null otherwise
 */
extern (C) inout(void)* _aaGetRvalueX(inout AA aa, scope const TypeInfo keyti, const size_t valsz,
    scope const void* pkey)
{
    return _aaInX(aa, keyti, pkey);
}

/******************************
 * Lookup *pkey in aa.
 * Called only from implementation of (key in aa) expressions.
 * Params:
 *      aa = associative array opaque pointer
 *      keyti = TypeInfo for the key
 *      pkey = pointer to the key value
 * Returns:
 *      pointer to value if present, null otherwise
 */
extern (C) inout(void)* _aaInX(inout AA aa, scope const TypeInfo keyti, scope const void* pkey)
{
    if (aa.empty)
        return null;

    immutable hash = calcHash(pkey, aa);
    if (auto p = aa.findSlotLookup(hash, pkey, keyti))
        return p.entry + aa.valoff;
    return null;
}

/// Delete entry scope const AA, return true if it was present
extern (C) bool _aaDelX(AA aa, scope const TypeInfo keyti, scope const void* pkey)
{
    if (aa.empty)
        return false;

    immutable hash = calcHash(pkey, aa);
    if (auto p = aa.findSlotLookup(hash, pkey, keyti))
    {
        // clear entry
        p.hash = HASH_DELETED;
        p.entry = null;

        ++aa.deleted;
        // `shrink` reallocates, and allocating from a finalizer leads to
        // InvalidMemoryError: https://issues.dlang.org/show_bug.cgi?id=21442
        if (aa.length * SHRINK_DEN < aa.dim * SHRINK_NUM && !GC.inFinalizer())
            aa.shrink(keyti);

        return true;
    }
    return false;
}

/// Remove all elements from AA.
extern (C) void _aaClear(AA aa) pure nothrow @safe
{
    if (!aa.empty)
    {
        aa.clear();
    }
}

/// Rehash AA
extern (C) void* _aaRehash(AA* paa, scope const TypeInfo keyti) pure nothrow
{
    AA aa = *paa;
    if (!aa.empty)
        aa.resize(nextpow2(INIT_DEN * aa.length / INIT_NUM));
    return aa;
}

/// Return a GC allocated array of all values
extern (C) inout(void[]) _aaValues(inout AA aa, const size_t keysz, const size_t valsz,
    const TypeInfo tiValueArray) pure nothrow
{
    if (aa.empty)
        return null;

    import rt.lifetime : _d_newarrayU;

    auto res = _d_newarrayU(tiValueArray, aa.length).ptr;
    auto pval = res;

    immutable off = aa.valoff;
    foreach (b; aa.buckets[aa.firstUsed .. $])
    {
        if (!b.filled)
            continue;
        pval[0 .. valsz] = b.entry[off .. valsz + off];
        pval += valsz;
    }
    // postblit is done in object.values
    return (cast(inout(void)*) res)[0 .. aa.length]; // fake length, return number of elements
}

/// Return a GC allocated array of all keys
extern (C) inout(void[]) _aaKeys(inout AA aa, const size_t keysz, const TypeInfo tiKeyArray) pure nothrow
{
    if (aa.empty)
        return null;

    import rt.lifetime : _d_newarrayU;

    auto res = _d_newarrayU(tiKeyArray, aa.length).ptr;
    auto pkey = res;

    foreach (b; aa.buckets[aa.firstUsed .. $])
    {
        if (!b.filled)
            continue;
        pkey[0 .. keysz] = b.entry[0 .. keysz];
        pkey += keysz;
    }
    // postblit is done in object.keys
    return (cast(inout(void)*) res)[0 .. aa.length]; // fake length, return number of elements
}

// opApply callbacks are extern(D)
extern (D) alias dg_t = int delegate(void*);
extern (D) alias dg2_t = int delegate(void*, void*);

/// foreach opApply over all values
extern (C) int _aaApply(AA aa, const size_t keysz, dg_t dg)
{
    if (aa.empty)
        return 0;

    immutable off = aa.valoff;
    foreach (b; aa.buckets)
    {
        if (!b.filled)
            continue;
        if (auto res = dg(b.entry + off))
            return res;
    }
    return 0;
}

/// foreach opApply over all key/value pairs
extern (C) int _aaApply2(AA aa, const size_t keysz, dg2_t dg)
{
    if (aa.empty)
        return 0;

    immutable off = aa.valoff;
    foreach (b; aa.buckets)
    {
        if (!b.filled)
            continue;
        if (auto res = dg(b.entry, b.entry + off))
            return res;
    }
    return 0;
}

/** Construct an associative array of type ti from corresponding keys and values.
 * Called for an AA literal `[k1:v1, k2:v2]`.
 * Params:
 *      ti = TypeInfo for the associative array
 *      keys = array of keys
 *      vals = array of values
 * Returns:
 *      A new associative array opaque pointer, or null if `keys` is empty.
 */
extern (C) Impl* _d_assocarrayliteralTX(const TypeInfo_AssociativeArray ti, void[] keys,
    void[] vals)
{
    assert(keys.length == vals.length);

    immutable keysz = ti.key.tsize;
    immutable valsz = ti.value.tsize;
    immutable length = keys.length;

    if (!length)
        return null;

    auto aa = new Impl(ti, nextpow2(INIT_DEN * length / INIT_NUM));

    void* pkey = keys.ptr;
    void* pval = vals.ptr;
    immutable off = aa.valoff;
    uint actualLength = 0;
    foreach (_; 0 .. length)
    {
        immutable hash = calcHash(pkey, aa);

        auto p = aa.findSlotLookup(hash, pkey, ti.key);
        if (p is null)
        {
            p = aa.findSlotInsert(hash);
            p.hash = hash;
            p.entry = allocEntry(aa, pkey); // move key, no postblit
            aa.firstUsed = min(aa.firstUsed, cast(uint)(p - aa.buckets.ptr));
            actualLength++;
        }
        else if (aa.entryTI && hasDtor(ti.value))
        {
            // destroy existing value before overwriting it
            ti.value.destroy(p.entry + off);
        }
        // set hash and blit value
        auto pdst = p.entry + off;
        pdst[0 .. valsz] = pval[0 .. valsz]; // move value, no postblit

        pkey += keysz;
        pval += valsz;
    }
    aa.used = actualLength;
    return aa;
}

/// compares 2 AAs for equality
extern (C) int _aaEqual(scope const TypeInfo tiRaw, scope const AA aa1, scope const AA aa2)
{
    if (aa1 is aa2)
        return true;

    immutable len = _aaLen(aa1);
    if (len != _aaLen(aa2))
        return false;

    if (!len) // both empty
        return true;

    import rt.lifetime : unqualify;

    auto uti = unqualify(tiRaw);
    auto ti = *cast(TypeInfo_AssociativeArray*)&uti;
    // compare the entries
    immutable off = aa1.valoff;
    foreach (b1; aa1.buckets)
    {
        if (!b1.filled)
            continue;
        auto pb2 = aa2.findSlotLookup(b1.hash, b1.entry, ti.key);
        if (pb2 is null || !ti.value.equals(b1.entry + off, pb2.entry + off))
            return false;
    }
    return true;
}

/// compute a hash
extern (C) hash_t _aaGetHash(scope const AA* paa, scope const TypeInfo tiRaw) nothrow
{
    const AA aa = *paa;

    if (aa.empty)
        return 0;

    import rt.lifetime : unqualify;

    auto uti = unqualify(tiRaw);
    auto ti = *cast(TypeInfo_AssociativeArray*)&uti;
    immutable off = aa.valoff;
    auto keyHash = &ti.key.getHash;
    auto valHash = &ti.value.getHash;

    size_t h;
    foreach (b; aa.buckets)
    {
        // use addition here, so that hash is independent of element order
        if (b.filled)
            h += hashOf(valHash(b.entry + off), keyHash(b.entry));
    }

    return h;
}

/**
 * _aaRange implements a ForwardRange
 */
struct Range
{
    Impl* impl;
    size_t idx;
    alias impl this;
}

extern (C) pure nothrow @nogc @safe
{
    Range _aaRange(return scope AA aa)
    {
        if (!aa)
            return Range();

        foreach (i; aa.firstUsed .. aa.dim)
        {
            if (aa.buckets[i].filled)
                return Range(aa, i);
        }
        return Range(aa, aa.dim);
    }

    bool _aaRangeEmpty(Range r)
    {
        return r.impl is null || r.idx >= r.dim;
    }

    void* _aaRangeFrontKey(Range r)
    {
        assert(!_aaRangeEmpty(r));
        if (r.idx >= r.dim)
            return null;
        return r.buckets[r.idx].entry;
    }

    void* _aaRangeFrontValue(Range r)
    {
        assert(!_aaRangeEmpty(r));
        if (r.idx >= r.dim)
            return null;

        auto entry = r.buckets[r.idx].entry;
        return entry is null ?
            null :
            (() @trusted { return entry + r.valoff; } ());
    }

    void _aaRangePopFront(ref Range r)
    {
        if (r.idx >= r.dim) return;
        for (++r.idx; r.idx < r.dim; ++r.idx)
        {
            if (r.buckets[r.idx].filled)
                break;
        }
    }
}

// Most tests are now in test_aa.d

// test postblit for AA literals
unittest
{
    static struct T
    {
        ubyte field;
        static size_t postblit, dtor;
        this(this)
        {
            ++postblit;
        }

        ~this()
        {
            ++dtor;
        }
    }

    T t;
    auto aa1 = [0 : t, 1 : t];
    assert(T.dtor == 0 && T.postblit == 2);
    aa1[0] = t;
    assert(T.dtor == 1 && T.postblit == 3);

    T.dtor = 0;
    T.postblit = 0;

    auto aa2 = [0 : t, 1 : t, 0 : t]; // literal with duplicate key => value overwritten
    assert(T.dtor == 1 && T.postblit == 3);

    T.dtor = 0;
    T.postblit = 0;

    auto aa3 = [t : 0];
    assert(T.dtor == 0 && T.postblit == 1);
    aa3[t] = 1;
    assert(T.dtor == 0 && T.postblit == 1);
    aa3.remove(t);
    assert(T.dtor == 0 && T.postblit == 1);
    aa3[t] = 2;
    assert(T.dtor == 0 && T.postblit == 2);

    // dtor will be called by GC finalizers
    aa1 = null;
    aa2 = null;
    aa3 = null;
    GC.runFinalizers((cast(char*)(&entryDtor))[0 .. 1]);
    assert(T.dtor == 6 && T.postblit == 2);
}

// Ensure the newaa struct layout (used for static initialization) is in sync
unittest
{
    import newaa = core.internal.newaa;
    static assert(newaa.Impl.sizeof == Impl.sizeof);
    // ensure compatible types and offsets
    static foreach (i; 0 .. Impl.tupleof.length)
    {
        // for bucket array and Flags, we "compatible" types, not exactly the same types.
        static if (__traits(identifier, Impl.tupleof[i]) == "buckets"
            || __traits(identifier, Impl.tupleof[i]) == "flags")
            static assert(Impl.tupleof[i].sizeof == newaa.Impl.tupleof[i].sizeof);
        else
            static assert(is(typeof(Impl.tupleof[i]) == typeof(newaa.Impl.tupleof[i])));

        static assert(Impl.tupleof[i].offsetof == newaa.Impl.tupleof[i].offsetof);
    }
}

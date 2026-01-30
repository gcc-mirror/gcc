/**
 * template implementation of associative arrays.
 *
 * Copyright: Copyright Digital Mars 2000 - 2015, Steven Schveighoffer 2022.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak, Steven Schveighoffer, Rainer Schuetze
 *
 * Source: $(DRUNTIMESRC core/internal/_newaa.d)
 *
 * derived from rt/aaA.d
 */
module core.internal.newaa;

/// AA version for debuggers, bump whenever changing the layout
immutable int _aaVersion = 1;

import core.internal.util.math : min, max;
import core.internal.traits : substInout;

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

/// AA wrapper
struct AA(K, V)
{
    Impl!(K,V)* impl;
    alias impl this;

    @property bool empty() const pure nothrow @nogc @safe
    {
        pragma(inline, true);
        return impl is null || !impl.length;
    }
    @property size_t length() const pure nothrow @nogc @safe
    {
        pragma(inline, true);
        return impl is null ? 0 : impl.length;
    }
}

/// like core.internal.traits.Unconst, but stripping inout, too
private template Unconstify(T : const U, U)
{
    static if (is(U == inout V, V))
        alias Unconstify = V;
    else
        alias Unconstify = U;
}

ref _refAA(K, V)(ref V[K] aa) @trusted
{
    pragma(inline, true);
    return *(cast(AA!(substInout!K, substInout!V)*)&aa);
}

auto _toAA(K, V)(const V[K] aa) @trusted
{
    pragma(inline, true);
    return *(cast(const(AA!(K, V))*)&aa);
}

auto _toAA(K, V)(inout V[K] aa) @trusted
{
    pragma(inline, true);
    return *(cast(inout(AA!(K, V))*)&aa);
}

// for backward compatibility, but should be deprecated
auto _toAA(K, V)(shared const V[K] aa) @trusted
{
    pragma(inline, true);
    return *(cast(AA!(K, V)*)&aa);
}

// for backward compatibility, but should be deprecated
auto _toAA(K, V)(shared V[K] aa) @trusted
{
    pragma(inline, true);
    return *(cast(AA!(K, V)*)&aa);
}

// resolve ambiguity for immutable converting to const and shared const
auto _toAA(K, V)(immutable V[K] aa) @trusted
{
    pragma(inline, true);
    return *(cast(AA!(K, V)*)&aa);
}

static struct Entry(K, V)
{
    K key;
    V value;
}

// backward compatibility conversions
private ref compat_key(K, K2)(ref K2 key)
{
    pragma(inline, true);
    static if (is(K2 == const(char)[]) && is(K == string))
        return (ref (ref return K2 k2) @trusted => *cast(string*)&k2)(key);
    else
        return key;
}

private void _aaMove(V)(ref V src, ref V dst) @trusted
{
    import core.stdc.string : memcpy, memset;
    // move without postblit!?
    memcpy(&dst, &src, V.sizeof);
    static if (__traits(isZeroInit, V))
        memset(&src, 0, V.sizeof);
    else
        memcpy(&src, &V.init, V.sizeof);
}

// mimick behaviour of rt.aaA for initialization
Entry!(K, V)* _newEntry(K, V)(ref K key, ref V value)
{
    static if (__traits(compiles, new Entry!(K, V)(key, value)))
    {
        auto entry = new Entry!(K, V)(key, value);
    }
    else static if (__traits(compiles, { K k; new Entry!(K, V)(k); }))
    {
        auto entry = new Entry!(K, V)(key);
        _aaMove(value, entry.value);
    }
    else
    {
        auto entry = new Entry!(K, V);
        _aaMove(key, entry.key);
        _aaMove(value, entry.value);
    }
    return entry;
}

// mimick behaviour of rt.aaA for initialization
Entry!(K, V)* _newEntry(K, V, K2)(ref K2 key)
{
    static if (__traits(compiles, new Entry!(K, V)(key)) &&
               !(is(V == struct) && __traits(isNested, V))) // not detected by "compiles"
    {
        auto entry = new Entry!(K, V)(key);
    }
    else static if (__traits(compiles, { K2 k; new Entry!(K, V)(k, V.init); }))
    {
        // with disabled ctor for V
        auto entry = new Entry!(K, V)(key, V.init);
    }
    else
    {
        // with disabled ctor for K and V
        auto entry = new Entry!(K, V);
        entry.key = key;
    }
    static if (!__traits(isZeroInit, V))
    {
        () @trusted { (cast(ubyte*)&entry.value)[0..V.sizeof] = 0; }();
    }
    return entry;
}

template pure_hashOf(K)
{
    static if (__traits(compiles, function hash_t(scope const ref K key) pure nothrow @nogc @trusted { return hashOf(cast()key); }))
    {
        // avoid wrapper call in debug builds if pure nothrow @nogc is inferred
        pragma(inline, true)
        hash_t pure_hashOf(scope const ref K key) @trusted { return hashOf(cast()key); }
    }
    else
    {
        // for backward compatibility, do not require const in hashOf()
        hash_t wrap_hashOf(K)(scope const ref K key) @trusted { return hashOf(cast()key); }
        enum pure_hashOf = cast(hash_t function(scope ref const K key) pure nothrow @nogc @safe) &wrap_hashOf!K;
    }
}

// for backward compatibilty pretend the comparison is @safe, pure, etc
// this also breaks cyclic inference on recursive data types
template pure_keyEqual(K1, K2 = K1)
{
    static if (__traits(compiles, function bool(ref const K1 k1, ref const K2 k2) pure nothrow @nogc @trusted { return cast()k1 == cast()k2; }))
    {
        // avoid wrapper call in debug builds if pure nothrow @nogc is inferred
        pragma(inline, true)
        bool pure_keyEqual(ref const K1 k1, ref const K2 k2) @trusted { return cast()k1 == cast()k2; }
    }
    else
    {
        bool keyEqual(ref const K1 k1, ref const K2 k2) @trusted { return cast()k1 == cast()k2; }
        enum pure_keyEqual = cast(bool function(ref const K1, ref const K2) pure nothrow @nogc @safe) &keyEqual;
    }
}

private struct Impl(K, V)
{
private:
    alias Bucket = .Bucket!(K, V);

    this(size_t sz /* = INIT_NUM_BUCKETS */) nothrow
    {
        buckets = allocBuckets(sz);
        firstUsed = cast(uint) buckets.length;

        // only for binary compatibility
        version(D_TypeInfo)
            entryTI = typeid(Entry!(K, V));
        hashFn = delegate size_t (scope ref const K key) nothrow pure @nogc @safe {
            return pure_hashOf!K(key);
        };

        keysz = cast(uint) K.sizeof;
        valsz = cast(uint) V.sizeof;
        valoff = cast(uint) talign(keysz, V.alignof);

        enum ctflags = () {
            import core.internal.traits;
            Impl.Flags flags;
            static if (__traits(hasPostblit, K))
                flags |= flags.keyHasPostblit;
            static if (hasIndirections!K || hasIndirections!V)
                flags |= flags.hasPointers;
            return flags;
        } ();
        flags = ctflags;
    }

    Bucket[] buckets;
    uint used;
    uint deleted;
    const(TypeInfo) entryTI; // only for binary compatibility
    uint firstUsed;
    immutable uint keysz;    // only for binary compatibility
    immutable uint valsz;    // only for binary compatibility
    immutable uint valoff;   // only for binary compatibility
    Flags flags;             // only for binary compatibility
    size_t delegate(scope ref const K) nothrow pure @nogc @safe hashFn;

    enum Flags : ubyte
    {
        none = 0x0,
        keyHasPostblit = 0x1,
        hasPointers = 0x2,
    }

    @property size_t length() const pure nothrow @nogc @safe
    {
        pragma(inline, true);
        assert(used >= deleted);
        return used - deleted;
    }

    @property size_t dim() const pure nothrow @nogc @safe
    {
        pragma(inline, true);
        return buckets.length;
    }

    @property size_t mask() const pure nothrow @nogc @safe
    {
        pragma(inline, true);
        return dim - 1;
    }

    // find the first slot to insert a value with hash
    size_t findSlotInsert(size_t hash) const pure nothrow @nogc @safe
    {
        for (size_t i = hash & mask, j = 1;; ++j)
        {
            if (!buckets[i].filled)
                return i;
            i = (i + j) & mask;
        }
    }

    // lookup a key
    inout(Bucket)* findSlotLookup(K2)(size_t hash, scope ref const K2 key) inout pure @safe nothrow
    {
        for (size_t i = hash & mask, j = 1;; ++j)
        {
            auto b = &buckets[i]; // avoid multiple bounds checks
            if (b.hash == hash && b.entry)
                if (pure_keyEqual!(K2, K)(key, b.entry.key))
                    return b;
            if (b.empty)
                return null;
            i = (i + j) & mask;
        }
    }

    void grow() pure nothrow @safe
    {
        // If there are so many deleted entries, that growing would push us
        // below the shrink threshold, we just purge deleted entries instead.
        if (length * SHRINK_DEN < GROW_FAC * dim * SHRINK_NUM)
            resize(dim);
        else
            resize(GROW_FAC * dim);
    }

    void shrink() pure nothrow @safe
    {
        if (dim > INIT_NUM_BUCKETS)
            resize(dim / GROW_FAC);
    }

    void resize(size_t ndim) pure nothrow @safe
    {
        auto obuckets = buckets;
        buckets = allocBuckets(ndim);

        foreach (ref b; obuckets[firstUsed .. $])
            if (b.filled)
                buckets[findSlotInsert(b.hash)] = b;

        firstUsed = 0;
        used -= deleted;
        deleted = 0;
        obuckets.length = 0; // safe to free b/c impossible to reference, but doesn't really free
    }

    void clear() pure nothrow
    {
        // clear all data, but don't change bucket array length
        buckets[firstUsed .. $] = Bucket.init;
        deleted = used = 0;
        firstUsed = cast(uint) dim;
    }

    size_t calcHash(K2)(ref K2 key) const nothrow pure @nogc @safe
    {
        static if(is(K2* : K*)) // ref compatible?
            hash_t hash = pure_hashOf!K(key);
        else
            hash_t hash = pure_hashOf!K2(key);
        // highest bit is set to distinguish empty/deleted from filled buckets
        return mix(hash) | HASH_FILLED_MARK;
    }

    static Bucket[] allocBuckets(size_t dim) pure nothrow @safe
    {
        // could allocate with BlkAttr.NO_INTERIOR, but that does not combine
        //  well with arrays and type info for precise scanning
        return new Bucket[dim];
    }
}

//==============================================================================
// Bucket
//------------------------------------------------------------------------------

private struct Bucket(K, V)
{
private pure nothrow @nogc:
    size_t hash;
    Entry!(K, V)* entry;

    @property bool empty() const
    {
        pragma(inline, true);
        return hash == HASH_EMPTY;
    }

    @property bool deleted() const
    {
        pragma(inline, true);
        return hash == HASH_DELETED;
    }

    @property bool filled() const @safe
    {
        pragma(inline, true);
        return cast(ptrdiff_t) hash < 0;
    }
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

private size_t nextpow2(const size_t n) pure nothrow @nogc @safe
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
 * Returns:
 *      A new associative array.
 * Note:
 *  not supported in CTFE
 */
V[K] _d_aaNew(K, V)()
{
    AA!(K, V) aa;
    aa.impl = new Impl!(K,V)(INIT_NUM_BUCKETS);
    return *cast(V[K]*)&aa;
}

/// Determine number of entries in associative array.
/// Note:
///  emulated by the compiler during CTFE
size_t _d_aaLen(K, V)(inout V[K] a)
{
    auto aa = _toAA!(K, V)(a);
    return aa ? aa.length : 0;
}

/******************************
 * Lookup key in aa.
 * Called only from implementation of (aa[key]) expressions when value is mutable.
 * Params:
 *      aa = associative array
 *      key = reference to the key value
 *      found = returns whether the key was found or a new entry was added
 * Returns:
 *      if key was in the aa, a mutable pointer to the existing value.
 *      If key was not in the aa, a mutable pointer to newly inserted value which
 *      is set to zero
 */
V* _d_aaGetY(K, V, T : V1[K1], K1, V1, K2)(auto ref scope T aa, auto ref K2 key, out bool found)
{
    ref aax = cast(V[K])cast(V1[K1])aa; // remove outer const from T
    return _aaGetX!(K, V)(aax, key, found);
}

/******************************
 * Lookup key in aa.
 * Called only from implementation of require, update and _d_aaGetY
 * Params:
 *      a = associative array
 *      key = reference to the key value
 *      found = true if the value was found
 * Returns:
 *      if key was in the aa, a mutable pointer to the existing value.
 *      If key was not in the aa, a mutable pointer to newly inserted value which
 *      is set to V.init
 */
V* _aaGetX(K, V, K2)(auto ref scope V[K] a, auto ref K2 key, out bool found)
{
    ref aa = _refAA!(K, V)(a);

    // lazily alloc implementation
    if (aa is null)
    {
        aa.impl = new Impl!(K, V)(INIT_NUM_BUCKETS);
    }

    ref key2 = compat_key!(K)(key);

    // get hash and bucket for key
    immutable hash = aa.calcHash(key2);

    // found a value => return it
    if (auto p = aa.findSlotLookup(hash, key2))
    {
        found = true;
        return &p.entry.value;
    }

    auto pi = aa.findSlotInsert(hash);
    if (aa.buckets[pi].deleted)
        --aa.deleted;
    // check load factor and possibly grow
    else if (++aa.used * GROW_DEN > aa.dim * GROW_NUM)
    {
        aa.grow();
        pi = aa.findSlotInsert(hash);
        assert(aa.buckets[pi].empty);
    }

    // update search cache and allocate entry
    aa.firstUsed = min(aa.firstUsed, cast(uint)pi);
    ref p = aa.buckets[pi];
    p.hash = hash;
    p.entry = _newEntry!(K, V)(key2);
    return &p.entry.value;
}

/******************************
 * Lookup key in aa.
 * Called only from implementation of (aa[key]) expressions when value is not mutable.
 * Params:
 *      aa = associative array
 *      key = key value
 * Returns:
 *      pointer to value if present, null otherwise
 */
auto _d_aaGetRvalueX(K, V, K2)(inout V[K] aa, auto ref scope K2 key)
{
    return _d_aaIn(aa, key);
}

/// ditto
auto _d_aaGetRvalueX(K, V, K2)(shared(V[K]) aa, auto ref scope K2 key)
{
    // accept shared for backward compatibility, should be deprecated
    return cast(shared(V)*)_d_aaIn(cast(V[K]) aa, key);
}

/// ditto
auto _d_aaGetRvalueX(K, V, K2)(shared const(V[K]) aa, auto ref scope K2 key)
{
    // accept shared for backward compatibility, should be deprecated
    return cast(const shared(V)*)_d_aaIn(cast(V[K]) aa, key);
}

/// ditto
auto _d_aaGetRvalueX(K, V, K2)(immutable(V[K]) aa, auto ref scope K2 key)
{
    // resolve ambiguity for immutable converting to const and shared const
    return _d_aaIn((() @trusted => cast(V[K]) aa) (), key);
}

/***********************************
 * Creates a new associative array of the same size and copies the contents of
 * the associative array into it.
 * Params:
 *      a =     The associative array.
 */
auto _aaDup(T : V[K], K, V)(T a)
{
    auto aa = _toAA!(K, V)(a);
    immutable len = aa.length;
    if (len == 0)
        return null;

    auto impl = new Impl!(K, V)(aa.dim);
    // copy the entries
    bool sameHash = aa.hashFn == impl.hashFn; // can be different if coming from template/rt
    foreach (b; aa.buckets[aa.firstUsed .. $])
    {
        if (!b.filled)
            continue;
        hash_t hash = sameHash ? b.hash : impl.calcHash(b.entry.key);
        auto pi = impl.findSlotInsert(hash);
        auto p = &impl.buckets[pi];
        p.hash = hash;
        p.entry = new Entry!(K, V)(b.entry.key, b.entry.value);
        impl.firstUsed = min(impl.firstUsed, cast(uint)pi);
    }
    impl.used = cast(uint) len;
    return () @trusted { return *cast(Unconstify!V[K]*)&impl; }();
}

/******************************
 * Lookup key in aa.
 * Called only from implementation of (key in aa) expressions.
 * Params:
 *      a = associative array opaque pointer
 *      key = reference to the key value
 * Returns:
 *      pointer to value if present, null otherwise
 */
auto _d_aaIn(T : V[K], K, V, K2)(inout T a, auto ref scope K2 key)
{
    auto aa = _toAA!(K, V)(a);
    if (aa.empty)
        return null;

    ref key2 = compat_key!(K)(key);

    immutable hash = aa.calcHash(key2);
    if (auto p = aa.findSlotLookup(hash, key2))
        return &p.entry.value;
    return null;
}

// fake purity for backward compatibility with runtime hooks
private extern(C) bool gc_inFinalizer() pure nothrow @safe;

/// Delete entry scope const AA, return true if it was present
auto _d_aaDel(T : V[K], K, V, K2)(T a, auto ref K2 key)
{
    auto aa = _toAA!(K, V)(a);
    if (aa.empty)
        return false;

    ref key2 = compat_key!(K)(key);

    immutable hash = aa.calcHash(key2);
    if (auto p = aa.findSlotLookup(hash, key2))
    {
        // clear entry
        p.hash = HASH_DELETED;
        p.entry = null;

        ++aa.deleted;
        // `shrink` reallocates, and allocating from a finalizer leads to
        // InvalidMemoryError: https://issues.dlang.org/show_bug.cgi?id=21442
        if (aa.length * SHRINK_DEN < aa.dim * SHRINK_NUM && !__ctfe && !gc_inFinalizer())
            aa.shrink();

        return true;
    }
    return false;
}

/// Remove all elements from AA.
void _aaClear(K, V)(V[K] a)
{
    auto aa = _toAA!(K, V)(a);
    if (!aa.empty)
    {
        aa.clear();
    }
}

/// Rehash AA
V[K] _aaRehash(K, V)(V[K] a)
{
    auto aa = _toAA!(K, V)(a);
    if (!aa.empty)
        aa.resize(nextpow2(INIT_DEN * aa.length / INIT_NUM));
    return a;
}

/// Return a GC allocated array of all values
auto _aaValues(K, V)(inout V[K] a)
{
    auto aa = _toAA!(K, V)(a);
    if (aa.empty)
        return null;

    static if (__traits(compiles, { V val = aa.buckets[0].entry.value; } ))
        V[] res; // if value has no const indirections
    else
        typeof([aa.buckets[0].entry.value]) res; // as mutable as it can get
    res = new typeof(res[0])[aa.length];

    if (false) // never execute, but infer function attributes from this operation
        res ~= aa.buckets[0].entry.value;

    size_t i = 0;
    foreach (b; aa.buckets[aa.firstUsed .. $])
    {
        if (!b.filled)
            continue;
        import core.lifetime;
        () @trusted { copyEmplace(b.entry.value, res[i++]); }();
    }
    return res;
}

/// Return a GC allocated array of all keys
auto _aaKeys(K, V)(inout V[K] a)
{
    auto aa = _toAA!(K, V)(a);
    if (aa.empty)
        return null;

    static if (__traits(compiles, { K key = aa.buckets[0].entry.key; } ))
        K[] res; // if key has no const indirections
    else
        typeof([aa.buckets[0].entry.key]) res; // as mutable as it can get
    res = new typeof(res[0])[aa.length];

    if (false) // never execute, but infer function attributes from this operation
        res ~= aa.buckets[0].entry.key;

    size_t i = 0;
    foreach (b; aa.buckets[aa.firstUsed .. $])
    {
        if (!b.filled)
            continue;
        // res ~= b.entry.key;
        import core.lifetime;
        () @trusted { copyEmplace(b.entry.key, res[i++]); }();
    }
    return res;
}

/// foreach opApply over all values
/// Note:
///  emulated by the compiler during CTFE
int _d_aaApply(K, V, DG)(inout V[K] a, DG dg)
{
    auto aa = () @trusted { return cast(AA!(K, V))_toAA!(K, V)(a); }();
    if (aa.empty)
        return 0;

    foreach (b; aa.buckets)
    {
        if (!b.filled)
            continue;
        if (auto res = dg(b.entry.value))
            return res;
    }
    return 0;
}

int _d_aaApply(K, V, DG)(shared V[K] a, DG dg)
{
    return _d_aaApply!(K, V, DG)(cast(V[K]) a, dg);
}

int _d_aaApply(K, V, DG)(shared const V[K] a, DG dg)
{
    return _d_aaApply!(K, V, DG)(cast(const V[K]) a, dg);
}

int _d_aaApply(K, V, DG)(immutable V[K] a, DG dg)
{
    return _d_aaApply!(K, V, DG)(cast(const V[K]) a, dg);
}

/// foreach opApply over all key/value pairs
/// Note:
///  emulated by the compiler during CTFE
int _d_aaApply2(K, V, DG)(inout V[K] a, DG dg)
{
    auto aa = () @trusted { return cast(AA!(K, V))_toAA!(K, V)(a); }();
    if (aa.empty)
        return 0;

    foreach (b; aa.buckets)
    {
        if (!b.filled)
            continue;
        if (auto res = dg(b.entry.key, b.entry.value))
            return res;
    }
    return 0;
}

int _d_aaApply2(K, V, DG)(shared V[K] a, DG dg)
{
    return _d_aaApply2!(K, V, DG)(cast(V[K]) a, dg);
}

int _d_aaApply2(K, V, DG)(shared const V[K] a, DG dg)
{
    return _d_aaApply2!(K, V, DG)(cast(const V[K]) a, dg);
}

int _d_aaApply2(K, V, DG)(immutable V[K] a, DG dg)
{
    return _d_aaApply2!(K, V, DG)(cast(const V[K]) a, dg);
}

/** Construct an associative array of type ti from corresponding keys and values.
 * Called for an AA literal `[k1:v1, k2:v2]`.
 * Params:
 *      keys = array of keys
 *      vals = array of values
 * Returns:
 *      A new associative array opaque pointer, or null if `keys` is empty.
 */
Impl!(K, V)* _d_assocarrayliteralTX(K, V)(K[] keys, V[] vals)
{
    assert(keys.length == vals.length);

    immutable length = keys.length;

    if (!length)
        return null;

    auto aa = new Impl!(K, V)(nextpow2(INIT_DEN * length / INIT_NUM));
    size_t duplicates = 0;
    foreach (i; 0 .. length)
    {
        immutable hash = aa.calcHash(keys[i]);

        auto p = aa.findSlotLookup!K(hash, keys[i]);
        if (p)
        {
            static if (__traits(compiles, p.entry.value = vals[i])) // immutable?
                p.entry.value = vals[i];
            else
                p.entry = _newEntry!(K, V)(keys[i], vals[i]);
            duplicates++;
            continue;
        }
        auto pi = aa.findSlotInsert(hash);
        p = &aa.buckets[pi];
        p.hash = hash;
        p.entry = _newEntry!(K, V)(keys[i], vals[i]); // todo: move key and value?
        aa.firstUsed = min(aa.firstUsed, cast(uint)pi);
    }
    aa.used = cast(uint) (length - duplicates);
    return aa;
}

/// compares 2 AAs for equality
bool _aaEqual(T : AA!(K, V), K, V)(scope T aa1, scope T aa2)
{
    if (aa1 is aa2)
        return true;

    immutable len = aa1.length;
    if (len != aa2.length)
        return false;

    if (!len) // both empty
        return true;

    bool sameHash = aa1.hashFn == aa2.hashFn; // can be different if coming from template/rt
    // compare the entries
    foreach (b1; aa1.buckets[aa1.firstUsed .. $])
    {
        if (!b1.filled)
            continue;
        hash_t hash = sameHash ? b1.hash : aa2.calcHash(b1.entry.key);
        auto pb2 = aa2.findSlotLookup!K(hash, b1.entry.key);
        if (pb2 is null || !pure_keyEqual!(V, V)(b1.entry.value, pb2.entry.value)) // rarely, inference on opEqual breaks builds here
            return false;
    }
    return true;
}

/// compares 2 AAs for equality (compiler hook)
bool _d_aaEqual(K, V)(scope const V[K] a1, scope const V[K] a2)
{
    scope aa1 = _toAA!(K, V)(a1);
    scope aa2 = _toAA!(K, V)(a2);
    return _aaEqual(aa1, aa2);
}

/// callback from TypeInfo_AssociativeArray.equals (ignore const for now)
bool _aaOpEqual(K, V)(scope /* const */ AA!(K, V)* aa1, scope /* const */ AA!(K, V)* aa2)
{
    return _aaEqual(*aa1, *aa2);
}

/// compute a hash callback from TypeInfo_AssociativeArray.xtoHash (ignore scope const for now)
hash_t _aaGetHash(K, V)(/* scope const */ AA!(K, V)* paa)
{
    const aa = *paa;

    if (aa.empty)
        return 0;

    size_t h;
    foreach (b; aa.buckets)
    {
        // use addition here, so that hash is independent of element order
        if (b.filled)
            h += hashOf(pure_hashOf!V(b.entry.value), pure_hashOf!K(b.entry.key));
    }

    return h;
}

/**
 * _aaRange implements a ForwardRange
 */
struct AARange(K, V)
{
    alias Key = substInout!K;
    alias Value = substInout!V;

    Impl!(Key, Value)* impl;
    size_t idx;
    alias impl this;
}

AARange!(K, V) _aaRange(K, V)(V[K] a)
{
    auto aa = _toAA!(K, V)(a);
    if (!aa)
        return AARange!(K, V)();

    foreach (i; aa.firstUsed .. aa.dim)
    {
        if (aa.buckets[i].filled)
            return AARange!(K, V)(aa, i);
    }
    return AARange!(K, V)(aa, aa.dim);
}

bool _aaRangeEmpty(K, V)(AARange!(K, V) r)
{
    return r.impl is null || r.idx >= r.dim;
}

K* _aaRangeFrontKey(K, V)(AARange!(K, V) r)
{
    assert(!_aaRangeEmpty(r));
    if (r.idx >= r.dim)
        return null;
    auto entry = r.buckets[r.idx].entry;
    return entry is null ? null : &r.buckets[r.idx].entry.key;
}

V* _aaRangeFrontValue(K, V)(AARange!(K, V) r)
{
    assert(!_aaRangeEmpty(r));
    if (r.idx >= r.dim)
        return null;

    auto entry = r.buckets[r.idx].entry;
    return entry is null ? null : &r.buckets[r.idx].entry.value;
}

void _aaRangePopFront(K, V)(ref AARange!(K, V) r)
{
    if (r.idx >= r.dim) return;
    for (++r.idx; r.idx < r.dim; ++r.idx)
    {
        if (r.buckets[r.idx].filled)
            break;
    }
}

// test postblit for AA literals
unittest
{
    import core.memory;

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
    assert(T.dtor == 2 && T.postblit == 4);
    aa1[0] = t;
    assert(T.dtor == 3 && T.postblit == 5);

    T.dtor = 0;
    T.postblit = 0;

    auto aa2 = [0 : t, 1 : t, 0 : t]; // literal with duplicate key => value overwritten
    assert(T.dtor == 4 && T.postblit == 6);

    T.dtor = 0;
    T.postblit = 0;

    auto aa3 = [t : 0];
    assert(T.dtor == 1 && T.postblit == 2);
    aa3[t] = 1;
    assert(T.dtor == 1 && T.postblit == 2);
    aa3.remove(t);
    assert(T.dtor == 1 && T.postblit == 2);
    aa3[t] = 2;
    assert(T.dtor == 1 && T.postblit == 3);

    // dtor will be called by GC finalizers
    aa1 = null;
    aa2 = null;
    aa3 = null;
    auto dtor1 = typeid(TypeInfo_AssociativeArray.Entry!(int, T)).xdtor;
    GC.runFinalizers((cast(char*)dtor1)[0 .. 1]);
    auto dtor2 = typeid(TypeInfo_AssociativeArray.Entry!(T, int)).xdtor;
    GC.runFinalizers((cast(char*)dtor2)[0 .. 1]);
    assert(T.dtor == 7 && T.postblit == 3);
}

// create a binary-compatible AA structure that can be used directly as an
// associative array.
// NOTE: this must only be called during CTFE
AA!(K, V) makeAA(K, V)(V[K] src) @trusted
{
    assert(__ctfe, "makeAA Must only be called at compile time");
    // keys and values are cheap operations in CTFE, so just reuse _d_assocarrayliteralTX
    auto impl = _d_assocarrayliteralTX!(K, V)(cast(K[])src.keys, cast(V[])src.values);
    auto aa = AA!(K, V)(impl);
    return aa;
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

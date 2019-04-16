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
import core.internal.traits : allSatisfy;

// If true ensure that positive zero and negative zero have the same hash.
// Historically typeid(float).getHash did this but hashOf(float) did not.
private enum floatCoalesceZeroes = true;
// If true ensure that all NaNs of the same floating point type have the same hash.
// Historically typeid(float).getHash didn't do this but hashOf(float) did.
private enum floatCoalesceNaNs = true;

// If either of the above are true then no struct or array that contains the
// representation of a floating point number may be hashed with `bytesHash`.

@nogc nothrow pure @safe unittest
{
    static if (floatCoalesceZeroes)
        assert(hashOf(+0.0) == hashOf(-0.0)); // Same hash for +0.0 and -0.0.
    static if (floatCoalesceNaNs)
        assert(hashOf(double.nan) == hashOf(-double.nan)); // Same hash for different NaN.
}

private enum hasCallableToHash(T) = __traits(compiles,
    {
        size_t hash = ((T* x) => (*x).toHash())(null);
    });

@nogc nothrow pure @safe unittest
{
    static struct S { size_t toHash() { return 4; } }
    assert(hasCallableToHash!S);
    assert(!hasCallableToHash!(shared const S));
}

private enum isFinalClassWithAddressBasedHash(T) = __traits(isFinalClass, T)
    // Use __traits(compiles, ...) in case there are multiple overloads of `toHash`.
    && __traits(compiles, {static assert(&Object.toHash is &T.toHash);});

@nogc nothrow pure @safe unittest
{
    static class C1 {}
    final static class C2 : C1 {}
    final static class C3 : C1 { override size_t toHash() const nothrow { return 1; }}
    static assert(!isFinalClassWithAddressBasedHash!Object);
    static assert(!isFinalClassWithAddressBasedHash!C1);
    static assert(isFinalClassWithAddressBasedHash!C2);
    static assert(!isFinalClassWithAddressBasedHash!C3);
}

/+
Is it valid to calculate a hash code for T based on the bits of its
representation? Always false for interfaces, dynamic arrays, and
associative arrays. False for all classes except final classes that do
not override `toHash`.

Note: according to the spec as of
https://github.com/dlang/dlang.org/commit/d66eff16491b0664c0fc00ba80a7aa291703f1f2
the contents of unnamed paddings between fields is undefined. Currently
this hashing implementation assumes that the padding contents (if any)
for all instances of `T` are the same. The correctness of this
assumption is yet to be verified.
+/
private template canBitwiseHash(T)
{
    static if (is(T EType == enum))
        enum canBitwiseHash = .canBitwiseHash!EType;
    else static if (__traits(isFloating, T))
        enum canBitwiseHash = !(floatCoalesceZeroes || floatCoalesceNaNs);
    else static if (__traits(isScalar, T))
        enum canBitwiseHash = true;
    else static if (is(T == class))
    {
        enum canBitwiseHash = isFinalClassWithAddressBasedHash!T;
    }
    else static if (is(T == interface))
    {
        enum canBitwiseHash = false;
    }
    else static if (is(T == struct))
    {
        static if (hasCallableToHash!T || __traits(isNested, T))
            enum canBitwiseHash = false;
        else
            enum canBitwiseHash = allSatisfy!(.canBitwiseHash, typeof(T.tupleof));
    }
    else static if (is(T == union))
    {
        // Right now we always bytewise hash unions that lack callable `toHash`.
        enum canBitwiseHash = !hasCallableToHash!T;
    }
    else static if (is(T E : E[]))
    {
        static if (__traits(isStaticArray, T))
            enum canBitwiseHash = (T.length == 0) || .canBitwiseHash!E;
        else
            enum canBitwiseHash = false;
    }
    else static if (__traits(isAssociativeArray, T))
    {
        enum canBitwiseHash = false;
    }
    else
    {
        static assert(is(T == delegate) || is(T : void) || is(T : typeof(null)),
            "Internal error: unanticipated type "~T.stringof);
        enum canBitwiseHash = true;
    }
}

// Overly restrictive for simplicity: has false negatives but no false positives.
private template useScopeConstPassByValue(T)
{
    static if (__traits(isScalar, T))
        enum useScopeConstPassByValue = true;
    else static if (is(T == class) || is(T == interface))
        // Overly restrictive for simplicity.
        enum useScopeConstPassByValue = isFinalClassWithAddressBasedHash!T;
    else static if (is(T == struct) || is(T == union))
    {
        // Overly restrictive for simplicity.
        enum useScopeConstPassByValue = T.sizeof <= (int[]).sizeof &&
            __traits(isPOD, T) && // "isPOD" just to check there's no dtor or postblit.
            canBitwiseHash!T; // We can't verify toHash doesn't leak.
    }
    else static if (is(T : E[], E))
    {
        static if (!__traits(isStaticArray, T))
            // Overly restrictive for simplicity.
            enum useScopeConstPassByValue = .useScopeConstPassByValue!E;
        else static if (T.length == 0)
            enum useScopeConstPassByValue = true;
        else
            enum useScopeConstPassByValue = T.sizeof <= (uint[]).sizeof
                && .useScopeConstPassByValue!(typeof(T.init[0]));
    }
    else static if (is(T : V[K], K, V))
    {
        // Overly restrictive for simplicity.
        enum useScopeConstPassByValue = .useScopeConstPassByValue!K
            && .useScopeConstPassByValue!V;
    }
    else
    {
        static assert(is(T == delegate) || is(T : void) || is(T : typeof(null)),
            "Internal error: unanticipated type "~T.stringof);
        enum useScopeConstPassByValue = true;
    }
}

@safe unittest
{
    static assert(useScopeConstPassByValue!int);
    static assert(useScopeConstPassByValue!string);

    static int ctr;
    static struct S1 { ~this() { ctr++; } }
    static struct S2 { this(this) { ctr++; } }
    static assert(!useScopeConstPassByValue!S1,
        "Don't default pass by value a struct with a non-vacuous destructor.");
    static assert(!useScopeConstPassByValue!S2,
        "Don't default pass by value a struct with a non-vacuous postblit.");
}

//enum hash. CTFE depends on base type
size_t hashOf(T)(scope const T val)
if (is(T EType == enum) && useScopeConstPassByValue!EType)
{
    static if (is(T EType == enum)) //for EType
    {
        return hashOf(cast(const EType) val);
    }
    else
    {
        static assert(0);
    }
}

//enum hash. CTFE depends on base type
size_t hashOf(T)(scope const T val, size_t seed)
if (is(T EType == enum) && useScopeConstPassByValue!EType)
{
    static if (is(T EType == enum)) //for EType
    {
        return hashOf(cast(const EType) val, seed);
    }
    else
    {
        static assert(0);
    }
}

//enum hash. CTFE depends on base type
size_t hashOf(T)(auto ref T val, size_t seed = 0)
if (is(T EType == enum) && !useScopeConstPassByValue!EType)
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

//CTFE ready (depends on base type).
size_t hashOf(T)(scope const auto ref T val, size_t seed = 0)
if (!is(T == enum) && __traits(isStaticArray, T) && canBitwiseHash!T)
{
    // FIXME:
    // We would like to to do this:
    //
    //static if (T.length == 0)
    //    return seed;
    //else static if (T.length == 1)
    //    return hashOf(val[0], seed);
    //else
    //    return bytesHashWithExactSizeAndAlignment!T(toUbyte(val), seed);
    //
    // ... but that's inefficient when using a runtime TypeInfo (introduces a branch)
    // and PR #2243 wants typeid(T).getHash(&val) to produce the same result as
    // hashOf(val).
    static if (T.length == 0)
    {
        return bytesHashAlignedBy!size_t((ubyte[]).init, seed);
    }
    static if (is(typeof(toUbyte(val)) == const(ubyte)[]))
    {
        return bytesHashAlignedBy!T(toUbyte(val), seed);
    }
    else //Other types. CTFE unsupported
    {
        assert(!__ctfe, "unable to compute hash of "~T.stringof~" at compile time");
        return bytesHashAlignedBy!T((cast(const(ubyte)*) &val)[0 .. T.sizeof], seed);
    }
}

//CTFE ready (depends on base type).
size_t hashOf(T)(auto ref T val, size_t seed = 0)
if (!is(T == enum) && __traits(isStaticArray, T) && !canBitwiseHash!T)
{
    // FIXME:
    // We would like to to do this:
    //
    //static if (T.length == 0)
    //    return seed;
    //else static if (T.length == 1)
    //    return hashOf(val[0], seed);
    //else
    //    /+ hash like a dynamic array +/
    //
    // ... but that's inefficient when using a runtime TypeInfo (introduces a branch)
    // and PR #2243 wants typeid(T).getHash(&val) to produce the same result as
    // hashOf(val).
    return hashOf(val[], seed);
}

//dynamic array hash
size_t hashOf(T)(scope const T val, size_t seed = 0)
if (!is(T == enum) && !is(T : typeof(null)) && is(T S: S[]) && !__traits(isStaticArray, T)
    && !is(T == struct) && !is(T == class) && !is(T == union)
    && (__traits(isScalar, S) || canBitwiseHash!S))
{
    alias ElementType = typeof(val[0]);
    static if (!canBitwiseHash!ElementType)
    {
        size_t hash = seed;
        foreach (ref o; val)
        {
            hash = hashOf(hashOf(o), hash); // double hashing to match TypeInfo.getHash
        }
        return hash;
    }
    else static if (is(typeof(toUbyte(val)) == const(ubyte)[]))
    //ubyteble array (arithmetic types and structs without toHash) CTFE ready for arithmetic types and structs without reference fields
    {
        return bytesHashAlignedBy!ElementType(toUbyte(val), seed);
    }
    else //Other types. CTFE unsupported
    {
        assert(!__ctfe, "unable to compute hash of "~T.stringof~" at compile time");
        return bytesHashAlignedBy!ElementType((cast(const(ubyte)*) val.ptr)[0 .. ElementType.sizeof*val.length], seed);
    }
}

//dynamic array hash
size_t hashOf(T)(T val, size_t seed = 0)
if (!is(T == enum) && !is(T : typeof(null)) && is(T S: S[]) && !__traits(isStaticArray, T)
    && !is(T == struct) && !is(T == class) && !is(T == union)
    && !(__traits(isScalar, S) || canBitwiseHash!S))
{
    size_t hash = seed;
    foreach (ref o; val)
    {
        hash = hashOf(hashOf(o), hash); // double hashing because TypeInfo.getHash doesn't allow to pass seed value
    }
    return hash;
}

//arithmetic type hash
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val) if (!is(T == enum) && __traits(isArithmetic, T)
    && __traits(isIntegral, T) && T.sizeof <= size_t.sizeof && !is(T == __vector))
{
    return val;
}

//arithmetic type hash
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val, size_t seed) if (!is(T == enum) && __traits(isArithmetic, T)
    && __traits(isIntegral, T) && T.sizeof <= size_t.sizeof && !is(T == __vector))
{
    static if (size_t.sizeof < ulong.sizeof)
    {
        //MurmurHash3 32-bit single round
        enum uint c1 = 0xcc9e2d51;
        enum uint c2 = 0x1b873593;
        enum uint c3 = 0xe6546b64;
        enum uint r1 = 15;
        enum uint r2 = 13;
    }
    else
    {
        //Half of MurmurHash3 64-bit single round
        //(omits second interleaved update)
        enum ulong c1 = 0x87c37b91114253d5;
        enum ulong c2 = 0x4cf5ad432745937f;
        enum ulong c3 = 0x52dce729;
        enum uint r1 = 31;
        enum uint r2 = 27;
    }
    size_t h = c1 * val;
    h = (h << r1) | (h >>> (size_t.sizeof * 8 - r1));
    h = (h * c2) ^ seed;
    h = (h << r2) | (h >>> (size_t.sizeof * 8 - r2));
    return h * 5 + c3;
}

//arithmetic type hash
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val, size_t seed = 0) if (!is(T == enum) && __traits(isArithmetic, T)
    && (!__traits(isIntegral, T) || T.sizeof > size_t.sizeof) && !is(T == __vector))
{
    static if (__traits(isFloating, val))
    {
        import core.internal.convert : floatSize;

        static if (floatCoalesceZeroes || floatCoalesceNaNs)
        {
            import core.internal.traits : Unqual;
            Unqual!T data = val;
            // +0.0 and -0.0 become the same.
            static if (floatCoalesceZeroes && is(typeof(data = 0)))
                if (data == 0) data = 0;
            static if (floatCoalesceZeroes && is(typeof(data = 0.0i)))
                if (data == 0.0i) data = 0.0i;
            static if (floatCoalesceZeroes && is(typeof(data = 0.0 + 0.0i)))
            {
                if (data.re == 0.0) data = 0.0 + (data.im * 1.0i);
                if (data.im == 0.0i) data = data.re + 0.0i;
            }
            static if (floatCoalesceNaNs)
                if (data != data) data = T.nan; // All NaN patterns become the same.
        }
        else
        {
            alias data = val;
        }

        static if (T.mant_dig == float.mant_dig && T.sizeof == uint.sizeof)
            return hashOf(*cast(const uint*) &data, seed);
        else static if (T.mant_dig == double.mant_dig && T.sizeof == ulong.sizeof)
            return hashOf(*cast(const ulong*) &data, seed);
        else
        {
            static if (is(T : creal) && T.sizeof != 2 * floatSize!(typeof(T.re)))
            {
                auto h1 = hashOf(data.re);
                return hashOf(data.im, h1);
            }
            else static if (is(T : real) || is(T : ireal))
            {
                // Ignore trailing padding
                auto bytes = toUbyte(data)[0 .. floatSize!T];
                return bytesHashWithExactSizeAndAlignment!T(bytes, seed);
            }
            else
            {
                return bytesHashWithExactSizeAndAlignment!T(toUbyte(data), seed);
            }
        }
    }
    else
    {
        static assert(T.sizeof > size_t.sizeof && __traits(isIntegral, T));
        foreach (i; 0 .. T.sizeof / size_t.sizeof)
            seed = hashOf(cast(size_t) (val >>> (size_t.sizeof * 8 * i)), seed);
        return seed;
    }
}

size_t hashOf(T)(scope const auto ref T val, size_t seed = 0) @safe @nogc nothrow pure
if (is(T == __vector) && !is(T == enum))
{
    static if (__traits(isFloating, T) && (floatCoalesceZeroes || floatCoalesceNaNs))
    {
        if (__ctfe)
        {
            // Workaround for CTFE bug.
            alias E = Unqual!(typeof(val[0]));
            E[T.sizeof / E.sizeof] array;
            foreach (i; 0 .. T.sizeof / E.sizeof)
                array[i] = val[i];
            return hashOf(array, seed);
        }
        return hashOf(val.array, seed);
    }
    else
    {
        return bytesHashAlignedBy!T(toUbyte(val), seed);
    }
}

//typeof(null) hash. CTFE supported
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val) if (!is(T == enum) && is(T : typeof(null)))
{
    return 0;
}

//typeof(null) hash. CTFE supported
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val, size_t seed) if (!is(T == enum) && is(T : typeof(null)))
{
    return hashOf(size_t(0), seed);
}

//Pointers hash. CTFE unsupported if not null
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val)
if (!is(T == enum) && is(T V : V*) && !is(T : typeof(null))
    && !is(T == struct) && !is(T == class) && !is(T == union))
{
    if (__ctfe)
    {
        if (val is null)
        {
            return 0;
        }
        else
        {
            assert(0, "Unable to calculate hash of non-null pointer at compile time");
        }

    }
    auto addr = cast(size_t) val;
    return addr ^ (addr >>> 4);
}

//Pointers hash. CTFE unsupported if not null
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val, size_t seed)
if (!is(T == enum) && is(T V : V*) && !is(T : typeof(null))
    && !is(T == struct) && !is(T == class) && !is(T == union))
{
    if (__ctfe)
    {
        if (val is null)
        {
            return hashOf(cast(size_t)0, seed);
        }
        else
        {
            assert(0, "Unable to calculate hash of non-null pointer at compile time");
        }

    }
    return hashOf(cast(size_t)val, seed);
}

private enum _hashOfStruct =
q{
    enum bool isChained = is(typeof(seed) : size_t);
    static if (!isChained) enum size_t seed = 0;
    static if (hasCallableToHash!T) //CTFE depends on toHash()
    {
        static if (isChained)
            return hashOf(cast(size_t) val.toHash(), seed);
        else
            return val.toHash();
    }
    else
    {
        static if (__traits(hasMember, T, "toHash") && is(typeof(T.toHash) == function))
        {
            pragma(msg, "Warning: struct "~__traits(identifier, T)~" has method toHash, however it cannot be called with "~T.stringof~" this.");
        }

        static if (T.tupleof.length == 0)
        {
            return seed;
        }
        else static if ((is(T == struct) && !canBitwiseHash!T) || T.tupleof.length == 1)
        {
            size_t h = void;
            static if (isChained) h = seed;
            foreach (i, F; typeof(val.tupleof))
            {
                static if (__traits(isStaticArray, F))
                {
                    static if (i == 0 && !isChained) h = 0;
                    static if (F.sizeof > 0 && canBitwiseHash!F)
                        // May use smallBytesHash instead of bytesHash.
                        h = bytesHashWithExactSizeAndAlignment!F(toUbyte(val.tupleof[i]), h);
                    else
                        // We can avoid the "double hashing" the top-level version uses
                        // for consistency with TypeInfo.getHash.
                        foreach (ref e; val.tupleof[i])
                            h = hashOf(e, h);
                }
                else static if (is(F == struct) || is(F == union))
                {
                    static if (hasCallableToHash!F)
                    {
                        static if (i == 0 && !isChained)
                            h = val.tupleof[i].toHash();
                        else
                            h = hashOf(cast(size_t) val.tupleof[i].toHash(), h);
                    }
                    else static if (F.tupleof.length == 1)
                    {
                        // Handle the single member case separately to avoid unnecessarily using bytesHash.
                        static if (i == 0 && !isChained)
                            h = hashOf(val.tupleof[i].tupleof[0]);
                        else
                            h = hashOf(val.tupleof[i].tupleof[0], h);
                    }
                    else static if (canBitwiseHash!F)
                    {
                        // May use smallBytesHash instead of bytesHash.
                        static if (i == 0 && !isChained) h = 0;
                        h = bytesHashWithExactSizeAndAlignment!F(toUbyte(val.tupleof[i]), h);
                    }
                    else
                    {
                        // Nothing special happening.
                        static if (i == 0 && !isChained)
                            h = hashOf(val.tupleof[i]);
                        else
                            h = hashOf(val.tupleof[i], h);
                    }
                }
                else
                {
                    // Nothing special happening.
                    static if (i == 0 && !isChained)
                        h = hashOf(val.tupleof[i]);
                    else
                        h = hashOf(val.tupleof[i], h);
                }
            }
            return h;
        }
        else static if (is(typeof(toUbyte(val)) == const(ubyte)[]))//CTFE ready for structs without reference fields
        {
            // Not using bytesHashWithExactSizeAndAlignment here because
            // the result may differ from typeid(T).hashOf(&val).
            return bytesHashAlignedBy!T(toUbyte(val), seed);
        }
        else // CTFE unsupported
        {
            assert(!__ctfe, "unable to compute hash of "~T.stringof~" at compile time");
            const(ubyte)[] bytes = (() @trusted => (cast(const(ubyte)*)&val)[0 .. T.sizeof])();
            // Not using bytesHashWithExactSizeAndAlignment here because
            // the result may differ from typeid(T).hashOf(&val).
            return bytesHashAlignedBy!T(bytes, seed);
        }
    }
};

//struct or union hash
size_t hashOf(T)(scope const auto ref T val, size_t seed = 0)
if (!is(T == enum) && (is(T == struct) || is(T == union))
    && canBitwiseHash!T)
{
    mixin(_hashOfStruct);
}

//struct or union hash
size_t hashOf(T)(auto ref T val)
if (!is(T == enum) && (is(T == struct) || is(T == union))
    && !canBitwiseHash!T)
{
    mixin(_hashOfStruct);
}

//struct or union hash
size_t hashOf(T)(auto ref T val, size_t seed)
if (!is(T == enum) && (is(T == struct) || is(T == union))
    && !canBitwiseHash!T)
{
    mixin(_hashOfStruct);
}

//delegate hash. CTFE unsupported
@trusted @nogc nothrow pure
size_t hashOf(T)(scope const T val, size_t seed = 0) if (!is(T == enum) && is(T == delegate))
{
    assert(!__ctfe, "unable to compute hash of "~T.stringof~" at compile time");
    const(ubyte)[] bytes = (cast(const(ubyte)*)&val)[0 .. T.sizeof];
    return bytesHashWithExactSizeAndAlignment!T(bytes, seed);
}

//address-based class hash. CTFE only if null.
@nogc nothrow pure @trusted
size_t hashOf(T)(scope const T val)
if (!is(T == enum) && (is(T == interface) || is(T == class))
    && canBitwiseHash!T)
{
    if (__ctfe) if (val is null) return 0;
    return hashOf(cast(const void*) val);
}

//address-based class hash. CTFE only if null.
@nogc nothrow pure @trusted
size_t hashOf(T)(scope const T val, size_t seed)
if (!is(T == enum) && (is(T == interface) || is(T == class))
    && canBitwiseHash!T)
{
    if (__ctfe) if (val is null) return hashOf(size_t(0), seed);
    return hashOf(cast(const void*) val, seed);
}

//class or interface hash. CTFE depends on toHash
size_t hashOf(T)(T val)
if (!is(T == enum) && (is(T == interface) || is(T == class))
    && !canBitwiseHash!T)
{
    static if (__traits(compiles, {size_t h = val.toHash();}))
        return val ? val.toHash() : 0;
    else
        return val ? (cast(Object)val).toHash() : 0;
}

//class or interface hash. CTFE depends on toHash
size_t hashOf(T)(T val, size_t seed)
if (!is(T == enum) && (is(T == interface) || is(T == class))
    && !canBitwiseHash!T)
{
    static if (__traits(compiles, {size_t h = val.toHash();}))
        return hashOf(val ? cast(size_t) val.toHash() : size_t(0), seed);
    else
        return hashOf(val ? (cast(Object)val).toHash() : 0, seed);
}

//associative array hash. CTFE depends on base types
size_t hashOf(T)(T aa) if (!is(T == enum) && __traits(isAssociativeArray, T))
{
    static if (is(typeof(aa) : V[K], K, V)) {} // Put K & V in scope.
    static if (__traits(compiles, (ref K k, ref V v) nothrow => .hashOf(k) + .hashOf(v)))
        scope (failure) assert(0); // Allow compiler to infer nothrow.

    if (!aa.length) return 0;
    size_t h = 0;

    // The computed hash is independent of the foreach traversal order.
    foreach (key, ref val; aa)
    {
        size_t[2] hpair;
        hpair[0] = key.hashOf();
        hpair[1] = val.hashOf();
        h += hpair.hashOf();
    }
    return h;
}

//associative array hash. CTFE depends on base types
size_t hashOf(T)(T aa, size_t seed) if (!is(T == enum) && __traits(isAssociativeArray, T))
{
    return hashOf(hashOf(aa), seed);
}

// MurmurHash3 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.

// This overload is for backwards compatibility.
@system pure nothrow @nogc
size_t bytesHash()(scope const(void)* buf, size_t len, size_t seed)
{
    return bytesHashAlignedBy!ubyte((cast(const(ubyte)*) buf)[0 .. len], seed);
}

private template bytesHashAlignedBy(AlignType)
{
    alias bytesHashAlignedBy = bytesHash!(AlignType.alignof >= uint.alignof);
}

private template bytesHashWithExactSizeAndAlignment(SizeAndAlignType)
{
    static if (SizeAndAlignType.alignof < uint.alignof
            ? SizeAndAlignType.sizeof <= 12
            : SizeAndAlignType.sizeof <= 10)
        alias bytesHashWithExactSizeAndAlignment = smallBytesHash;
    else
        alias bytesHashWithExactSizeAndAlignment = bytesHashAlignedBy!SizeAndAlignType;
}

// Fowler/Noll/Vo hash. http://www.isthe.com/chongo/tech/comp/fnv/
private size_t fnv()(scope const(ubyte)[] bytes, size_t seed) @nogc nothrow pure @safe
{
    static if (size_t.max <= uint.max)
        enum prime = (1U << 24) + (1U << 8) + 0x93U;
    else static if (size_t.max <= ulong.max)
        enum prime = (1UL << 40) + (1UL << 8) + 0xb3UL;
    else
        enum prime = (size_t(1) << 88) + (size_t(1) << 8) + size_t(0x3b);
    foreach (b; bytes)
        seed = (seed ^ b) * prime;
    return seed;
}
private alias smallBytesHash = fnv;

//-----------------------------------------------------------------------------
// Block read - if your platform needs to do endian-swapping or can only
// handle aligned reads, do the conversion here
private uint get32bits()(scope const(ubyte)* x) @nogc nothrow pure @system
{
    version (BigEndian)
    {
        return ((cast(uint) x[0]) << 24) | ((cast(uint) x[1]) << 16) | ((cast(uint) x[2]) << 8) | (cast(uint) x[3]);
    }
    else
    {
        return ((cast(uint) x[3]) << 24) | ((cast(uint) x[2]) << 16) | ((cast(uint) x[1]) << 8) | (cast(uint) x[0]);
    }
}

/+
Params:
    dataKnownToBeAligned = whether the data is known at compile time to be uint-aligned.
+/
@nogc nothrow pure @trusted
private size_t bytesHash(bool dataKnownToBeAligned)(scope const(ubyte)[] bytes, size_t seed)
{
    auto len = bytes.length;
    auto data = bytes.ptr;
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
        static if (dataKnownToBeAligned)
            uint k1 = __ctfe ? get32bits(data) : *(cast(const uint*) data);
        else
            uint k1 = get32bits(data);
        k1 *= c1;
        k1 = (k1 << 15) | (k1 >> (32 - 15));
        k1 *= c2;

        h1 ^= k1;
        h1 = (h1 << 13) | (h1 >> (32 - 13));
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
                k1 *= c1; k1 = (k1 << 15) | (k1 >> (32 - 15)); k1 *= c2; h1 ^= k1;
                goto default;
        default:
    }

    //----------
    // finalization
    h1 ^= len;
    // Force all bits of the hash block to avalanche.
    h1 = (h1 ^ (h1 >> 16)) * 0x85ebca6b;
    h1 = (h1 ^ (h1 >> 13)) * 0xc2b2ae35;
    h1 ^= h1 >> 16;
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

    // Detect unintended changes to bytesHash on unaligned and aligned inputs.
    version (BigEndian)
    {
        const ubyte[7] a = [99, 4, 3, 2, 1, 5, 88];
        const uint[2] b = [0x04_03_02_01, 0x05_ff_ff_ff];
    }
    else
    {
        const ubyte[7] a = [99, 1, 2, 3, 4, 5, 88];
        const uint[2] b = [0x04_03_02_01, 0xff_ff_ff_05];
    }
    // It is okay to change the below values if you make a change
    // that you expect to change the result of bytesHash.
    assert(bytesHash(&a[1], a.length - 2, 0) == 2727459272);
    assert(bytesHash(&b, 5, 0) == 2727459272);
    assert(bytesHashAlignedBy!uint((cast(const ubyte*) &b)[0 .. 5], 0) == 2727459272);
}

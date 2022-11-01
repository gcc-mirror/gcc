// Written in the D programming language.
/**
Utility and ancillary artifacts of `std.experimental.allocator`. This module
shouldn't be used directly; its functionality will be migrated into more
appropriate parts of `std`.

Authors: $(HTTP erdani.com, Andrei Alexandrescu), Timon Gehr (`Ternary`)

Source: $(PHOBOSSRC std/experimental/allocator/common.d)
*/
module std.experimental.allocator.common;
import std.algorithm.comparison, std.traits;

/**
Is `true` iff `A` is an allocator.
 */
enum isAllocator(A) = (is(typeof(A.allocate(size_t.init)) == void[]) && is(typeof(A.alignment) : size_t));

///
@safe @nogc nothrow pure
unittest
{
    import std.experimental.allocator.building_blocks.null_allocator : NullAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mmap_allocator : MmapAllocator;
    static assert(isAllocator!NullAllocator);
    static assert(isAllocator!Mallocator);
    static assert(isAllocator!GCAllocator);
    static assert(isAllocator!MmapAllocator);
    static assert(!isAllocator!int);
}

/**
Returns the size in bytes of the state that needs to be allocated to hold an
object of type `T`. `stateSize!T` is zero for `struct`s that are not
nested and have no nonstatic member variables.
 */
template stateSize(T)
{
    static if (is(T == class) || is(T == interface))
        enum stateSize = __traits(classInstanceSize, T);
    else static if (is(T == struct) || is(T == union))
        enum stateSize = Fields!T.length || isNested!T ? T.sizeof : 0;
    else static if (is(T == void))
        enum size_t stateSize = 0;
    else
        enum stateSize = T.sizeof;
}

@safe @nogc nothrow pure
unittest
{
    static assert(stateSize!void == 0);
    struct A {}
    static assert(stateSize!A == 0);
    struct B { int x; }
    static assert(stateSize!B == 4);
    interface I1 {}
    //static assert(stateSize!I1 == 2 * size_t.sizeof);
    class C1 {}
    static assert(stateSize!C1 == 3 * size_t.sizeof);
    class C2 { char c; }
    static assert(stateSize!C2 == 4 * size_t.sizeof);
    static class C3 { char c; }
    static assert(stateSize!C3 == 2 * size_t.sizeof + char.sizeof);
}

/**
Returns `true` if the `Allocator` has the alignment known at compile time;
otherwise it returns `false`.
 */
template hasStaticallyKnownAlignment(Allocator)
{
    enum hasStaticallyKnownAlignment = __traits(compiles,
                                                {enum x = Allocator.alignment;});
}

/**
`chooseAtRuntime` is a compile-time constant of type `size_t` that several
parameterized structures in this module recognize to mean deferral to runtime of
the exact value. For example, $(D BitmappedBlock!(Allocator, 4096)) (described in
detail below) defines a block allocator with block size of 4096 bytes, whereas
$(D BitmappedBlock!(Allocator, chooseAtRuntime)) defines a block allocator that has a
field storing the block size, initialized by the user.
*/
enum chooseAtRuntime = size_t.max - 1;

/**
`unbounded` is a compile-time constant of type `size_t` that several
parameterized structures in this module recognize to mean "infinite" bounds for
the parameter. For example, `Freelist` (described in detail below) accepts a
`maxNodes` parameter limiting the number of freelist items. If `unbounded`
is passed for `maxNodes`, then there is no limit and no checking for the
number of nodes.
*/
enum unbounded = size_t.max;

/**
The alignment that is guaranteed to accommodate any D object allocation on the
current platform.
*/
enum uint platformAlignment = std.algorithm.comparison.max(double.alignof, real.alignof);

/**
The default good size allocation is deduced as `n` rounded up to the
allocator's alignment.
*/
size_t goodAllocSize(A)(auto ref A a, size_t n)
{
    return n.roundUpToMultipleOf(a.alignment);
}

/*
Returns s rounded up to a multiple of base.
*/
@safe @nogc nothrow pure
package size_t roundUpToMultipleOf(size_t s, uint base)
{
    assert(base);
    auto rem = s % base;
    return rem ? s + base - rem : s;
}

@safe @nogc nothrow pure
unittest
{
    assert(10.roundUpToMultipleOf(11) == 11);
    assert(11.roundUpToMultipleOf(11) == 11);
    assert(12.roundUpToMultipleOf(11) == 22);
    assert(118.roundUpToMultipleOf(11) == 121);
}

/*
Returns `n` rounded up to a multiple of alignment, which must be a power of 2.
*/
@safe @nogc nothrow pure
package size_t roundUpToAlignment(size_t n, uint alignment)
{
    import std.math.traits : isPowerOf2;
    assert(alignment.isPowerOf2);
    immutable uint slack = cast(uint) n & (alignment - 1);
    const result = slack
        ? n + alignment - slack
        : n;
    assert(result >= n);
    return result;
}

@safe @nogc nothrow pure
unittest
{
    assert(10.roundUpToAlignment(4) == 12);
    assert(11.roundUpToAlignment(2) == 12);
    assert(12.roundUpToAlignment(8) == 16);
    assert(118.roundUpToAlignment(64) == 128);
}

/*
Returns `n` rounded down to a multiple of alignment, which must be a power of 2.
*/
@safe @nogc nothrow pure
package size_t roundDownToAlignment(size_t n, uint alignment)
{
    import std.math.traits : isPowerOf2;
    assert(alignment.isPowerOf2);
    return n & ~size_t(alignment - 1);
}

@safe @nogc nothrow pure
unittest
{
    assert(10.roundDownToAlignment(4) == 8);
    assert(11.roundDownToAlignment(2) == 10);
    assert(12.roundDownToAlignment(8) == 8);
    assert(63.roundDownToAlignment(64) == 0);
}

/*
Advances the beginning of `b` to start at alignment `a`. The resulting buffer
may therefore be shorter. Returns the adjusted buffer, or null if obtaining a
non-empty buffer is impossible.
*/
@nogc nothrow pure
package void[] roundUpToAlignment(void[] b, uint a)
{
    auto e = b.ptr + b.length;
    auto p = cast(void*) roundUpToAlignment(cast(size_t) b.ptr, a);
    if (e <= p) return null;
    return p[0 .. e - p];
}

@nogc nothrow pure
@system unittest
{
    void[] empty;
    assert(roundUpToAlignment(empty, 4) == null);
    char[128] buf;
    // At least one pointer inside buf is 128-aligned
    assert(roundUpToAlignment(buf, 128) !is null);
}

/*
Like `a / b` but rounds the result up, not down.
*/
@safe @nogc nothrow pure
package size_t divideRoundUp(size_t a, size_t b)
{
    assert(b);
    return (a + b - 1) / b;
}

/*
Returns `s` rounded up to a multiple of `base`.
*/
@nogc nothrow pure
package void[] roundStartToMultipleOf(void[] s, uint base)
{
    assert(base);
    auto p = cast(void*) roundUpToMultipleOf(
        cast(size_t) s.ptr, base);
    auto end = s.ptr + s.length;
    return p[0 .. end - p];
}

nothrow pure
@system unittest
{
    void[] p;
    assert(roundStartToMultipleOf(p, 16) is null);
    p = new ulong[10];
    assert(roundStartToMultipleOf(p, 16) is p);
}

/*
Returns `s` rounded up to the nearest power of 2.
*/
@safe @nogc nothrow pure
package size_t roundUpToPowerOf2(size_t s)
{
    import std.meta : AliasSeq;
    assert(s <= (size_t.max >> 1) + 1);
    --s;
    static if (size_t.sizeof == 4)
        alias Shifts = AliasSeq!(1, 2, 4, 8, 16);
    else
        alias Shifts = AliasSeq!(1, 2, 4, 8, 16, 32);
    foreach (i; Shifts)
    {
        s |= s >> i;
    }
    return s + 1;
}

@safe @nogc nothrow pure
unittest
{
    assert(0.roundUpToPowerOf2 == 0);
    assert(1.roundUpToPowerOf2 == 1);
    assert(2.roundUpToPowerOf2 == 2);
    assert(3.roundUpToPowerOf2 == 4);
    assert(7.roundUpToPowerOf2 == 8);
    assert(8.roundUpToPowerOf2 == 8);
    assert(10.roundUpToPowerOf2 == 16);
    assert(11.roundUpToPowerOf2 == 16);
    assert(12.roundUpToPowerOf2 == 16);
    assert(118.roundUpToPowerOf2 == 128);
    assert((size_t.max >> 1).roundUpToPowerOf2 == (size_t.max >> 1) + 1);
    assert(((size_t.max >> 1) + 1).roundUpToPowerOf2 == (size_t.max >> 1) + 1);
}

/*
Returns the number of trailing zeros of `x`.
*/
@safe @nogc nothrow pure
package uint trailingZeros(ulong x)
{
    import core.bitop : bsf;
    return x == 0 ? 64 : bsf(x);
}

@safe @nogc nothrow pure
unittest
{
    assert(trailingZeros(0) == 64);
    assert(trailingZeros(1) == 0);
    assert(trailingZeros(2) == 1);
    assert(trailingZeros(3) == 0);
    assert(trailingZeros(4) == 2);
}

/*
Returns `true` if `ptr` is aligned at `alignment`.
*/
@nogc nothrow pure
package bool alignedAt(T)(T* ptr, uint alignment)
{
    return cast(size_t) ptr % alignment == 0;
}

/*
Returns the effective alignment of `ptr`, i.e. the largest power of two that is
a divisor of `ptr`.
*/
@nogc nothrow pure
package size_t effectiveAlignment(void* ptr)
{
    return (cast(size_t) 1) << trailingZeros(cast(size_t) ptr);
}

@nogc nothrow pure
@system unittest
{
    int x;
    assert(effectiveAlignment(&x) >= int.alignof);

    const max = (cast(size_t) 1) << (size_t.sizeof * 8 - 1);
    assert(effectiveAlignment(cast(void*) max) == max);
}

/*
Aligns a pointer down to a specified alignment. The resulting pointer is less
than or equal to the given pointer.
*/
@nogc nothrow pure
package void* alignDownTo(return scope void* ptr, uint alignment)
{
    import std.math.traits : isPowerOf2;
    assert(alignment.isPowerOf2);
    return cast(void*) (cast(size_t) ptr & ~(alignment - 1UL));
}

/*
Aligns a pointer up to a specified alignment. The resulting pointer is greater
than or equal to the given pointer.
*/
@nogc nothrow pure
package void* alignUpTo(return scope void* ptr, uint alignment)
{
    import std.math.traits : isPowerOf2;
    assert(alignment.isPowerOf2);
    immutable uint slack = cast(size_t) ptr & (alignment - 1U);
    return slack ? ptr + alignment - slack : ptr;
}

@safe @nogc nothrow pure
package bool isGoodStaticAlignment(uint x)
{
    import std.math.traits : isPowerOf2;
    return x.isPowerOf2;
}

@safe @nogc nothrow pure
package bool isGoodDynamicAlignment(uint x)
{
    import std.math.traits : isPowerOf2;
    return x.isPowerOf2 && x >= (void*).sizeof;
}

/**
The default `reallocate` function first attempts to use `expand`. If $(D
Allocator.expand) is not defined or returns `false`, `reallocate`
allocates a new block of memory of appropriate size and copies data from the old
block to the new block. Finally, if `Allocator` defines `deallocate`, $(D
reallocate) uses it to free the old memory block.

`reallocate` does not attempt to use `Allocator.reallocate` even if
defined. This is deliberate so allocators may use it internally within their own
implementation of `reallocate`.

*/
bool reallocate(Allocator)(ref Allocator a, ref void[] b, size_t s)
{
    if (b.length == s) return true;
    static if (hasMember!(Allocator, "expand"))
    {
        if (b.length <= s && a.expand(b, s - b.length)) return true;
    }
    auto newB = a.allocate(s);
    if (newB.length != s) return false;
    if (newB.length <= b.length) newB[] = b[0 .. newB.length];
    else newB[0 .. b.length] = b[];
    static if (hasMember!(Allocator, "deallocate"))
        a.deallocate(b);
    b = newB;
    return true;
}

/**

The default `alignedReallocate` function first attempts to use `expand`.
If `Allocator.expand` is not defined or returns `false`,  $(D
alignedReallocate) allocates a new block of memory of appropriate size and
copies data from the old block to the new block. Finally, if `Allocator`
defines `deallocate`, `alignedReallocate` uses it to free the old memory
block.

`alignedReallocate` does not attempt to use `Allocator.reallocate` even if
defined. This is deliberate so allocators may use it internally within their own
implementation of `reallocate`.

*/
bool alignedReallocate(Allocator)(ref Allocator alloc,
        ref void[] b, size_t s, uint a)
if (hasMember!(Allocator, "alignedAllocate"))
{
    static if (hasMember!(Allocator, "expand"))
    {
        if (b.length <= s && b.ptr.alignedAt(a)
            && alloc.expand(b, s - b.length)) return true;
    }
    else
    {
        if (b.length == s && b.ptr.alignedAt(a)) return true;
    }
    auto newB = alloc.alignedAllocate(s, a);
    if (newB.length != s) return false;
    if (newB.length <= b.length) newB[] = b[0 .. newB.length];
    else newB[0 .. b.length] = b[];
    static if (hasMember!(Allocator, "deallocate"))
        alloc.deallocate(b);
    b = newB;
    return true;
}

@system unittest
{
    bool called = false;
    struct DummyAllocator
    {
        void[] alignedAllocate(size_t size, uint alignment)
        {
            called = true;
            return null;
        }
    }

    struct DummyAllocatorExpand
    {
        void[] alignedAllocate(size_t size, uint alignment)
        {
            return null;
        }

        bool expand(ref void[] b, size_t length)
        {
            called = true;
            return true;
        }
    }

    char[128] buf;
    uint alignment = 32;
    auto alignedPtr = roundUpToMultipleOf(cast(size_t) buf.ptr, alignment);
    auto diff = alignedPtr - cast(size_t) buf.ptr;

    // Align the buffer to 'alignment'
    void[] b = cast(void[]) (buf.ptr + diff)[0 .. buf.length - diff];

    DummyAllocator a1;
    // Ask for same length and alignment, should not call 'alignedAllocate'
    assert(alignedReallocate(a1, b, b.length, alignment));
    assert(!called);

    // Ask for same length, different alignment
    // should call 'alignedAllocate' if not aligned to new value
    alignedReallocate(a1, b, b.length, alignment + 1);
    assert(b.ptr.alignedAt(alignment + 1) || called);
    called = false;

    DummyAllocatorExpand a2;
    // Ask for bigger length, same alignment, should call 'expand'
    assert(alignedReallocate(a2, b, b.length + 1, alignment));
    assert(called);
    called = false;

    // Ask for bigger length, different alignment
    // should call 'alignedAllocate' if not aligned to new value
    alignedReallocate(a2, b, b.length + 1, alignment + 1);
    assert(b.ptr.alignedAt(alignment + 1) || !called);
}

/**
Forwards each of the methods in `funs` (if defined) to `member`.
*/
/*package*/ string forwardToMember(string member, string[] funs...)
{
    string result = "    import std.traits : hasMember, Parameters;\n";
    foreach (fun; funs)
    {
        result ~= "
    static if (hasMember!(typeof("~member~"), `"~fun~"`))
    auto ref "~fun~"(Parameters!(typeof("~member~"."~fun~")) args)
    {
        return "~member~"."~fun~"(args);
    }\n";
    }
    return result;
}

version (StdUnittest)
{

    package void testAllocator(alias make)()
    {
        import std.conv : text;
        import std.math.traits : isPowerOf2;
        import std.stdio : writeln, stderr;
        import std.typecons : Ternary;
        alias A = typeof(make());
        scope(failure) stderr.writeln("testAllocator failed for ", A.stringof);

        auto a = make();

        // Test alignment
        static assert(A.alignment.isPowerOf2);

        // Test goodAllocSize
        assert(a.goodAllocSize(1) >= A.alignment,
                text(a.goodAllocSize(1), " < ", A.alignment));
        assert(a.goodAllocSize(11) >= 11.roundUpToMultipleOf(A.alignment));
        assert(a.goodAllocSize(111) >= 111.roundUpToMultipleOf(A.alignment));

        // Test allocate
        assert(a.allocate(0) is null);

        auto b1 = a.allocate(1);
        assert(b1.length == 1);
        auto b2 = a.allocate(2);
        assert(b2.length == 2);
        assert(b2.ptr + b2.length <= b1.ptr || b1.ptr + b1.length <= b2.ptr);

        // Test allocateZeroed
        static if (hasMember!(A, "allocateZeroed"))
        {{
            auto b3 = a.allocateZeroed(8);
            if (b3 !is null)
            {
                assert(b3.length == 8);
                foreach (e; cast(ubyte[]) b3)
                    assert(e == 0);
            }
        }}

        // Test alignedAllocate
        static if (hasMember!(A, "alignedAllocate"))
        {{
             auto b3 = a.alignedAllocate(1, 256);
             assert(b3.length <= 1);
             assert(b3.ptr.alignedAt(256));
             assert(a.alignedReallocate(b3, 2, 512));
             assert(b3.ptr.alignedAt(512));
             static if (hasMember!(A, "alignedDeallocate"))
             {
                 a.alignedDeallocate(b3);
             }
         }}
        else
        {
            static assert(!hasMember!(A, "alignedDeallocate"));
            // This seems to be a bug in the compiler:
            //static assert(!hasMember!(A, "alignedReallocate"), A.stringof);
        }

        static if (hasMember!(A, "allocateAll"))
        {{
             auto aa = make();
             if (aa.allocateAll().ptr)
             {
                 // Can't get any more memory
                 assert(!aa.allocate(1).ptr);
             }
             auto ab = make();
             const b4 = ab.allocateAll();
             assert(b4.length);
             // Can't get any more memory
             assert(!ab.allocate(1).ptr);
         }}

        static if (hasMember!(A, "expand"))
        {{
             assert(a.expand(b1, 0));
             auto len = b1.length;
             if (a.expand(b1, 102))
             {
                 assert(b1.length == len + 102, text(b1.length, " != ", len + 102));
             }
             auto aa = make();
             void[] b5 = null;
             assert(aa.expand(b5, 0));
             assert(b5 is null);
             assert(!aa.expand(b5, 1));
             assert(b5.length == 0);
         }}

        void[] b6 = null;
        assert(a.reallocate(b6, 0));
        assert(b6.length == 0);
        assert(a.reallocate(b6, 1));
        assert(b6.length == 1, text(b6.length));
        assert(a.reallocate(b6, 2));
        assert(b6.length == 2);

        // Test owns
        static if (hasMember!(A, "owns"))
        {{
             assert(a.owns(null) == Ternary.no);
             assert(a.owns(b1) == Ternary.yes);
             assert(a.owns(b2) == Ternary.yes);
             assert(a.owns(b6) == Ternary.yes);
         }}

        static if (hasMember!(A, "resolveInternalPointer"))
        {{
             void[] p;
             assert(a.resolveInternalPointer(null, p) == Ternary.no);
             Ternary r = a.resolveInternalPointer(b1.ptr, p);
             assert(p.ptr is b1.ptr && p.length >= b1.length);
             r = a.resolveInternalPointer(b1.ptr + b1.length / 2, p);
             assert(p.ptr is b1.ptr && p.length >= b1.length);
             r = a.resolveInternalPointer(b2.ptr, p);
             assert(p.ptr is b2.ptr && p.length >= b2.length);
             r = a.resolveInternalPointer(b2.ptr + b2.length / 2, p);
             assert(p.ptr is b2.ptr && p.length >= b2.length);
             r = a.resolveInternalPointer(b6.ptr, p);
             assert(p.ptr is b6.ptr && p.length >= b6.length);
             r = a.resolveInternalPointer(b6.ptr + b6.length / 2, p);
             assert(p.ptr is b6.ptr && p.length >= b6.length);
             static int[10] b7 = [ 1, 2, 3 ];
             assert(a.resolveInternalPointer(b7.ptr, p) == Ternary.no);
             assert(a.resolveInternalPointer(b7.ptr + b7.length / 2, p) == Ternary.no);
             assert(a.resolveInternalPointer(b7.ptr + b7.length, p) == Ternary.no);
             int[3] b8 = [ 1, 2, 3 ];
             assert(a.resolveInternalPointer(b8.ptr, p) == Ternary.no);
             assert(a.resolveInternalPointer(b8.ptr + b8.length / 2, p) == Ternary.no);
             assert(a.resolveInternalPointer(b8.ptr + b8.length, p) == Ternary.no);
         }}
    }

    package void testAllocatorObject(RCAllocInterface)(RCAllocInterface a)
    {
        // this used to be a template constraint, but moving it inside prevents
        // unnecessary import of std.experimental.allocator
        import std.experimental.allocator : RCIAllocator, RCISharedAllocator;
        static assert(is(RCAllocInterface == RCIAllocator)
            || is (RCAllocInterface == RCISharedAllocator));

        import std.conv : text;
        import std.math.traits : isPowerOf2;
        import std.stdio : writeln, stderr;
        import std.typecons : Ternary;
        scope(failure) stderr.writeln("testAllocatorObject failed for ",
                RCAllocInterface.stringof);

        assert(!a.isNull);

        // Test alignment
        assert(a.alignment.isPowerOf2);

        // Test goodAllocSize
        assert(a.goodAllocSize(1) >= a.alignment,
                text(a.goodAllocSize(1), " < ", a.alignment));
        assert(a.goodAllocSize(11) >= 11.roundUpToMultipleOf(a.alignment));
        assert(a.goodAllocSize(111) >= 111.roundUpToMultipleOf(a.alignment));

        // Test empty
        assert(a.empty != Ternary.no);

        // Test allocate
        assert(a.allocate(0) is null);

        auto b1 = a.allocate(1);
        assert(b1.length == 1);
        auto b2 = a.allocate(2);
        assert(b2.length == 2);
        assert(b2.ptr + b2.length <= b1.ptr || b1.ptr + b1.length <= b2.ptr);

        // Test alignedAllocate
        {
            // If not implemented it will return null, so those should pass
            auto b3 = a.alignedAllocate(1, 256);
            assert(b3.length <= 1);
            assert(b3.ptr.alignedAt(256));
            if (a.alignedReallocate(b3, 1, 256))
            {
                // If it is false, then the wrapped allocator did not implement
                // this
                assert(a.alignedReallocate(b3, 2, 512));
                assert(b3.ptr.alignedAt(512));
            }
        }

        // Test allocateAll
        {
            auto aa = a.allocateAll();
            if (aa.ptr)
            {
                // Can't get any more memory
                assert(!a.allocate(1).ptr);
                a.deallocate(aa);
            }
            const b4 = a.allocateAll();
            if (b4.ptr)
            {
                // Can't get any more memory
                assert(!a.allocate(1).ptr);
            }
        }

        // Test expand
        {
            assert(a.expand(b1, 0));
            auto len = b1.length;
            if (a.expand(b1, 102))
            {
                assert(b1.length == len + 102, text(b1.length, " != ", len + 102));
            }
        }

        void[] b6 = null;
        assert(a.reallocate(b6, 0));
        assert(b6.length == 0);
        assert(a.reallocate(b6, 1));
        assert(b6.length == 1, text(b6.length));
        assert(a.reallocate(b6, 2));
        assert(b6.length == 2);

        // Test owns
        {
            if (a.owns(null) != Ternary.unknown)
            {
                assert(a.owns(null) == Ternary.no);
                assert(a.owns(b1) == Ternary.yes);
                assert(a.owns(b2) == Ternary.yes);
                assert(a.owns(b6) == Ternary.yes);
            }
        }

        // Test resolveInternalPointer
        {
            void[] p;
            if (a.resolveInternalPointer(null, p) != Ternary.unknown)
            {
                assert(a.resolveInternalPointer(null, p) == Ternary.no);
                Ternary r = a.resolveInternalPointer(b1.ptr, p);
                assert(p.ptr is b1.ptr && p.length >= b1.length);
                r = a.resolveInternalPointer(b1.ptr + b1.length / 2, p);
                assert(p.ptr is b1.ptr && p.length >= b1.length);
                r = a.resolveInternalPointer(b2.ptr, p);
                assert(p.ptr is b2.ptr && p.length >= b2.length);
                r = a.resolveInternalPointer(b2.ptr + b2.length / 2, p);
                assert(p.ptr is b2.ptr && p.length >= b2.length);
                r = a.resolveInternalPointer(b6.ptr, p);
                assert(p.ptr is b6.ptr && p.length >= b6.length);
                r = a.resolveInternalPointer(b6.ptr + b6.length / 2, p);
                assert(p.ptr is b6.ptr && p.length >= b6.length);
                static int[10] b7 = [ 1, 2, 3 ];
                assert(a.resolveInternalPointer(b7.ptr, p) == Ternary.no);
                assert(a.resolveInternalPointer(b7.ptr + b7.length / 2, p) == Ternary.no);
                assert(a.resolveInternalPointer(b7.ptr + b7.length, p) == Ternary.no);
                int[3] b8 = [ 1, 2, 3 ];
                assert(a.resolveInternalPointer(b8.ptr, p) == Ternary.no);
                assert(a.resolveInternalPointer(b8.ptr + b8.length / 2, p) == Ternary.no);
                assert(a.resolveInternalPointer(b8.ptr + b8.length, p) == Ternary.no);
            }
        }

        // Test deallocateAll
        {
            if (a.deallocateAll())
            {
                if (a.empty != Ternary.unknown)
                {
                    assert(a.empty == Ternary.yes);
                }
            }
        }
    }
}

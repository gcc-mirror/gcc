// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/quantizer.d)
*/
module std.experimental.allocator.building_blocks.quantizer;

import std.experimental.allocator.common;

/**
This allocator sits on top of `ParentAllocator` and quantizes allocation sizes,
usually from arbitrary positive numbers to a small set of round numbers (e.g.
powers of two, page sizes etc). This technique is commonly used to:

$(UL
$(LI Preallocate more memory than requested such that later on, when
reallocation is needed (e.g. to grow an array), expansion can be done quickly
in place. Reallocation to smaller sizes is also fast (in-place) when the new
size requested is within the same quantum as the existing size. Code that's
reallocation-heavy can therefore benefit from fronting a generic allocator with
a `Quantizer`. These advantages are present even if `ParentAllocator` does not
support reallocation at all.)
$(LI Improve behavior of allocators sensitive to allocation sizes, such as
`FreeList` and `FreeTree`. Rounding allocation requests up makes for smaller
free lists/trees at the cost of slack memory (internal fragmentation).)
)

The following methods are forwarded to the parent allocator if present:
`allocateAll`, `owns`, `deallocateAll`, `empty`.

Preconditions: `roundingFunction` must satisfy three constraints. These are
not enforced (save for the use of `assert`) for the sake of efficiency.
$(OL
$(LI $(D roundingFunction(n) >= n) for all `n` of type `size_t`;)
$(LI `roundingFunction` must be monotonically increasing, i.e. $(D
roundingFunction(n1) <= roundingFunction(n2)) for all $(D n1 < n2);)
$(LI `roundingFunction` must be `nothrow`, `@safe`, `@nogc` and `pure`, i.e.
always return the same value for a given `n`.)
)
*/
struct Quantizer(ParentAllocator, alias roundingFunction)
{
    import std.traits : hasMember;

    /**
    The parent allocator. Depending on whether `ParentAllocator` holds state
    or not, this is a member variable or an alias for
    `ParentAllocator.instance`.
    */
    static if (stateSize!ParentAllocator)
    {
        ParentAllocator parent;
    }
    else
    {
        alias parent = ParentAllocator.instance;
        __gshared Quantizer instance;
    }

    /**
    Returns `roundingFunction(n)`.
    */
    size_t goodAllocSize(size_t n)
    {
        auto result = roundingFunction(n);
        assert(result >= n);
        return result;
    }

    /**
    Alignment is identical to that of the parent.
    */
    enum alignment = ParentAllocator.alignment;

    /**
    Gets a larger buffer `buf` by calling
    `parent.allocate(goodAllocSize(n))`. If `buf` is `null`, returns
    `null`. Otherwise, returns $(D buf[0 .. n]).
    */
    void[] allocate(size_t n)
    {
        auto result = parent.allocate(goodAllocSize(n));
        return result.ptr ? result.ptr[0 .. n] : null;
    }

    static if (hasMember!(ParentAllocator, "allocateZeroed"))
    package(std) void[] allocateZeroed()(size_t n)
    {
        auto result = parent.allocateZeroed(goodAllocSize(n));
        return result.ptr ? result.ptr[0 .. n] : null;
    }

    /**
    Defined only if `parent.alignedAllocate` exists and works similarly to
    `allocate` by forwarding to
    $(D parent.alignedAllocate(goodAllocSize(n), a)).
    */
    static if (hasMember!(ParentAllocator, "alignedAllocate"))
    void[] alignedAllocate(size_t n, uint a)
    {
        auto result = parent.alignedAllocate(goodAllocSize(n), a);
        return result.ptr ? result.ptr[0 .. n] : null;
    }

    /**
    First checks whether there's enough slack memory preallocated for `b`
    by evaluating $(D b.length + delta <= goodAllocSize(b.length)). If that's
    the case, expands `b` in place. Otherwise, attempts to use
    `parent.expand` appropriately if present.
    */
    bool expand(ref void[] b, size_t delta)
    {
        if (!b || delta == 0) return delta == 0;
        immutable allocated = goodAllocSize(b.length),
            needed = b.length + delta,
            neededAllocation = goodAllocSize(needed);
        assert(b.length <= allocated);
        assert(needed <= neededAllocation);
        assert(allocated <= neededAllocation);
        // Second test needed because expand must work for null pointers, too.
        if (allocated == neededAllocation)
        {
            // Nice!
            b = (() @trusted => b.ptr[0 .. needed])();
            return true;
        }
        // Hail Mary
        static if (hasMember!(ParentAllocator, "expand"))
        {
            // Expand to the appropriate quantum
            auto original = (() @trusted => b.ptr[0 .. allocated])();
            assert(goodAllocSize(needed) >= allocated);
            if (!parent.expand(original, neededAllocation - allocated))
                return false;
            // Dial back the size
            b = (() @trusted => original.ptr[0 .. needed])();
            return true;
        }
        else
        {
            return false;
        }
    }

    /**
    Expands or shrinks allocated block to an allocated size of $(D
    goodAllocSize(s)). Expansion occurs in place under the conditions required
    by `expand`. Shrinking occurs in place if $(D goodAllocSize(b.length)
    == goodAllocSize(s)).
    */
    bool reallocate(ref void[] b, size_t s)
    {
        if (!b.ptr)
        {
            b = allocate(s);
            return b.length == s;
        }
        if (s >= b.length && expand(b, s - b.length)) return true;
        immutable toAllocate = goodAllocSize(s),
            allocated = goodAllocSize(b.length);
        // Are the lengths within the same quantum?
        if (allocated == toAllocate)
        {
            // Reallocation (whether up or down) will be done in place
            b = b.ptr[0 .. s];
            return true;
        }
        // Defer to parent (or global) with quantized size
        auto original = b.ptr[0 .. allocated];
        if (!parent.reallocate(original, toAllocate)) return false;
        b = original.ptr[0 .. s];
        return true;
    }

    /**
    Defined only if `ParentAllocator.alignedAllocate` exists. Expansion
    occurs in place under the conditions required by `expand`. Shrinking
    occurs in place if $(D goodAllocSize(b.length) == goodAllocSize(s)).
    */
    static if (hasMember!(ParentAllocator, "alignedAllocate"))
    bool alignedReallocate(ref void[] b, size_t s, uint a)
    {
        if (!b.ptr)
        {
            b = alignedAllocate(s, a);
            return b.length == s;
        }
        if (s >= b.length && b.ptr.alignedAt(a) && expand(b, s - b.length)) return true;
        immutable toAllocate = goodAllocSize(s),
            allocated = goodAllocSize(b.length);
        // Are the lengths within the same quantum?
        if (allocated == toAllocate && b.ptr.alignedAt(a))
        {
            assert(b.ptr); // code above must have caught this
            // Reallocation (whether up or down) will be done in place
            b = b.ptr[0 .. s];
            return true;
        }
        // Defer to parent (or global) with quantized size
        auto original = b.ptr[0 .. allocated];
        if (!parent.alignedReallocate(original, toAllocate, a)) return false;
        b = original.ptr[0 .. s];
        return true;
    }

    /**
    Defined if `ParentAllocator.deallocate` exists and forwards to
    $(D parent.deallocate(b.ptr[0 .. goodAllocSize(b.length)])).
    */
    static if (hasMember!(ParentAllocator, "deallocate"))
    bool deallocate(void[] b)
    {
        if (!b.ptr) return true;
        return parent.deallocate(b.ptr[0 .. goodAllocSize(b.length)]);
    }

    // Forwarding methods
    mixin(forwardToMember("parent",
        "allocateAll", "owns", "deallocateAll", "empty"));
}

///
@system unittest
{
    import std.experimental.allocator.building_blocks.free_tree : FreeTree;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    size_t roundUpToMultipleOf(size_t s, uint base)
    {
        auto rem = s % base;
        return rem ? s + base - rem : s;
    }

    // Quantize small allocations to a multiple of cache line, large ones to a
    // multiple of page size
    alias MyAlloc = Quantizer!(
        FreeTree!GCAllocator,
        n => roundUpToMultipleOf(n, n <= 16_384 ? 64 : 4096));
    MyAlloc alloc;
    const buf = alloc.allocate(256);
    assert(buf.ptr);
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    alias MyAlloc = Quantizer!(GCAllocator,
        (size_t n) => n.roundUpToMultipleOf(64));
    testAllocator!(() => MyAlloc());

    assert((() pure nothrow @safe @nogc => MyAlloc().goodAllocSize(1))() == 64);

    auto a = MyAlloc();
    auto b = a.allocate(42);
    assert(b.length == 42);
    // Inplace expand, since goodAllocSize is 64
    assert((() @safe => a.expand(b, 22))());
    //assert((() nothrow @safe => a.expand(b, 22))());
    assert(b.length == 64);
    // Trigger parent.expand, which may or may not succed
    //() nothrow @safe { a.expand(b, 1); }();
    () @safe { a.expand(b, 1); }();
    assert(a.reallocate(b, 100));
    assert(b.length == 100);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { a.deallocate(b); }();
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;

    alias Alloc = Quantizer!(Region!(Mallocator),
            (size_t n) => n.roundUpToMultipleOf(64));
    auto a = Alloc(Region!Mallocator(1024 * 64));
    const b = a.allocate(42);
    assert(b.length == 42);
    // Check that owns inherits from parent, i.e. Region
    assert((() pure nothrow @safe @nogc => a.owns(b))() == Ternary.yes);
    assert((() pure nothrow @safe @nogc => a.owns(null))() == Ternary.no);

    auto c = a.allocate(42);
    assert(c.length == 42);
    assert((() pure nothrow @safe @nogc => a.owns(c))() == Ternary.yes);
    // Inplace expand, since goodAllocSize is 64
    assert((() nothrow @safe => a.expand(c, 22))());
    assert(c.length == 64);
    // Trigger parent.expand
    assert((() nothrow @safe => a.expand(c, 1))());
    assert(c.length == 65);
    // Check that reallocate inherits from parent
    assert((() nothrow @nogc => a.reallocate(c, 100))());
    assert(c.length == 100);
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.mallocator : Mallocator;

    alias MyAlloc = Quantizer!(Region!(Mallocator),
            (size_t n) => n.roundUpToMultipleOf(64));
    testAllocator!(() => MyAlloc(Region!Mallocator(1024 * 64)));

    auto a = MyAlloc(Region!Mallocator(1024 * 64));
    void[] b;
    assert((() nothrow @nogc => a.alignedReallocate(b, 42, 16))());
    assert(b.length == 42);
    assert(alignedAt(&b[0], 16));
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;
    import std.typecons : Ternary;

    alias MyAlloc = Quantizer!(BorrowedRegion!(),
        (size_t n) => n.roundUpToMultipleOf(64));
    testAllocator!(() => MyAlloc(BorrowedRegion!()(new ubyte[1024 * 64])));

    auto a = MyAlloc(BorrowedRegion!()(new ubyte[1024 * 64]));
    // Check that empty inherits from parent
    assert((() pure nothrow @safe @nogc => a.empty)() == Ternary.yes);
    auto b = a.allocate(42);
    assert(b.length == 42);
    assert((() pure nothrow @safe @nogc => a.empty)() == Ternary.no);
    // Check that deallocateAll inherits from parent
    assert((() nothrow @nogc => a.deallocateAll())());
    assert((() pure nothrow @safe @nogc => a.empty)() == Ternary.yes);
}

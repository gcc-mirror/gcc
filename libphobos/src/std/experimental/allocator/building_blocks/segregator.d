// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/segregator.d)
*/
module std.experimental.allocator.building_blocks.segregator;

import std.experimental.allocator.common;

/**
Dispatches allocations (and deallocations) between two allocators ($(D
SmallAllocator) and `LargeAllocator`) depending on the size allocated, as
follows. All allocations smaller than or equal to `threshold` will be
dispatched to `SmallAllocator`. The others will go to `LargeAllocator`.

If both allocators are `shared`, the `Segregator` will also offer $(D
shared) methods.
*/
struct Segregator(size_t threshold, SmallAllocator, LargeAllocator)
{
    import std.algorithm.comparison : min;
    import std.traits : hasMember, ReturnType;
    import std.typecons : Ternary;

    static if (stateSize!SmallAllocator) private SmallAllocator _small;
    else private alias _small = SmallAllocator.instance;
    static if (stateSize!LargeAllocator) private LargeAllocator _large;
    else private alias _large = LargeAllocator.instance;

    version (StdDdoc)
    {
        /**
        The alignment offered is the minimum of the two allocators' alignment.
        */
        enum uint alignment;
        /**
        This method is defined only if at least one of the allocators defines
        it. The good allocation size is obtained from `SmallAllocator` if $(D
        s <= threshold), or `LargeAllocator` otherwise. (If one of the
        allocators does not define `goodAllocSize`, the default
        implementation in this module applies.)
        */
        static size_t goodAllocSize(size_t s);
        /**
        The memory is obtained from `SmallAllocator` if $(D s <= threshold),
        or `LargeAllocator` otherwise.
        */
        void[] allocate(size_t);
        /**
        This method is defined if both allocators define it, and forwards to
        `SmallAllocator` or `LargeAllocator` appropriately.
        */
        void[] alignedAllocate(size_t, uint);
        /**
        This method is defined only if at least one of the allocators defines
        it. If `SmallAllocator` defines `expand` and $(D b.length +
        delta <= threshold), the call is forwarded to `SmallAllocator`. If $(D
        LargeAllocator) defines `expand` and $(D b.length > threshold), the
        call is forwarded to `LargeAllocator`. Otherwise, the call returns
        `false`.
        */
        bool expand(ref void[] b, size_t delta);
        /**
        This method is defined only if at least one of the allocators defines
        it. If `SmallAllocator` defines `reallocate` and $(D b.length <=
        threshold && s <= threshold), the call is forwarded to $(D
        SmallAllocator). If `LargeAllocator` defines `expand` and $(D
        b.length > threshold && s > threshold), the call is forwarded to $(D
        LargeAllocator). Otherwise, the call returns `false`.
        */
        bool reallocate(ref void[] b, size_t s);
        /**
        This method is defined only if at least one of the allocators defines
        it, and work similarly to `reallocate`.
        */
        bool alignedReallocate(ref void[] b, size_t s, uint a);
        /**
        This method is defined only if both allocators define it. The call is
        forwarded to `SmallAllocator` if $(D b.length <= threshold), or $(D
        LargeAllocator) otherwise.
        */
        Ternary owns(void[] b);
        /**
        This function is defined only if both allocators define it, and forwards
        appropriately depending on `b.length`.
        */
        bool deallocate(void[] b);
        /**
        This function is defined only if both allocators define it, and calls
        `deallocateAll` for them in turn.
        */
        bool deallocateAll();
        /**
        This function is defined only if both allocators define it, and returns
        the conjunction of `empty` calls for the two.
        */
        Ternary empty();
    }

    /**
    Composite allocators involving nested instantiations of `Segregator` make
    it difficult to access individual sub-allocators stored within. $(D
    allocatorForSize) simplifies the task by supplying the allocator nested
    inside a `Segregator` that is responsible for a specific size `s`.

    Example:
    ----
    alias A = Segregator!(300,
        Segregator!(200, A1, A2),
        A3);
    A a;
    static assert(typeof(a.allocatorForSize!10) == A1);
    static assert(typeof(a.allocatorForSize!250) == A2);
    static assert(typeof(a.allocatorForSize!301) == A3);
    ----
    */
    ref auto allocatorForSize(size_t s)()
    {
        static if (s <= threshold)
            static if (is(SmallAllocator == Segregator!(Args), Args...))
                return _small.allocatorForSize!s;
            else return _small;
        else
            static if (is(LargeAllocator == Segregator!(Args), Args...))
                return _large.allocatorForSize!s;
            else return _large;
    }

    enum uint alignment = min(SmallAllocator.alignment,
        LargeAllocator.alignment);

    private template Impl()
    {
        size_t goodAllocSize(size_t s)
        {
            return s <= threshold
                ? _small.goodAllocSize(s)
                : _large.goodAllocSize(s);
        }

        void[] allocate(size_t s)
        {
            return s <= threshold ? _small.allocate(s) : _large.allocate(s);
        }

        static if (hasMember!(SmallAllocator, "alignedAllocate")
                && hasMember!(LargeAllocator, "alignedAllocate"))
        void[] alignedAllocate(size_t s, uint a)
        {
            return s <= threshold
                ? _small.alignedAllocate(s, a)
                : _large.alignedAllocate(s, a);
        }

        static if (hasMember!(SmallAllocator, "expand")
                || hasMember!(LargeAllocator, "expand"))
        bool expand(ref void[] b, size_t delta)
        {
            if (!delta) return true;
            if (b.length + delta <= threshold)
            {
                // Old and new allocations handled by _small
                static if (hasMember!(SmallAllocator, "expand"))
                    return _small.expand(b, delta);
                else
                    return false;
            }
            if (b.length > threshold)
            {
                // Old and new allocations handled by _large
                static if (hasMember!(LargeAllocator, "expand"))
                    return _large.expand(b, delta);
                else
                    return false;
            }
            // Oops, cross-allocator transgression
            return false;
        }

        static if (hasMember!(SmallAllocator, "reallocate")
                || hasMember!(LargeAllocator, "reallocate"))
        bool reallocate(ref void[] b, size_t s)
        {
            static if (hasMember!(SmallAllocator, "reallocate"))
                if (b.length <= threshold && s <= threshold)
                {
                    // Old and new allocations handled by _small
                    return _small.reallocate(b, s);
                }
            static if (hasMember!(LargeAllocator, "reallocate"))
                if (b.length > threshold && s > threshold)
                {
                    // Old and new allocations handled by _large
                    return _large.reallocate(b, s);
                }
            // Cross-allocator transgression
            return .reallocate(this, b, s);
        }

        static if (hasMember!(SmallAllocator, "alignedReallocate")
                || hasMember!(LargeAllocator, "alignedReallocate"))
        bool alignedReallocate(ref void[] b, size_t s, uint a)
        {
            static if (hasMember!(SmallAllocator, "alignedReallocate"))
                if (b.length <= threshold && s <= threshold)
                {
                    // Old and new allocations handled by _small
                    return _small.alignedReallocate(b, s, a);
                }
            static if (hasMember!(LargeAllocator, "alignedReallocate"))
                if (b.length > threshold && s > threshold)
                {
                    // Old and new allocations handled by _large
                    return _large.alignedReallocate(b, s, a);
                }
            // Cross-allocator transgression
            return .alignedReallocate(this, b, s, a);
        }

        static if (hasMember!(SmallAllocator, "allocateZeroed")
                || hasMember!(LargeAllocator, "allocateZeroed"))
        package(std) void[] allocateZeroed()(size_t s)
        {
            if (s <= threshold)
            {
                static if (hasMember!(SmallAllocator, "allocateZeroed"))
                    return _small.allocateZeroed(s);
                else
                {
                    auto b = _small.allocate(s);
                    (() @trusted => (cast(ubyte[]) b)[] = 0)(); // OK even if b is null.
                    return b;
                }
            }
            else
            {
                static if (hasMember!(LargeAllocator, "allocateZeroed"))
                    return _large.allocateZeroed(s);
                else
                {
                    auto b = _large.allocate(s);
                    (() @trusted => (cast(ubyte[]) b)[] = 0)(); // OK even if b is null.
                    return b;
                }
            }
        }

        static if (hasMember!(SmallAllocator, "owns")
                && hasMember!(LargeAllocator, "owns"))
        Ternary owns(void[] b)
        {
            return Ternary(b.length <= threshold
                ? _small.owns(b) : _large.owns(b));
        }

        static if (hasMember!(SmallAllocator, "deallocate")
                && hasMember!(LargeAllocator, "deallocate"))
        bool deallocate(void[] data)
        {
            return data.length <= threshold
                ? _small.deallocate(data)
                : _large.deallocate(data);
        }

        static if (hasMember!(SmallAllocator, "deallocateAll")
                && hasMember!(LargeAllocator, "deallocateAll"))
        bool deallocateAll()
        {
            // Use & insted of && to evaluate both
            return _small.deallocateAll() & _large.deallocateAll();
        }

        static if (hasMember!(SmallAllocator, "empty")
                && hasMember!(LargeAllocator, "empty"))
        Ternary empty()
        {
            return _small.empty & _large.empty;
        }

        static if (hasMember!(SmallAllocator, "resolveInternalPointer")
                && hasMember!(LargeAllocator, "resolveInternalPointer"))
        Ternary resolveInternalPointer(const void* p, ref void[] result)
        {
            Ternary r = _small.resolveInternalPointer(p, result);
            return r == Ternary.no ? _large.resolveInternalPointer(p, result) : r;
        }
    }

    private enum sharedMethods =
        !stateSize!SmallAllocator
        && !stateSize!LargeAllocator
        && is(typeof(SmallAllocator.instance) == shared)
        && is(typeof(LargeAllocator.instance) == shared);

    static if (sharedMethods)
    {
        static shared Segregator instance;
        shared { mixin Impl!(); }
    }
    else
    {
        static if (!stateSize!SmallAllocator && !stateSize!LargeAllocator)
            __gshared Segregator instance;
        mixin Impl!();
    }
}

///
@system unittest
{
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    alias A =
        Segregator!(
            1024 * 4,
            Segregator!(
                128, FreeList!(Mallocator, 0, 128),
                GCAllocator),
            Segregator!(
                1024 * 1024, Mallocator,
                GCAllocator)
            );
    A a;
    auto b = a.allocate(200);
    assert(b.length == 200);
    a.deallocate(b);
}

/**
A `Segregator` with more than three arguments expands to a composition of
elemental `Segregator`s, as illustrated by the following example:

----
alias A =
    Segregator!(
        n1, A1,
        n2, A2,
        n3, A3,
        A4
    );
----

With this definition, allocation requests for `n1` bytes or less are directed
to `A1`; requests between $(D n1 + 1) and `n2` bytes (inclusive) are
directed to `A2`; requests between $(D n2 + 1) and `n3` bytes (inclusive)
are directed to `A3`; and requests for more than `n3` bytes are directed
to `A4`. If some particular range should not be handled, `NullAllocator`
may be used appropriately.

*/
template Segregator(Args...)
if (Args.length > 3)
{
    // Binary search
    private enum cutPoint = ((Args.length - 2) / 4) * 2;
    static if (cutPoint >= 2)
    {
        alias Segregator = .Segregator!(
            Args[cutPoint],
            .Segregator!(Args[0 .. cutPoint], Args[cutPoint + 1]),
            .Segregator!(Args[cutPoint + 2 .. $])
        );
    }
    else
    {
        // Favor small sizes
        alias Segregator = .Segregator!(
            Args[0],
            Args[1],
            .Segregator!(Args[2 .. $])
        );
    }
}

///
@system unittest
{
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    alias A =
        Segregator!(
            128, FreeList!(Mallocator, 0, 128),
            1024 * 4, GCAllocator,
            1024 * 1024, Mallocator,
            GCAllocator
        );
    A a;
    auto b = a.allocate(201);
    assert(b.length == 201);
    a.deallocate(b);
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.building_blocks.kernighan_ritchie : KRRegion;
    Segregator!(128, GCAllocator, KRRegion!GCAllocator) alloc;
    assert((() nothrow @safe @nogc => alloc.goodAllocSize(1))()
            == GCAllocator.instance.goodAllocSize(1));

    // Note: we infer `shared` from GCAllocator.goodAllocSize so we need a
    // shared object in order to be able to use the function
    shared Segregator!(128, GCAllocator, GCAllocator) sharedAlloc;
    assert((() nothrow @safe @nogc => sharedAlloc.goodAllocSize(1))()
            == GCAllocator.instance.goodAllocSize(1));
}

@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.typecons : Ternary;

    alias A =
        Segregator!(
            128, BitmappedBlock!(4096),
            BitmappedBlock!(4096)
        );

    A a = A(
            BitmappedBlock!(4096)(new ubyte[4096 * 1024]),
            BitmappedBlock!(4096)(new ubyte[4096 * 1024])
    );

    assert(a.empty == Ternary.yes);
    auto b = a.allocate(42);
    assert(b.length == 42);
    assert(a.empty == Ternary.no);
    assert(a.alignedReallocate(b, 256, 512));
    assert(b.length == 256);
    assert(a.alignedReallocate(b, 42, 512));
    assert(b.length == 42);
    assert((() pure nothrow @safe @nogc => a.owns(b))() == Ternary.yes);
    assert((() pure nothrow @safe @nogc => a.owns(null))() == Ternary.no);
    // Ensure deallocate inherits from parent allocators
    assert((() nothrow @nogc => a.deallocate(b))());
    assert(a.empty == Ternary.yes);

    // Test that deallocateAll inherits from parents
    auto c = a.allocate(42);
    assert(c.length == 42);
    assert((() pure nothrow @safe @nogc => a.expand(c, 58))());
    assert(c.length == 100);
    assert(a.empty == Ternary.no);
    assert((() nothrow @nogc => a.deallocateAll())());
    assert(a.empty == Ternary.yes);
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;

    shared Segregator!(1024 * 4, GCAllocator, GCAllocator) a;

    auto b = a.allocate(201);
    assert(b.length == 201);

    void[] p;
    assert((() nothrow @safe @nogc => a.resolveInternalPointer(&b[0], p))() == Ternary.yes);
    assert((() nothrow @safe @nogc => a.resolveInternalPointer(null, p))() == Ternary.no);

    // Ensure deallocate inherits from parent allocators
    assert((() nothrow @nogc => a.deallocate(b))());
}

@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlockWithInternalPointers;
    import std.typecons : Ternary;

    alias A =
        Segregator!(
            10_240, BitmappedBlockWithInternalPointers!(4096),
            BitmappedBlockWithInternalPointers!(4096)
        );

    A a = A(
            BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024]),
            BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024])
    );

    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);
    auto b = a.allocate(201);
    assert(b.length == 201);
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.no);
    assert((() nothrow @nogc => a.deallocate(b))());
}

// Test that reallocate infers from parent
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    alias a = Segregator!(10_240, Mallocator, Mallocator).instance;

    auto b = a.allocate(42);
    assert(b.length == 42);
    assert((() nothrow @nogc => a.reallocate(b, 100))());
    assert(b.length == 100);
    assert((() nothrow @nogc => a.deallocate(b))());
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;
    import std.typecons : Ternary;

    auto a = Segregator!(10_240, BorrowedRegion!(), BorrowedRegion!())(
                BorrowedRegion!()(new ubyte[4096 * 1024]),
                BorrowedRegion!()(new ubyte[4096 * 1024]));

    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);
    auto b = a.alignedAllocate(42, 8);
    assert(b.length == 42);
    assert((() nothrow @nogc => a.alignedReallocate(b, 100, 8))());
    assert(b.length == 100);
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.no);
    assert((() nothrow @nogc => a.deallocate(b))());
}

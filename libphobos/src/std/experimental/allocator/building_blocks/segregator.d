///
module std.experimental.allocator.building_blocks.segregator;

import std.experimental.allocator.common;

/**
Dispatches allocations (and deallocations) between two allocators ($(D
SmallAllocator) and $(D LargeAllocator)) depending on the size allocated, as
follows. All allocations smaller than or equal to $(D threshold) will be
dispatched to $(D SmallAllocator). The others will go to $(D LargeAllocator).

If both allocators are $(D shared), the $(D Segregator) will also offer $(D
shared) methods.
*/
struct Segregator(size_t threshold, SmallAllocator, LargeAllocator)
{
    import std.algorithm.comparison : min;
    import std.traits : hasMember;
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
        it. The good allocation size is obtained from $(D SmallAllocator) if $(D
        s <= threshold), or $(D LargeAllocator) otherwise. (If one of the
        allocators does not define $(D goodAllocSize), the default
        implementation in this module applies.)
        */
        static size_t goodAllocSize(size_t s);
        /**
        The memory is obtained from $(D SmallAllocator) if $(D s <= threshold),
        or $(D LargeAllocator) otherwise.
        */
        void[] allocate(size_t);
        /**
        This method is defined if both allocators define it, and forwards to
        $(D SmallAllocator) or $(D LargeAllocator) appropriately.
        */
        void[] alignedAllocate(size_t, uint);
        /**
        This method is defined only if at least one of the allocators defines
        it. If $(D SmallAllocator) defines $(D expand) and $(D b.length +
        delta <= threshold), the call is forwarded to $(D SmallAllocator). If $(D
        LargeAllocator) defines $(D expand) and $(D b.length > threshold), the
        call is forwarded to $(D LargeAllocator). Otherwise, the call returns
        $(D false).
        */
        bool expand(ref void[] b, size_t delta);
        /**
        This method is defined only if at least one of the allocators defines
        it. If $(D SmallAllocator) defines $(D reallocate) and $(D b.length <=
        threshold && s <= threshold), the call is forwarded to $(D
        SmallAllocator). If $(D LargeAllocator) defines $(D expand) and $(D
        b.length > threshold && s > threshold), the call is forwarded to $(D
        LargeAllocator). Otherwise, the call returns $(D false).
        */
        bool reallocate(ref void[] b, size_t s);
        /**
        This method is defined only if at least one of the allocators defines
        it, and work similarly to $(D reallocate).
        */
        bool alignedReallocate(ref void[] b, size_t s);
        /**
        This method is defined only if both allocators define it. The call is
        forwarded to $(D SmallAllocator) if $(D b.length <= threshold), or $(D
        LargeAllocator) otherwise.
        */
        Ternary owns(void[] b);
        /**
        This function is defined only if both allocators define it, and forwards
        appropriately depending on $(D b.length).
        */
        bool deallocate(void[] b);
        /**
        This function is defined only if both allocators define it, and calls
        $(D deallocateAll) for them in turn.
        */
        bool deallocateAll();
        /**
        This function is defined only if both allocators define it, and returns
        the conjunction of $(D empty) calls for the two.
        */
        Ternary empty();
    }

    /**
    Composite allocators involving nested instantiations of $(D Segregator) make
    it difficult to access individual sub-allocators stored within. $(D
    allocatorForSize) simplifies the task by supplying the allocator nested
    inside a $(D Segregator) that is responsible for a specific size $(D s).

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
        bool reallocate(ref void[] b, size_t s)
        {
            static if (hasMember!(SmallAllocator, "alignedReallocate"))
                if (b.length <= threshold && s <= threshold)
                {
                    // Old and new allocations handled by _small
                    return _small.alignedReallocate(b, s);
                }
            static if (hasMember!(LargeAllocator, "alignedReallocate"))
                if (b.length > threshold && s > threshold)
                {
                    // Old and new allocations handled by _large
                    return _large.alignedReallocate(b, s);
                }
            // Cross-allocator transgression
            return .alignedReallocate(this, b, s);
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
            return _small.empty && _large.empty;
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
            static __gshared Segregator instance;
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
A $(D Segregator) with more than three arguments expands to a composition of
elemental $(D Segregator)s, as illustrated by the following example:

----
alias A =
    Segregator!(
        n1, A1,
        n2, A2,
        n3, A3,
        A4
    );
----

With this definition, allocation requests for $(D n1) bytes or less are directed
to $(D A1); requests between $(D n1 + 1) and $(D n2) bytes (inclusive) are
directed to $(D A2); requests between $(D n2 + 1) and $(D n3) bytes (inclusive)
are directed to $(D A3); and requests for more than $(D n3) bytes are directed
to $(D A4). If some particular range should not be handled, $(D NullAllocator)
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

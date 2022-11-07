// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/fallback_allocator.d)
*/
module std.experimental.allocator.building_blocks.fallback_allocator;

import std.experimental.allocator.common;

/**
`FallbackAllocator` is the allocator equivalent of an "or" operator in
algebra. An allocation request is first attempted with the `Primary`
allocator. If that returns `null`, the request is forwarded to the $(D
Fallback) allocator. All other requests are dispatched appropriately to one of
the two allocators.

In order to work, `FallbackAllocator` requires that `Primary` defines the
`owns` method. This is needed in order to decide which allocator was
responsible for a given allocation.

`FallbackAllocator` is useful for fast, special-purpose allocators backed up
by general-purpose allocators. The example below features a stack region backed
up by the `GCAllocator`.
*/
struct FallbackAllocator(Primary, Fallback)
{
    import std.algorithm.comparison : min;
    import std.traits : hasMember;
    import std.typecons : Ternary;

    // Need both allocators to be stateless
    // This is to avoid using default initialized stateful allocators
    static if (!stateSize!Primary && !stateSize!Fallback)
    version (StdUnittest)
    @system unittest
    {
        testAllocator!(() => FallbackAllocator());
    }

    /// The primary allocator.
    static if (stateSize!Primary) Primary primary;
    else alias primary = Primary.instance;

    /// The fallback allocator.
    static if (stateSize!Fallback) Fallback fallback;
    else alias fallback = Fallback.instance;

    /**
    If both `Primary` and `Fallback` are stateless, `FallbackAllocator`
    defines a static instance called `instance`.
    */
    static if (!stateSize!Primary && !stateSize!Fallback)
    {
        static FallbackAllocator instance;
    }

    /**
    The alignment offered is the minimum of the two allocators' alignment.
    */
    enum uint alignment = min(Primary.alignment, Fallback.alignment);

    /**
    Allocates memory trying the primary allocator first. If it returns $(D
    null), the fallback allocator is tried.
    */
    void[] allocate(size_t s)
    {
        auto result = primary.allocate(s);
        return result.length == s ? result : fallback.allocate(s);
    }

    static if (hasMember!(Primary, "allocateZeroed")
            || (hasMember!(Fallback, "allocateZeroed")))
    package(std) void[] allocateZeroed()(size_t s)
    {
        // Try to allocate with primary.
        static if (hasMember!(Primary, "allocateZeroed"))
        {
            void[] result = primary.allocateZeroed(s);
            if (result.length == s) return result;
        }
        else
        {
            void[] result = primary.allocate(s);
            if (result.length == s)
            {
                (() @trusted => (cast(ubyte[]) result)[] = 0)();
                return result;
            }
        }
        // Allocate with fallback.
        static if (hasMember!(Fallback, "allocateZeroed"))
        {
            return fallback.allocateZeroed(s);
        }
        else
        {
            result = fallback.allocate(s);
            (() @trusted => (cast(ubyte[]) result)[] = 0)(); // OK even if result is null.
            return result;
        }
    }

    /**
    `FallbackAllocator` offers `alignedAllocate` iff at least one of the
    allocators also offers it. It attempts to allocate using either or both.
    */
    static if (hasMember!(Primary, "alignedAllocate")
        || hasMember!(Fallback, "alignedAllocate"))
    void[] alignedAllocate(size_t s, uint a)
    {
        static if (hasMember!(Primary, "alignedAllocate"))
        {{
            auto result = primary.alignedAllocate(s, a);
            if (result.length == s) return result;
        }}
        static if (hasMember!(Fallback, "alignedAllocate"))
        {{
            auto result = fallback.alignedAllocate(s, a);
            if (result.length == s) return result;
        }}
        return null;
    }

    /**

    `expand` is defined if and only if at least one of the allocators
    defines `expand`. It works as follows. If `primary.owns(b)`, then the
    request is forwarded to `primary.expand` if it is defined, or fails
    (returning `false`) otherwise. If `primary` does not own `b`, then
    the request is forwarded to `fallback.expand` if it is defined, or fails
    (returning `false`) otherwise.

    */
    static if (hasMember!(Primary, "owns")
        && (hasMember!(Primary, "expand") || hasMember!(Fallback, "expand")))
    bool expand(ref void[] b, size_t delta)
    {
        if (!delta) return true;
        if (!b.ptr) return false;
        if (primary.owns(b) == Ternary.yes)
        {
            static if (hasMember!(Primary, "expand"))
                return primary.expand(b, delta);
            else
                return false;
        }
        static if (hasMember!(Fallback, "expand"))
            return fallback.expand(b, delta);
        else
            return false;
    }

    /**

    `reallocate` works as follows. If `primary.owns(b)`, then $(D
    primary.reallocate(b, newSize)) is attempted. If it fails, an attempt is
    made to move the allocation from `primary` to `fallback`.

    If `primary` does not own `b`, then $(D fallback.reallocate(b,
    newSize)) is attempted. If that fails, an attempt is made to move the
    allocation from `fallback` to `primary`.

    */
    static if (hasMember!(Primary, "owns"))
    bool reallocate(ref void[] b, size_t newSize)
    {
        bool crossAllocatorMove(From, To)(ref From from, ref To to)
        {
            auto b1 = to.allocate(newSize);
            if (b1.length != newSize) return false;
            if (b.length < newSize) b1[0 .. b.length] = b[];
            else b1[] = b[0 .. newSize];
            static if (hasMember!(From, "deallocate"))
                from.deallocate(b);
            b = b1;
            return true;
        }

        if (b is null || primary.owns(b) == Ternary.yes)
        {
            return primary.reallocate(b, newSize)
                // Move from primary to fallback
                || crossAllocatorMove(primary, fallback);
        }
        return fallback.reallocate(b, newSize)
            // Interesting. Move from fallback to primary.
            || crossAllocatorMove(fallback, primary);
    }

    static if (hasMember!(Primary, "owns")
        && (hasMember!(Primary, "alignedAllocate")
            || hasMember!(Fallback, "alignedAllocate")))
    bool alignedReallocate(ref void[] b, size_t newSize, uint a)
    {
        bool crossAllocatorMove(From, To)(ref From from, ref To to)
        {
            static if (!hasMember!(To, "alignedAllocate"))
            {
                return false;
            }
            else
            {
                auto b1 = to.alignedAllocate(newSize, a);
                if (b1.length != newSize) return false;
                if (b.length < newSize) b1[0 .. b.length] = b[];
                else b1[] = b[0 .. newSize];
                static if (hasMember!(From, "deallocate"))
                    from.deallocate(b);
                b = b1;
                return true;
            }
        }

        static if (hasMember!(Primary, "alignedAllocate"))
        {
            if (b is null || primary.owns(b) == Ternary.yes)
            {
                return primary.alignedReallocate(b, newSize, a)
                    || crossAllocatorMove(primary, fallback);
            }
        }
        static if (hasMember!(Fallback, "alignedAllocate"))
        {
            return fallback.alignedReallocate(b, newSize, a)
                || crossAllocatorMove(fallback, primary);
        }
        else
        {
            return false;
        }
    }

    /**
    `owns` is defined if and only if both allocators define `owns`.
    Returns $(D primary.owns(b) | fallback.owns(b)).
    */
    static if (hasMember!(Primary, "owns") && hasMember!(Fallback, "owns"))
    Ternary owns(void[] b)
    {
        return primary.owns(b) | fallback.owns(b);
    }

    /**
    `resolveInternalPointer` is defined if and only if both allocators
    define it.
    */
    static if (hasMember!(Primary, "resolveInternalPointer")
        && hasMember!(Fallback, "resolveInternalPointer"))
    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        Ternary r = primary.resolveInternalPointer(p, result);
        return r == Ternary.no ? fallback.resolveInternalPointer(p, result) : r;
    }

    /**
    `deallocate` is defined if and only if at least one of the allocators
    define    `deallocate`. It works as follows. If `primary.owns(b)`,
    then the request is forwarded to `primary.deallocate` if it is defined,
    or is a no-op otherwise. If `primary` does not own `b`, then the
    request is forwarded to `fallback.deallocate` if it is defined, or is a
    no-op otherwise.
    */
    static if (hasMember!(Primary, "owns") &&
        (hasMember!(Primary, "deallocate")
            || hasMember!(Fallback, "deallocate")))
    bool deallocate(void[] b)
    {
        if (primary.owns(b) == Ternary.yes)
        {
            static if (hasMember!(Primary, "deallocate"))
                return primary.deallocate(b);
            else
                return false;
        }
        else
        {
            static if (hasMember!(Fallback, "deallocate"))
                return fallback.deallocate(b);
            else
                return false;
        }
    }

    /**
    `empty` is defined if both allocators also define it.

    Returns: $(D primary.empty & fallback.empty)
    */
    static if (hasMember!(Primary, "empty")
               && hasMember!(Fallback, "empty"))
    Ternary empty()
    {
        return primary.empty & fallback.empty;
    }
}

@system unittest
{
    import std.conv : text;
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;
    FallbackAllocator!(InSituRegion!16_384, GCAllocator) a;
    // This allocation uses the stack
    auto b1 = a.allocate(1024);
    assert(b1.length == 1024, text(b1.length));
    assert((() pure nothrow @safe @nogc => a.primary.owns(b1))() == Ternary.yes);
    assert((() nothrow => a.reallocate(b1, 2048))());
    assert(b1.length == 2048, text(b1.length));
    assert((() pure nothrow @safe @nogc => a.primary.owns(b1))() == Ternary.yes);
    // This large allocation will go to the GCAllocator
    auto b2 = a.allocate(1024 * 1024);
    assert((() pure nothrow @safe @nogc => a.primary.owns(b2))() == Ternary.no);
    // Ensure deallocate inherits from parent allocators
    () nothrow @nogc { a.deallocate(b1); }();
    () nothrow @nogc { a.deallocate(b2); }();
}

@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlockWithInternalPointers;
    import std.typecons : Ternary;

    alias A =
        FallbackAllocator!(
            BitmappedBlockWithInternalPointers!(4096),
            BitmappedBlockWithInternalPointers!(4096)
        );

    A a = A(
            BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024]),
            BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024])
    );

    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);
    auto b = a.allocate(201);
    assert(b.length == 201);
    assert(a.reallocate(b, 202));
    assert(b.length == 202);
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.no);
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;
    import std.typecons : Ternary;

    auto a = FallbackAllocator!(BorrowedRegion!(), BorrowedRegion!())(
                BorrowedRegion!()(new ubyte[4096 * 1024]),
                BorrowedRegion!()(new ubyte[4096 * 1024]));

    auto b = a.alignedAllocate(42, 8);
    assert(b.length == 42);
    assert((() nothrow @nogc => a.alignedReallocate(b, 100, 8))());
    assert(b.length == 100);
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlockWithInternalPointers;
    import std.typecons : Ternary;

    alias A =
        FallbackAllocator!(
            BitmappedBlockWithInternalPointers!(4096),
            BitmappedBlockWithInternalPointers!(4096)
        );

    // Run testAllocator here since both allocators stateful
    testAllocator!(
        () => A(
            BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024]),
            BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024])
        )
    );
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;

    alias a = FallbackAllocator!(Mallocator, Mallocator).instance;

    auto b = a.allocate(42);
    assert(b.length == 42);
    assert((() nothrow @nogc => a.reallocate(b, 100))());
    assert(b.length == 100);
}

/*
Forwards an argument from one function to another
*/
private auto ref forward(alias arg)()
{
    static if (__traits(isRef, arg))
    {
        return arg;
    }
    else
    {
        import std.algorithm.mutation : move;
        return move(arg);
    }
}

@safe unittest
{
    void fun(T)(auto ref T, string) { /* ... */ }
    void gun(T...)(auto ref T args)
    {
        fun(forward!(args[0]), forward!(args[1]));
    }
    gun(42, "hello");
    int x;
    gun(x, "hello");
}

@safe unittest
{
    static void checkByRef(T)(auto ref T value)
    {
        static assert(__traits(isRef, value));
    }

    static void checkByVal(T)(auto ref T value)
    {
        static assert(!__traits(isRef, value));
    }

    static void test1(ref int a) { checkByRef(forward!a); }
    static void test2(int a) { checkByVal(forward!a); }
    static void test3() { int a; checkByVal(forward!a); }
}

/**
Convenience function that uses type deduction to return the appropriate
`FallbackAllocator` instance. To initialize with allocators that don't have
state, use their `it` static member.
*/
FallbackAllocator!(Primary, Fallback)
fallbackAllocator(Primary, Fallback)(auto ref Primary p, auto ref Fallback f)
{
    alias R = FallbackAllocator!(Primary, Fallback);

    static if (stateSize!Primary)
        static if (stateSize!Fallback)
            return R(forward!p, forward!f);
        else
            return R(forward!p);
    else
        static if (stateSize!Fallback)
            return R(forward!f);
        else
            return R();
}

///
@system unittest
{
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;
    auto a = fallbackAllocator(Region!GCAllocator(1024), GCAllocator.instance);
    auto b1 = a.allocate(1020);
    assert(b1.length == 1020);
    assert(a.primary.owns(b1) == Ternary.yes);
    auto b2 = a.allocate(10);
    assert(b2.length == 10);
    assert(a.primary.owns(b2) == Ternary.no);
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    testAllocator!(() => fallbackAllocator(Region!GCAllocator(1024), GCAllocator.instance));
}

// Ensure `owns` inherits function attributes
@system unittest
{
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    import std.typecons : Ternary;

    FallbackAllocator!(InSituRegion!16_384, InSituRegion!16_384) a;
    auto buff = a.allocate(42);
    assert((() pure nothrow @safe @nogc => a.owns(buff))() == Ternary.yes);
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;

    auto a = fallbackAllocator(GCAllocator.instance, GCAllocator.instance);
    auto b = a.allocate(1020);
    assert(b.length == 1020);

    void[] p;
    assert((() nothrow @safe @nogc => a.resolveInternalPointer(null, p))() == Ternary.no);
    assert((() nothrow @safe @nogc => a.resolveInternalPointer(&b[0], p))() == Ternary.yes);
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;
    import std.typecons : Ternary;

    alias A = FallbackAllocator!(BorrowedRegion!(), BorrowedRegion!());
    auto a = A(BorrowedRegion!()(new ubyte[16_384]), BorrowedRegion!()(new ubyte[16_384]));

    auto b = a.allocate(42);
    assert(b.length == 42);
    assert((() pure nothrow @safe @nogc => a.owns(b))() == Ternary.yes);
    assert((() nothrow @safe @nogc => a.expand(b, 58))());
    assert(b.length == 100);
}

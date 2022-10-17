// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/scoped_allocator.d)
*/
module std.experimental.allocator.building_blocks.scoped_allocator;

import std.experimental.allocator.common;

/**

`ScopedAllocator` delegates all allocation requests to `ParentAllocator`.
When destroyed, the `ScopedAllocator` object automatically calls $(D
deallocate) for all memory allocated through its lifetime. (The $(D
deallocateAll) function is also implemented with the same semantics.)

`deallocate` is also supported, which is where most implementation effort
and overhead of `ScopedAllocator` go. If `deallocate` is not needed, a
simpler design combining `AllocatorList` with `Region` is recommended.

*/
struct ScopedAllocator(ParentAllocator)
{
    static if (!stateSize!ParentAllocator)
    {
        // This test is available only for stateless allocators
        version (StdUnittest)
        @system unittest
        {
            testAllocator!(() => ScopedAllocator());
        }
    }

    import std.experimental.allocator.building_blocks.affix_allocator
        : AffixAllocator;
    import std.traits : hasMember;
    import std.typecons : Ternary;

    private struct Node
    {
        Node* prev;
        Node* next;
        size_t length;
    }

    alias Allocator = AffixAllocator!(ParentAllocator, Node);

    // state
    /**
    If `ParentAllocator` is stateful, `parent` is a property giving access
    to an `AffixAllocator!ParentAllocator`. Otherwise, `parent` is an alias for `AffixAllocator!ParentAllocator.instance`.
    */
    static if (stateSize!ParentAllocator)
    {
        Allocator parent;
    }
    else
    {
        alias parent = Allocator.instance;
    }
    private Node* root;

    /**
    `ScopedAllocator` is not copyable.
    */
    @disable this(this);

    /**
    `ScopedAllocator`'s destructor releases all memory allocated during its
    lifetime.
    */
    ~this()
    {
        deallocateAll;
    }

    /// Alignment offered
    enum alignment = Allocator.alignment;

    /**
    Forwards to `parent.goodAllocSize` (which accounts for the management
    overhead).
    */
    size_t goodAllocSize(size_t n)
    {
        return parent.goodAllocSize(n);
    }

    // Common code shared between allocate and allocateZeroed.
    private enum _processAndReturnAllocateResult =
    q{
       if (!b.ptr) return b;
        Node* toInsert = & parent.prefix(b);
        toInsert.prev = null;
        toInsert.next = root;
        toInsert.length = n;
        assert(!root || !root.prev);
        if (root) root.prev = toInsert;
        root = toInsert;
        return b;
    };

    /**
    Allocates memory. For management it actually allocates extra memory from
    the parent.
    */
    void[] allocate(size_t n)
    {
        auto b = parent.allocate(n);
        mixin(_processAndReturnAllocateResult);
    }

    static if (hasMember!(Allocator, "allocateZeroed"))
    package(std) void[] allocateZeroed()(size_t n)
    {
        auto b = parent.allocateZeroed(n);
        mixin(_processAndReturnAllocateResult);
    }

    /**
    Forwards to $(D parent.expand(b, delta)).
    */
    static if (hasMember!(Allocator, "expand"))
    bool expand(ref void[] b, size_t delta)
    {
        auto result = parent.expand(b, delta);
        if (result && b)
        {
            () @trusted { parent.prefix(b).length = b.length; }();
        }
        return result;
    }

    /**
    Reallocates `b` to new size `s`.
    */
    bool reallocate(ref void[] b, size_t s)
    {
        // Remove from list
        if (b.ptr)
        {
            Node* n = & parent.prefix(b);
            if (n.prev) n.prev.next = n.next;
            else root = n.next;
            if (n.next) n.next.prev = n.prev;
        }
        auto result = parent.reallocate(b, s);
        // Add back to list
        if (b.ptr)
        {
            Node* n = & parent.prefix(b);
            n.prev = null;
            n.next = root;
            n.length = s;
            if (root) root.prev = n;
            root = n;
        }
        return result;
    }

    /**
    Forwards to `parent.owns(b)`.
    */
    static if (hasMember!(Allocator, "owns"))
    Ternary owns(void[] b)
    {
        return parent.owns(b);
    }

    /**
    Deallocates `b`.
    */
    static if (hasMember!(Allocator, "deallocate"))
    bool deallocate(void[] b)
    {
        // Remove from list
        if (b.ptr)
        {
            Node* n = & parent.prefix(b);
            if (n.prev) n.prev.next = n.next;
            else root = n.next;
            if (n.next) n.next.prev = n.prev;
        }
        return parent.deallocate(b);
    }

    /**
    Deallocates all memory allocated.
    */
    bool deallocateAll()
    {
        bool result = true;
        for (auto n = root; n; )
        {
            void* p = n + 1;
            auto length = n.length;
            n = n.next;
            if (!parent.deallocate(p[0 .. length]))
                result = false;
        }
        root = null;
        return result;
    }

    /**
    Returns `Ternary.yes` if this allocator is not responsible for any memory,
    `Ternary.no` otherwise. (Never returns `Ternary.unknown`.)
    */
    pure nothrow @safe @nogc
    Ternary empty() const
    {
        return Ternary(root is null);
    }
}

///
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;
    ScopedAllocator!Mallocator alloc;
    assert(alloc.empty == Ternary.yes);
    const b = alloc.allocate(10);
    assert(b.length == 10);
    assert(alloc.empty == Ternary.no);
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    testAllocator!(() => ScopedAllocator!GCAllocator());
}

@system unittest // https://issues.dlang.org/show_bug.cgi?id=16046
{
    import std.exception;
    import std.experimental.allocator;
    import std.experimental.allocator.mallocator;
    ScopedAllocator!Mallocator alloc;
    auto foo = alloc.make!int(1).enforce;
    auto bar = alloc.make!int(2).enforce;
    alloc.dispose(foo);
    alloc.dispose(bar); // segfault here
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    ScopedAllocator!GCAllocator a;

    assert(__traits(compiles, (() nothrow @safe @nogc => a.goodAllocSize(0))()));

    // Ensure deallocate inherits from parent allocators
    auto b = a.allocate(42);
    assert(b.length == 42);
    () nothrow @nogc { a.deallocate(b); }();
}

// Test that deallocateAll infers from parent
@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;

    ScopedAllocator!(BorrowedRegion!()) a;
    a.parent.parent = BorrowedRegion!()(new ubyte[1024 * 64]);
    auto b = a.allocate(42);
    assert(b.length == 42);
    assert((() pure nothrow @safe @nogc => a.expand(b, 22))());
    assert(b.length == 64);
    assert((() nothrow @nogc => a.reallocate(b, 100))());
    assert(b.length == 100);
    assert((() nothrow @nogc => a.deallocateAll())());
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;

    auto a = Region!(Mallocator)(1024 * 64);
    auto b = a.allocate(42);
    assert(b.length == 42);
    assert((() pure nothrow @safe @nogc => a.expand(b, 22))());
    assert(b.length == 64);
    assert((() pure nothrow @safe @nogc => a.owns(b))() == Ternary.yes);
    assert((() nothrow @nogc => a.reallocate(b, 100))());
    assert(b.length == 100);
    assert((() pure nothrow @safe @nogc => a.owns(b))() == Ternary.yes);
    assert((() pure nothrow @safe @nogc => a.owns(null))() == Ternary.no);
}

// Test empty
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Ternary;
    ScopedAllocator!Mallocator alloc;

    assert((() pure nothrow @safe @nogc => alloc.empty)() == Ternary.yes);
    const b = alloc.allocate(10);
    assert((() pure nothrow @safe @nogc => alloc.empty)() == Ternary.no);
}

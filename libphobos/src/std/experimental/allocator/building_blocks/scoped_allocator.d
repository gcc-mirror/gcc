///
module std.experimental.allocator.building_blocks.scoped_allocator;

import std.experimental.allocator.common;

/**

$(D ScopedAllocator) delegates all allocation requests to $(D ParentAllocator).
When destroyed, the $(D ScopedAllocator) object automatically calls $(D
deallocate) for all memory allocated through its lifetime. (The $(D
deallocateAll) function is also implemented with the same semantics.)

$(D deallocate) is also supported, which is where most implementation effort
and overhead of $(D ScopedAllocator) go. If $(D deallocate) is not needed, a
simpler design combining $(D AllocatorList) with $(D Region) is recommended.

*/
struct ScopedAllocator(ParentAllocator)
{
    @system unittest
    {
        testAllocator!(() => ScopedAllocator());
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
    If $(D ParentAllocator) is stateful, $(D parent) is a property giving access
    to an $(D AffixAllocator!ParentAllocator). Otherwise, $(D parent) is an alias for `AffixAllocator!ParentAllocator.instance`.
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
    $(D ScopedAllocator) is not copyable.
    */
    @disable this(this);

    /**
    $(D ScopedAllocator)'s destructor releases all memory allocated during its
    lifetime.
    */
    ~this()
    {
        deallocateAll;
    }

    /// Alignment offered
    enum alignment = Allocator.alignment;

    /**
    Forwards to $(D parent.goodAllocSize) (which accounts for the management
    overhead).
    */
    size_t goodAllocSize(size_t n)
    {
        return parent.goodAllocSize(n);
    }

    /**
    Allocates memory. For management it actually allocates extra memory from
    the parent.
    */
    void[] allocate(size_t n)
    {
        auto b = parent.allocate(n);
        if (!b.ptr) return b;
        Node* toInsert = & parent.prefix(b);
        toInsert.prev = null;
        toInsert.next = root;
        toInsert.length = n;
        assert(!root || !root.prev);
        if (root) root.prev = toInsert;
        root = toInsert;
        return b;
    }

    /**
    Forwards to $(D parent.expand(b, delta)).
    */
    static if (hasMember!(Allocator, "expand"))
    bool expand(ref void[] b, size_t delta)
    {
        auto result = parent.expand(b, delta);
        if (result && b.ptr)
        {
            parent.prefix(b).length = b.length;
        }
        return result;
    }

    /**
    Reallocates $(D b) to new size $(D s).
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
    Forwards to $(D parent.owns(b)).
    */
    static if (hasMember!(Allocator, "owns"))
    Ternary owns(void[] b)
    {
        return parent.owns(b);
    }

    /**
    Deallocates $(D b).
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

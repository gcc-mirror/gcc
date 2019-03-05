///
module std.experimental.allocator.building_blocks.free_tree;

import std.experimental.allocator.common;

//debug = std_experimental_allocator_free_tree;

/**

The Free Tree allocator, stackable on top of any other allocator, bears
similarity with the free list allocator. Instead of a singly-linked list of
previously freed blocks, it maintains a binary search tree. This allows the
Free Tree allocator to manage blocks of arbitrary lengths and search them
efficiently.

Common uses of $(D FreeTree) include:

$(UL
$(LI Adding $(D deallocate) capability to an allocator that lacks it (such as simple regions).)
$(LI Getting the benefits of multiple adaptable freelists that do not need to
be tuned for one specific size but insted automatically adapts itself to
frequently used sizes.)
)

The free tree has special handling of duplicates (a singly-linked list per
node) in anticipation of large number of duplicates. Allocation time from the
free tree is expected to be $(BIGOH log n) where $(D n) is the number of
distinct sizes (not total nodes) kept in the free tree.

Allocation requests first search the tree for a buffer of suitable size
deallocated in the past. If a match is found, the node is removed from the tree
and the memory is returned. Otherwise, the allocation is directed to $(D
ParentAllocator). If at this point $(D ParentAllocator) also fails to allocate,
$(D FreeTree) frees everything and then tries the parent allocator again.

Upon deallocation, the deallocated block is inserted in the internally
maintained free tree (not returned to the parent). The free tree is not kept
balanced. Instead, it has a last-in-first-out flavor because newly inserted
blocks are rotated to the root of the tree. That way allocations are cache
friendly and also frequently used sizes are more likely to be found quickly,
whereas seldom used sizes migrate to the leaves of the tree.

$(D FreeTree) rounds up small allocations to at least $(D 4 * size_t.sizeof),
which on 64-bit system is one cache line size. If very small objects need to
be efficiently allocated, the $(D FreeTree) should be fronted with an
appropriate small object allocator.

The following methods are defined if $(D ParentAllocator) defines them, and forward to it: $(D allocateAll), $(D expand), $(D owns), $(D reallocate).
*/
struct FreeTree(ParentAllocator)
{
    static assert(ParentAllocator.alignment % size_t.alignof == 0,
        "FreeTree must be on top of a word-aligned allocator");

    import std.algorithm.comparison : min, max;
    import std.algorithm.mutation : swap;
    import std.traits : hasMember;

    // State
    static if (stateSize!ParentAllocator) private ParentAllocator parent;
    else private alias parent = ParentAllocator.instance;
    private Node* root; // that's the entire added state

    private struct Node
    {
        Node*[2] kid;
        Node* sibling;
        size_t size;
        ref Node* left() { return kid[0]; }
        ref Node* right() { return kid[1]; }
    }

    // Removes "which" from the tree, returns the memory it occupied
    private void[] remove(ref Node* which)
    {
        assert(which);
        assert(!which.sibling);
        auto result = (cast(ubyte*) which)[0 .. which.size];
        if (!which.right) which = which.left;
        else if (!which.left) which = which.right;
        else
        {
            // result has two kids
            static bool toggler;
            // Crude randomization: alternate left/right choices
            toggler = !toggler;
            auto newRoot = which.kid[toggler], orphan = which.kid[!toggler];
            which = newRoot;
            for (Node* n = void; (n = newRoot.kid[!toggler]) !is null; )
            {
                newRoot = n;
            }
            newRoot.kid[!toggler] = orphan;
        }
        return result;
    }

    private void[] findAndRemove(ref Node* n, size_t s)
    {
        if (!n) return null;
        if (s == n.size)
        {
            if (auto sis = n.sibling)
            {
                // Nice, give away one from the freelist
                auto result = (cast(ubyte*) sis)[0 .. sis.size];
                n.sibling = sis.sibling;
                return result;
            }
            return remove(n);
        }
        return findAndRemove(n.kid[s > n.size], s);
    }

    debug(std_experimental_allocator_free_tree)
    private void dump()
    {
        import std.stdio : writef, writefln, writeln;
        writeln(typeof(this).stringof, "@", &this, " {");
        scope(exit) writeln("}");

        if (!root) return;

        static void recurse(Node* n, uint indent = 4)
        {
            if (!n)
            {
                writefln("%*s(null)", indent, "");
                return;
            }
            for (auto sis = n; sis; sis = sis.sibling)
            {
                writef("%*s%x (%s bytes) ", indent, "",
                    cast(void*) n, n.size);
            }
            writeln;
            if (!n.left && !n.right) return;
            recurse(n.left, indent + 4);
            recurse(n.right, indent + 4);
        }
        recurse(root);
    }

    private string formatSizes()
    {
        string result = "(";
        void recurse(Node* n)
        {
            if (!n)
            {
                result ~= "_";
                return;
            }
            import std.conv : to;
            result ~= to!string(n.size);
            for (auto sis = n.sibling; sis; sis = sis.sibling)
            {
                result ~= "+moar";
            }
            if (n.left || n.right)
            {
                result ~= " (";
                recurse(n.left);
                result ~= ' ';
                recurse(n.right);
                result ~= ")";
            }
        }
        recurse(root);
        return result ~= ")";
    }

    private static void rotate(ref Node* parent, bool toRight)
    {
        assert(parent);
        auto opposing = parent.kid[!toRight];
        if (!opposing) return;
        parent.kid[!toRight] = opposing.kid[toRight];
        opposing.kid[toRight] = parent;
        parent = opposing;
    }

    // Inserts which into the tree, making it the new root
    private void insertAsRoot(Node* which)
    {
        assert(which);
        debug(std_experimental_allocator_free_tree)
        {
            assertValid;
            scope(exit) assertValid;
        }

        static void recurse(ref Node* where, Node* which)
        {
            if (!where)
            {
                where = which;
                which.left = null;
                which.right = null;
                which.sibling = null;
                return;
            }
            if (which.size == where.size)
            {
                // Special handling of duplicates
                which.sibling = where.sibling;
                where.sibling = which;
                which.left = null;
                which.right = null;
                return;
            }
            bool goRight = which.size > where.size;
            recurse(where.kid[goRight], which);
            rotate(where, !goRight);
        }
        recurse(root, which);
    }

    private void assertValid()
    {
        debug(std_experimental_allocator_free_tree)
        {
            static bool isBST(Node* n, size_t lb = 0, size_t ub = size_t.max)
            {
                if (!n) return true;
                for (auto sis = n.sibling; sis; sis = sis.sibling)
                {
                    assert(n.size == sis.size);
                    assert(sis.left is null);
                    assert(sis.right is null);
                }
                return lb < n.size && n.size <= ub
                    && isBST(n.left, lb, min(ub, n.size))
                    && isBST(n.right, max(lb, n.size), ub);
            }
            if (isBST(root)) return;
            dump;
            assert(0);
        }
    }

    /**
    The $(D FreeTree) is word aligned.
    */
    enum uint alignment = size_t.alignof;

    /**
    The $(D FreeTree) allocator is noncopyable.
    */
    this(this) @disable;

    /**
    The destructor of $(D FreeTree) releases all memory back to the parent
    allocator.
    */
    static if (hasMember!(ParentAllocator, "deallocate"))
    ~this()
    {
        clear;
    }

    /**
    Returns $(D parent.goodAllocSize(max(Node.sizeof, s))).
    */
    static if (stateSize!ParentAllocator)
        size_t goodAllocSize(size_t s)
        {
            return parent.goodAllocSize(max(Node.sizeof, s));
        }
    else
        static size_t goodAllocSize(size_t s)
        {
            return parent.goodAllocSize(max(Node.sizeof, s));
        }

    /**

    Allocates $(D n) bytes of memory. First consults the free tree, and returns
    from it if a suitably sized block is found. Otherwise, the parent allocator
    is tried. If allocation from the parent succeeds, the allocated block is
    returned. Otherwise, the free tree tries an alternate strategy: If $(D
    ParentAllocator) defines $(D deallocate), $(D FreeTree) releases all of its
    contents and tries again.

    TODO: Splitting and coalescing should be implemented if $(D ParentAllocator) does not defined $(D deallocate).

    */
    void[] allocate(size_t n)
    {
        assertValid;
        if (n == 0) return null;

        immutable s = goodAllocSize(n);

        // Consult the free tree.
        auto result = findAndRemove(root, s);
        if (result.ptr) return result.ptr[0 .. n];

        // No block found, try the parent allocator.
        result = parent.allocate(s);
        if (result.ptr) return result.ptr[0 .. n];

        // Parent ran out of juice, desperation mode on
        static if (hasMember!(ParentAllocator, "deallocate"))
        {
            clear;
            // Try parent allocator again.
            result = parent.allocate(s);
            if (result.ptr) return result.ptr[0 .. n];
            return null;
        }
        else
        {
            // TODO: get smart here
            return null;
        }
    }

    // Forwarding methods
    mixin(forwardToMember("parent",
        "allocateAll", "expand", "owns", "reallocate"));

    /** Places $(D b) into the free tree. */
    bool deallocate(void[] b)
    {
        if (!b.ptr) return true;
        auto which = cast(Node*) b.ptr;
        which.size = goodAllocSize(b.length);
        // deliberately don't initialize which.left and which.right
        assert(which.size >= Node.sizeof);
        insertAsRoot(which);
        return true;
    }

    @system unittest // test a few simple configurations
    {
        import std.experimental.allocator.gc_allocator;
        FreeTree!GCAllocator a;
        auto b1 = a.allocate(10000);
        auto b2 = a.allocate(20000);
        auto b3 = a.allocate(30000);
        assert(b1.ptr && b2.ptr && b3.ptr);
        a.deallocate(b1);
        a.deallocate(b3);
        a.deallocate(b2);
        assert(a.formatSizes == "(20480 (12288 32768))", a.formatSizes);

        b1 = a.allocate(10000);
        assert(a.formatSizes == "(20480 (_ 32768))", a.formatSizes);
        b1 = a.allocate(30000);
        assert(a.formatSizes == "(20480)", a.formatSizes);
        b1 = a.allocate(20000);
        assert(a.formatSizes == "(_)", a.formatSizes);
    }

    @system unittest // build a complex free tree
    {
        import std.experimental.allocator.gc_allocator, std.range;
        FreeTree!GCAllocator a;
        uint[] sizes = [3008,704,1856,576,1632,672,832,1856,1120,2656,1216,672,
            448,992,2400,1376,2688,2656,736,1440];
        void[][] allocs;
        foreach (s; sizes)
            allocs ~= a.allocate(s);
        foreach_reverse (b; allocs)
        {
            assert(b.ptr);
            a.deallocate(b);
        }
        a.assertValid;
        allocs = null;
        foreach (s; sizes)
            allocs ~= a.allocate(s);
        assert(a.root is null);
        a.assertValid;
    }

    /** Defined if $(D ParentAllocator.deallocate) exists, and returns to it
    all memory held in the free tree. */
    static if (hasMember!(ParentAllocator, "deallocate"))
    void clear()
    {
        void recurse(Node* n)
        {
            if (!n) return;
            recurse(n.left);
            recurse(n.right);
            parent.deallocate((cast(ubyte*) n)[0 .. n.size]);
        }
        recurse(root);
        root = null;
    }

    /**

    Defined if $(D ParentAllocator.deallocateAll) exists, and forwards to it.
    Also nullifies the free tree (it's assumed the parent frees all memory
    stil managed by the free tree).

    */
    static if (hasMember!(ParentAllocator, "deallocateAll"))
    bool deallocateAll()
    {
        // This is easy, just nuke the root and deallocate all from the
        // parent
        root = null;
        return parent.deallocateAll;
    }
}

@system unittest
{
    import std.experimental.allocator.gc_allocator;
    testAllocator!(() => FreeTree!GCAllocator());
}

@system unittest // issue 16506
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;

    static void f(ParentAllocator)(size_t sz)
    {
        static FreeTree!ParentAllocator myAlloc;
        byte[] _payload = cast(byte[]) myAlloc.allocate(sz);
        assert(_payload, "_payload is null");
        _payload[] = 0;
        myAlloc.deallocate(_payload);
    }

    f!Mallocator(33);
    f!Mallocator(43);
    f!GCAllocator(1);
}

@system unittest // issue 16507
{
    static struct MyAllocator
    {
        byte dummy;
        static bool alive = true;
        void[] allocate(size_t s) { return new byte[](s); }
        bool deallocate(void[] ) { if (alive) assert(false); return true; }
        enum alignment = size_t.sizeof;
    }

    FreeTree!MyAllocator ft;
    void[] x = ft.allocate(1);
    ft.deallocate(x);
    ft.allocate(1000);
    MyAllocator.alive = false;
}

@system unittest // "desperation mode"
{
    uint myDeallocCounter = 0;

    struct MyAllocator
    {
        byte[] allocation;
        void[] allocate(size_t s)
        {
            if (allocation.ptr) return null;
            allocation = new byte[](s);
            return allocation;
        }
        bool deallocate(void[] )
        {
            ++myDeallocCounter;
            allocation = null;
            return true;
        }
        enum alignment = size_t.sizeof;
    }

    FreeTree!MyAllocator ft;
    void[] x = ft.allocate(1);
    ft.deallocate(x);
    assert(myDeallocCounter == 0);
    x = ft.allocate(1000); // Triggers "desperation mode".
    assert(myDeallocCounter == 1);
    assert(x.ptr);
    void[] y = ft.allocate(1000); /* Triggers "desperation mode" but there's
        nothing to deallocate so MyAllocator can't deliver. */
    assert(myDeallocCounter == 1);
    assert(y.ptr is null);
}

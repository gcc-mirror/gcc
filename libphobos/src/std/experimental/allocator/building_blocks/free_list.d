// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/free_list.d)
*/
module std.experimental.allocator.building_blocks.free_list;

import std.experimental.allocator.common;
import std.typecons : Flag, Yes, No;

/**

$(HTTP en.wikipedia.org/wiki/Free_list, Free list allocator), stackable on top of
another allocator. Allocation requests between `min` and `max` bytes are
rounded up to `max` and served from a singly-linked list of buffers
deallocated in the past. All other allocations are directed to $(D
ParentAllocator). Due to the simplicity of free list management, allocations
from the free list are fast. If `adaptive` is set to `Yes.adaptive`,
the free list gradually reduces its size if allocations tend to use the parent
allocator much more than the lists' available nodes.

One instantiation is of particular interest: $(D FreeList!(0, unbounded)) puts
every deallocation in the freelist, and subsequently serves any allocation from
the freelist (if not empty). There is no checking of size matching, which would
be incorrect for a freestanding allocator but is both correct and fast when an
owning allocator on top of the free list allocator (such as `Segregator`) is
already in charge of handling size checking.

The following methods are defined if `ParentAllocator` defines them, and
forward to it: `expand`, `owns`, `reallocate`.

*/
struct FreeList(ParentAllocator,
    size_t minSize, size_t maxSize = minSize,
    Flag!"adaptive" adaptive = No.adaptive)
{
    import std.conv : text;
    import std.exception : enforce;
    import std.traits : hasMember;
    import std.typecons : Ternary;
    import std.experimental.allocator.building_blocks.null_allocator : NullAllocator;

    static assert(minSize != unbounded, "Use minSize = 0 for no low bound.");
    static assert(maxSize >= (void*).sizeof,
        "Maximum size must accommodate a pointer.");

    private enum unchecked = minSize == 0 && maxSize == unbounded;

    private enum hasTolerance = !unchecked && (minSize != maxSize
        || maxSize == chooseAtRuntime);

    static if (minSize == chooseAtRuntime)
    {
        /**
        Returns the smallest allocation size eligible for allocation from the
        freelist. (If $(D minSize != chooseAtRuntime), this is simply an alias
        for `minSize`.)
        */
        @property size_t min() const
        {
            assert(_min != chooseAtRuntime);
            return _min;
        }
        /**
        If `FreeList` has been instantiated with $(D minSize ==
        chooseAtRuntime), then the `min` property is writable. Setting it
        must precede any allocation.

        Params:
        low = new value for `min`

        Precondition: $(D low <= max), or $(D maxSize == chooseAtRuntime) and
        `max` has not yet been initialized. Also, no allocation has been
        yet done with this allocator.

        Postcondition: $(D min == low)
        */
        @property void min(size_t low)
        {
            assert(low <= max || max == chooseAtRuntime);
            minimize;
            _min = low;
        }
    }
    else
    {
        alias min = minSize;
    }

    static if (maxSize == chooseAtRuntime)
    {
        /**
        Returns the largest allocation size eligible for allocation from the
        freelist. (If $(D maxSize != chooseAtRuntime), this is simply an alias
        for `maxSize`.) All allocation requests for sizes greater than or
        equal to `min` and less than or equal to `max` are rounded to $(D
        max) and forwarded to the parent allocator. When the block fitting the
        same constraint gets deallocated, it is put in the freelist with the
        allocated size assumed to be `max`.
        */
        @property size_t max() const { return _max; }

        /**
        If `FreeList` has been instantiated with $(D maxSize ==
        chooseAtRuntime), then the `max` property is writable. Setting it
        must precede any allocation.

        Params:
        high = new value for `max`

        Precondition: $(D high >= min), or $(D minSize == chooseAtRuntime) and
        `min` has not yet been initialized. Also $(D high >= (void*).sizeof). Also, no allocation has been yet done with this allocator.

        Postcondition: $(D max == high)
        */
        @property void max(size_t high)
        {
            assert((high >= min || min == chooseAtRuntime)
                && high >= (void*).sizeof);
            minimize;
            _max = high;
        }

        @system unittest
        {
            import std.experimental.allocator.common : chooseAtRuntime;
            import std.experimental.allocator.mallocator : Mallocator;

            FreeList!(Mallocator, chooseAtRuntime, chooseAtRuntime) a;
            a.min = 64;
            a.max = 128;
            assert(a.min == 64);
            assert(a.max == 128);
        }
    }
    else
    {
        alias max = maxSize;
    }

    private bool tooSmall(size_t n) const
    {
        static if (minSize == 0) return false;
        else return n < min;
    }

    private bool tooLarge(size_t n) const
    {
        static if (maxSize == unbounded) return false;
        else return n > max;
    }

    private bool freeListEligible(size_t n) const
    {
        static if (unchecked)
        {
            return true;
        }
        else
        {
            static if (minSize == 0)
            {
                if (!n) return false;
            }
            static if (minSize == maxSize && minSize != chooseAtRuntime)
                return n == maxSize;
            else
                return !tooSmall(n) && !tooLarge(n);
        }
    }

    static if (!unchecked)
    private void[] blockFor(Node* p)
    {
        assert(p);
        return (cast(void*) p)[0 .. max];
    }

    // statistics
    static if (adaptive == Yes.adaptive)
    {
        private enum double windowLength = 1000.0;
        private enum double tooFewMisses = 0.01;
        private double probMiss = 1.0; // start with a high miss probability
        private uint accumSamples, accumMisses;

        void updateStats()
        {
            assert(accumSamples >= accumMisses);
            /*
            Given that for the past windowLength samples we saw misses with
            estimated probability probMiss, and assuming the new sample wasMiss or
            not, what's the new estimated probMiss?
            */
            probMiss = (probMiss * windowLength + accumMisses)
                / (windowLength + accumSamples);
            assert(probMiss <= 1.0);
            accumSamples = 0;
            accumMisses = 0;
            // If probability to miss is under x%, yank one off the freelist
            static if (!unchecked)
            {
                if (probMiss < tooFewMisses && _root)
                {
                    auto b = blockFor(_root);
                    _root = _root.next;
                    parent.deallocate(b);
                }
            }
        }
    }

    private struct Node { Node* next; }
    static assert(ParentAllocator.alignment >= Node.alignof);

    // state
    /**
    The parent allocator. Depending on whether `ParentAllocator` holds state
    or not, this is a member variable or an alias for
    `ParentAllocator.instance`.
    */
    static if (stateSize!ParentAllocator) ParentAllocator parent;
    else alias parent = ParentAllocator.instance;
    private Node* root;
    static if (minSize == chooseAtRuntime) private size_t _min = chooseAtRuntime;
    static if (maxSize == chooseAtRuntime) private size_t _max = chooseAtRuntime;

    /**
    Alignment offered.
    */
    alias alignment = ParentAllocator.alignment;

    /**
    If $(D maxSize == unbounded), returns  `parent.goodAllocSize(bytes)`.
    Otherwise, returns `max` for sizes in the interval $(D [min, max]), and
    `parent.goodAllocSize(bytes)` otherwise.

    Precondition:
    If set at runtime, `min` and/or `max` must be initialized
    appropriately.

    Postcondition:
    $(D result >= bytes)
    */
    size_t goodAllocSize(size_t bytes)
    {
        assert(minSize != chooseAtRuntime && maxSize != chooseAtRuntime);
        static if (maxSize != unbounded)
        {
            if (freeListEligible(bytes))
            {
                assert(parent.goodAllocSize(max) == max,
                    text("Wrongly configured freelist: maximum should be ",
                        parent.goodAllocSize(max), " instead of ", max));
                return max;
            }
        }
        return parent.goodAllocSize(bytes);
    }

    private void[] allocateEligible(string fillMode)(size_t bytes)
    if (fillMode == "void" || fillMode == "zero")
    {
        enum bool isFillZero = fillMode == "zero";
        assert(bytes);
        if (root)
        {
            // faster
            auto result = (cast(ubyte*) root)[0 .. bytes];
            root = root.next;
            static if (isFillZero) result[0 .. bytes] = 0;
            return result;
        }
        // slower
        static if (hasTolerance)
        {
            immutable toAllocate = max;
        }
        else
        {
            alias toAllocate = bytes;
        }
        assert(toAllocate == max || max == unbounded);
        static if (isFillZero)
            auto result = parent.allocateZeroed(toAllocate);
        else
            auto result = parent.allocate(toAllocate);
        static if (hasTolerance)
        {
            if (result) result = result.ptr[0 .. bytes];
        }
        static if (adaptive == Yes.adaptive)
        {
            ++accumMisses;
            updateStats;
        }
        return result;
    }

    /**
    Allocates memory either off of the free list or from the parent allocator.
    If `n` is within $(D [min, max]) or if the free list is unchecked
    ($(D minSize == 0 && maxSize == size_t.max)), then the free list is
    consulted first. If not empty (hit), the block at the front of the free
    list is removed from the list and returned. Otherwise (miss), a new block
    of `max` bytes is allocated, truncated to `n` bytes, and returned.

    Params:
    n = number of bytes to allocate

    Returns:
    The allocated block, or `null`.

    Precondition:
    If set at runtime, `min` and/or `max` must be initialized
    appropriately.

    Postcondition: $(D result.length == bytes || result is null)
    */
    void[] allocate(size_t n)
    {
        static if (adaptive == Yes.adaptive) ++accumSamples;
        assert(n < size_t.max / 2);
        // fast path
        if (freeListEligible(n))
        {
            return allocateEligible!"void"(n);
        }
        // slower
        static if (adaptive == Yes.adaptive)
        {
            updateStats;
        }
        return parent.allocate(n);
    }

    static if (hasMember!(ParentAllocator, "allocateZeroed"))
    package(std) void[] allocateZeroed()(size_t n)
    {
        static if (adaptive == Yes.adaptive) ++accumSamples;
        assert(n < size_t.max / 2);
        // fast path
        if (freeListEligible(n))
        {
            return allocateEligible!"zero"(n);
        }
        // slower
        static if (adaptive == Yes.adaptive)
        {
            updateStats;
        }
        return parent.allocateZeroed(n);
    }

    // Forwarding methods
    mixin(forwardToMember("parent",
        "expand", "owns", "reallocate"));

    /**
    If `block.length` is within $(D [min, max]) or if the free list is
    unchecked ($(D minSize == 0 && maxSize == size_t.max)), then inserts the
    block at the front of the free list. For all others, forwards to $(D
    parent.deallocate) if `Parent.deallocate` is defined.

    Params:
    block = Block to deallocate.

    Precondition:
    If set at runtime, `min` and/or `max` must be initialized
    appropriately. The block must have been allocated with this
    freelist, and no dynamic changing of `min` or `max` is allowed to
    occur between allocation and deallocation.
    */
    bool deallocate(void[] block)
    {
        if (freeListEligible(block.length))
        {
            if (min == 0)
            {
                // In this case a null pointer might have made it this far.
                if (block is null) return true;
            }
            auto t = root;
            root = cast(Node*) block.ptr;
            root.next = t;
            return true;
        }
        static if (hasMember!(ParentAllocator, "deallocate"))
            return parent.deallocate(block);
        else
            return false;
    }

    /**
    Defined only if `ParentAllocator` defines `deallocateAll`. If so,
    forwards to it and resets the freelist.
    */
    static if (hasMember!(ParentAllocator, "deallocateAll"))
    bool deallocateAll()
    {
        root = null;
        return parent.deallocateAll();
    }

    /**
    Nonstandard function that minimizes the memory usage of the freelist by
    freeing each element in turn. Defined only if `ParentAllocator` defines
    `deallocate`. $(D FreeList!(0, unbounded)) does not have this function.
    */
    static if (hasMember!(ParentAllocator, "deallocate") && !unchecked)
    void minimize()
    {
        while (root)
        {
            auto nuke = blockFor(root);
            root = root.next;
            parent.deallocate(nuke);
        }
    }

    /**
    If `ParentAllocator` defines `deallocate`, the list frees all nodes
    on destruction. $(D FreeList!(0, unbounded)) does not deallocate the memory
    on destruction.
    */
    static if (!is(ParentAllocator == NullAllocator) &&
        hasMember!(ParentAllocator, "deallocate") && !unchecked)
    ~this()
    {
        minimize();
    }
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.building_blocks.stats_collector
        : StatsCollector, Options;

    struct StatsCollectorWrapper {
        ~this()
        {
            // buf2 should still be around and buf1 deallocated
            assert(parent.numDeallocate == 1);
            assert(parent.bytesUsed == 16);
        }
        static StatsCollector!(Mallocator, Options.all) parent;
        alias parent this;
    }

    FreeList!(StatsCollectorWrapper, 16, 16) fl;
    auto buf1 = fl.allocate(16);
    auto buf2 = fl.allocate(16);
    assert(fl.parent.bytesUsed == 32);

    // After this, the list has 1 node, so no actual deallocation by Mallocator
    fl.deallocate(buf1);
    assert(fl.parent.bytesUsed == 32);

    // Destruction should only deallocate the node
    destroy(fl);
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    FreeList!(GCAllocator, 0, 8) fl;
    assert(fl.root is null);
    auto b1 = fl.allocate(7);
    fl.allocate(8);
    assert(fl.root is null);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { fl.deallocate(b1); }();
    assert(fl.root !is null);
    fl.allocate(8);
    assert(fl.root is null);
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    FreeList!(GCAllocator, 0, 16) fl;
    // Not @nogc because of std.conv.text
    assert((() nothrow @safe /*@nogc*/ => fl.goodAllocSize(1))() == 16);
}

// Test that deallocateAll infers from parent
@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;

    auto fl = FreeList!(BorrowedRegion!(), 0, 16)(BorrowedRegion!()(new ubyte[1024 * 64]));
    auto b = fl.allocate(42);
    assert(b.length == 42);
    assert((() pure nothrow @safe @nogc => fl.expand(b, 48))());
    assert(b.length == 90);
    assert((() nothrow @nogc => fl.reallocate(b, 100))());
    assert(b.length == 100);
    assert((() nothrow @nogc => fl.deallocateAll())());
}

/**
Free list built on top of exactly one contiguous block of memory. The block is
assumed to have been allocated with `ParentAllocator`, and is released in
`ContiguousFreeList`'s destructor (unless `ParentAllocator` is $(D
NullAllocator)).

`ContiguousFreeList` has most advantages of `FreeList` but fewer
disadvantages. It has better cache locality because items are closer to one
another. It imposes less fragmentation on its parent allocator.

The disadvantages of `ContiguousFreeList` over `FreeList` are its pay
upfront model (as opposed to `FreeList`'s pay-as-you-go approach), and a
hard limit on the number of nodes in the list. Thus, a large number of long-
lived objects may occupy the entire block, making it unavailable for serving
allocations from the free list. However, an absolute cap on the free list size
may be beneficial.

The options $(D minSize == unbounded) and $(D maxSize == unbounded) are not
available for `ContiguousFreeList`.
*/
struct ContiguousFreeList(ParentAllocator,
     size_t minSize, size_t maxSize = minSize)
{
    import std.experimental.allocator.building_blocks.null_allocator
        : NullAllocator;
    import std.experimental.allocator.building_blocks.stats_collector
        : StatsCollector, Options;
    import std.traits : hasMember;
    import std.typecons : Ternary;

    alias Impl = FreeList!(NullAllocator, minSize, maxSize);
    enum unchecked = minSize == 0 && maxSize == unbounded;
    alias Node = Impl.Node;

    alias SParent = StatsCollector!(ParentAllocator, Options.bytesUsed);

    // state
    /**
    The parent allocator. Depending on whether `ParentAllocator` holds state
    or not, this is a member variable or an alias for
    `ParentAllocator.instance`.
    */
    SParent parent;
    FreeList!(NullAllocator, minSize, maxSize) fl;
    void[] support;
    size_t allocated;

    /// Alignment offered.
    enum uint alignment = (void*).alignof;

    private void initialize(ubyte[] buffer, size_t itemSize = fl.max)
    {
        assert(itemSize != unbounded && itemSize != chooseAtRuntime);
        assert(buffer.ptr.alignedAt(alignment));
        immutable available = buffer.length / itemSize;
        if (available == 0) return;
        support = buffer;
        fl.root = cast(Node*) buffer.ptr;
        auto past = cast(Node*) (buffer.ptr + available * itemSize);
        for (auto n = fl.root; ; )
        {
            auto next = cast(Node*) (cast(ubyte*) n + itemSize);
            if (next == past)
            {
                n.next = null;
                break;
            }
            assert(next < past);
            assert(n < next);
            n.next = next;
            n = next;
        }
    }

    /**
    Constructors setting up the memory structured as a free list.

    Params:
    buffer = Buffer to structure as a free list. If `ParentAllocator` is not
    `NullAllocator`, the buffer is assumed to be allocated by `parent`
    and will be freed in the destructor.
    parent = Parent allocator. For construction from stateless allocators, use
    their `instance` static member.
    bytes = Bytes (not items) to be allocated for the free list. Memory will be
    allocated during construction and deallocated in the destructor.
    max = Maximum size eligible for freelisting. Construction with this
    parameter is defined only if $(D maxSize == chooseAtRuntime) or $(D maxSize
    == unbounded).
    min = Minimum size eligible for freelisting. Construction with this
    parameter is defined only if $(D minSize == chooseAtRuntime). If this
    condition is met and no `min` parameter is present, `min` is
    initialized with `max`.
    */
    static if (!stateSize!ParentAllocator)
    this(ubyte[] buffer)
    {
        initialize(buffer);
    }

    /// ditto
    static if (stateSize!ParentAllocator)
    this(ParentAllocator parent, ubyte[] buffer)
    {
        initialize(buffer);
        this.parent = SParent(parent);
    }

    /// ditto
    static if (!stateSize!ParentAllocator)
    this(size_t bytes)
    {
        initialize(cast(ubyte[])(ParentAllocator.instance.allocate(bytes)));
    }

    /// ditto
    static if (stateSize!ParentAllocator)
    this(ParentAllocator parent, size_t bytes)
    {
        initialize(cast(ubyte[])(parent.allocate(bytes)));
        this.parent = SParent(parent);
    }

    /// ditto
    static if (!stateSize!ParentAllocator
        && (maxSize == chooseAtRuntime || maxSize == unbounded))
    this(size_t bytes, size_t max)
    {
        static if (maxSize == chooseAtRuntime) fl.max = max;
        static if (minSize == chooseAtRuntime) fl.min = max;
        initialize(cast(ubyte[])(parent.allocate(bytes)), max);
    }

    /// ditto
    static if (stateSize!ParentAllocator
        && (maxSize == chooseAtRuntime || maxSize == unbounded))
    this(ParentAllocator parent, size_t bytes, size_t max)
    {
        static if (maxSize == chooseAtRuntime) fl.max = max;
        static if (minSize == chooseAtRuntime) fl.min = max;
        initialize(cast(ubyte[])(parent.allocate(bytes)), max);
        this.parent = SParent(parent);
    }

    /// ditto
    static if (!stateSize!ParentAllocator
        && (maxSize == chooseAtRuntime || maxSize == unbounded)
        && minSize == chooseAtRuntime)
    this(size_t bytes, size_t min, size_t max)
    {
        static if (maxSize == chooseAtRuntime) fl.max = max;
        fl.min = min;
        initialize(cast(ubyte[])(parent.allocate(bytes)), max);
        static if (stateSize!ParentAllocator)
            this.parent = SParent(parent);
    }

    /// ditto
    static if (stateSize!ParentAllocator
        && (maxSize == chooseAtRuntime || maxSize == unbounded)
        && minSize == chooseAtRuntime)
    this(ParentAllocator parent, size_t bytes, size_t min, size_t max)
    {
        static if (maxSize == chooseAtRuntime) fl.max = max;
        fl.min = min;
        initialize(cast(ubyte[])(parent.allocate(bytes)), max);
        static if (stateSize!ParentAllocator)
            this.parent = SParent(parent);
    }

    /**
    If `n` is eligible for freelisting, returns `max`. Otherwise, returns
    `parent.goodAllocSize(n)`.

    Precondition:
    If set at runtime, `min` and/or `max` must be initialized
    appropriately.

    Postcondition:
    $(D result >= bytes)
    */
    size_t goodAllocSize(size_t n)
    {
        if (fl.freeListEligible(n)) return fl.max;
        return parent.goodAllocSize(n);
    }

    /**
    Allocate `n` bytes of memory. If `n` is eligible for freelist and the
    freelist is not empty, pops the memory off the free list. In all other
    cases, uses the parent allocator.
    */
    void[] allocate(size_t n)
    {
        auto result = fl.allocate(n);
        if (result)
        {
            // Only case we care about: eligible sizes allocated from us
            ++allocated;
            return result;
        }
        // All others, allocate from parent
        return parent.allocate(n);
    }

    /**
    Defined if `ParentAllocator` defines it. Checks whether the block
    belongs to this allocator.
    */
    static if (hasMember!(SParent, "owns") || unchecked)
    // Ternary owns(const void[] b) const ?
    Ternary owns(void[] b)
    {
        if ((() @trusted => support && b
                            && (&support[0] <= &b[0])
                            && (&b[0] < &support[0] + support.length)
            )())
            return Ternary.yes;
        static if (unchecked)
            return Ternary.no;
        else
            return parent.owns(b);
    }

    /**
    Deallocates `b`. If it's of eligible size, it's put on the free list.
    Otherwise, it's returned to `parent`.

    Precondition: `b` has been allocated with this allocator, or is $(D
    null).
    */
    bool deallocate(void[] b)
    {
        if (support.ptr <= b.ptr && b.ptr < support.ptr + support.length)
        {
            // we own this guy
            assert(fl.freeListEligible(b.length));
            assert(allocated);
            --allocated;
            // Put manually in the freelist
            auto t = fl.root;
            fl.root = cast(Node*) b.ptr;
            fl.root.next = t;
            return true;
        }
        return parent.deallocate(b);
    }

    /**
    Deallocates everything from the parent.
    */
    static if (hasMember!(ParentAllocator, "deallocateAll")
        && stateSize!ParentAllocator)
    bool deallocateAll()
    {
        bool result = fl.deallocateAll && parent.deallocateAll;
        allocated = 0;
        return result;
    }

    /**
    Returns `Ternary.yes` if no memory is currently allocated with this
    allocator, `Ternary.no` otherwise. This method never returns
    `Ternary.unknown`.
    */
    Ternary empty()
    {
        return Ternary(allocated == 0 && parent.bytesUsed == 0);
    }
}

///
@safe unittest
{
    import std.experimental.allocator.building_blocks.allocator_list
        : AllocatorList;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    import std.experimental.allocator.common : unbounded;

    alias ScalableFreeList = AllocatorList!((n) =>
        ContiguousFreeList!(GCAllocator, 0, unbounded)(4096)
    );
}

@system unittest
{
    import std.experimental.allocator.building_blocks.null_allocator
        : NullAllocator;
    import std.typecons : Ternary;
    alias A = ContiguousFreeList!(NullAllocator, 0, 64);
    auto a = A(new ubyte[1024]);

    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);

    assert((() pure nothrow @safe @nogc => a.goodAllocSize(15))() == 64);
    assert((() pure nothrow @safe @nogc => a.goodAllocSize(65))()
            == (() nothrow @safe @nogc => NullAllocator.instance.goodAllocSize(65))());

    auto b = a.allocate(100);
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);
    assert(b.length == 0);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { a.deallocate(b); }();
    b = a.allocate(64);
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.no);
    assert(b.length == 64);
    assert((() nothrow @safe @nogc => a.owns(b))() == Ternary.yes);
    assert((() nothrow @safe @nogc => a.owns(null))() == Ternary.no);
    () nothrow @nogc { a.deallocate(b); }();
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : Region;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;
    alias A = ContiguousFreeList!(Region!GCAllocator, 0, 64);
    auto a = A(Region!GCAllocator(1024 * 4), 1024);

    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);

    assert((() pure nothrow @safe @nogc => a.goodAllocSize(15))() == 64);
    assert((() pure nothrow @safe @nogc => a.goodAllocSize(65))()
            == (() pure nothrow @safe @nogc => a.parent.goodAllocSize(65))());

    auto b = a.allocate(100);
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.no);
    assert(a.allocated == 0);
    assert(b.length == 100);
    // Ensure deallocate inherits from parent
    assert((() nothrow @nogc => a.deallocate(b))());
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);
    b = a.allocate(64);
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.no);
    assert(b.length == 64);
    assert(a.reallocate(b, 100));
    assert(b.length == 100);
    assert((() nothrow @safe @nogc => a.owns(b))() == Ternary.yes);
    assert((() nothrow @safe @nogc => a.owns(null))() == Ternary.no);
    // Test deallocate infers from parent
    assert((() nothrow @nogc => a.deallocate(b))());
    assert((() nothrow @nogc => a.deallocateAll())());
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    alias A = ContiguousFreeList!(GCAllocator, 64, 64);
    auto a = A(1024);
    const b = a.allocate(100);
    assert(b.length == 100);
}

/**
FreeList shared across threads. Allocation and deallocation are lock-free. The
parameters have the same semantics as for `FreeList`.

`expand` is defined to forward to `ParentAllocator.expand`
(it must be also `shared`).
*/
struct SharedFreeList(ParentAllocator,
    size_t minSize, size_t maxSize = minSize, size_t approxMaxNodes = unbounded)
{
    import std.conv : text;
    import std.exception : enforce;
    import std.traits : hasMember;

    static if (hasMember!(ParentAllocator, "owns"))
    {
        import std.typecons : Ternary;
    }

    static assert(approxMaxNodes, "approxMaxNodes must not be null.");
    static assert(minSize != unbounded, "Use minSize = 0 for no low bound.");
    static assert(maxSize >= (void*).sizeof,
        "Maximum size must accommodate a pointer.");

    import core.atomic : atomicOp, cas;
    import core.internal.spinlock : SpinLock;

    private enum unchecked = minSize == 0 && maxSize == unbounded;

    static if (minSize != chooseAtRuntime)
    {
        alias min = minSize;
    }
    else
    {
        private shared size_t _min = chooseAtRuntime;
        @property size_t min() const shared
        {
            assert(_min != chooseAtRuntime);
            return _min;
        }
        @property void min(size_t x) shared
        {
            enforce(x <= max);
            enforce(cas(&_min, chooseAtRuntime, x),
                "SharedFreeList.min must be initialized exactly once.");
        }
        static if (maxSize == chooseAtRuntime)
        {
            // Both bounds can be set, provide one function for setting both in
            // one shot.
            void setBounds(size_t low, size_t high) shared
            {
                enforce(low <= high && high >= (void*).sizeof);
                enforce(cas(&_min, chooseAtRuntime, low),
                    "SharedFreeList.min must be initialized exactly once.");
                enforce(cas(&_max, chooseAtRuntime, high),
                    "SharedFreeList.max must be initialized exactly once.");
            }
        }
    }

    private bool tooSmall(size_t n) const shared
    {
        static if (minSize == 0) return false;
        else static if (minSize == chooseAtRuntime) return n < _min;
        else return n < minSize;
    }

    static if (maxSize != chooseAtRuntime)
    {
        alias max = maxSize;
    }
    else
    {
        private shared size_t _max = chooseAtRuntime;
        @property size_t max() const shared { return _max; }
        @property void max(size_t x) shared
        {
            enforce(x >= min && x >= (void*).sizeof);
            enforce(cas(&_max, chooseAtRuntime, x),
                "SharedFreeList.max must be initialized exactly once.");
        }
    }

    private bool tooLarge(size_t n) const shared
    {
        static if (maxSize == unbounded) return false;
        else static if (maxSize == chooseAtRuntime) return n > _max;
        else return n > maxSize;
    }

    private bool freeListEligible(size_t n) const shared
    {
        static if (minSize == maxSize && minSize != chooseAtRuntime)
            return n == maxSize;
        else return !tooSmall(n) && !tooLarge(n);
    }

    static if (approxMaxNodes != chooseAtRuntime)
    {
        alias approxMaxLength = approxMaxNodes;
    }
    else
    {
        private shared size_t _approxMaxLength = chooseAtRuntime;
        @property size_t approxMaxLength() const shared { return _approxMaxLength; }
        @property void approxMaxLength(size_t x) shared { _approxMaxLength = enforce(x); }
    }

    static if (approxMaxNodes != unbounded)
    {
        private shared size_t nodes;
        private void incNodes() shared
        {
            atomicOp!("+=")(nodes, 1);
        }
        private void decNodes() shared
        {
            assert(nodes);
            atomicOp!("-=")(nodes, 1);
        }
        private void resetNodes() shared
        {
            nodes = 0;
        }
        private bool nodesFull() shared
        {
            return nodes >= approxMaxLength;
        }
    }
    else
    {
        private static void incNodes() { }
        private static void decNodes() { }
        private static void resetNodes() { }
        private enum bool nodesFull = false;
    }

    version (StdDdoc)
    {
        /**
        Properties for getting (and possibly setting) the bounds. Setting bounds
        is allowed only once , and before any allocation takes place. Otherwise,
        the primitives have the same semantics as those of `FreeList`.
        */
        @property size_t min();
        /// Ditto
        @property void min(size_t newMinSize);
        /// Ditto
        @property size_t max();
        /// Ditto
        @property void max(size_t newMaxSize);
        /// Ditto
        void setBounds(size_t newMin, size_t newMax);

        /**
        Properties for getting (and possibly setting) the approximate maximum length of a shared freelist.
        */
        @property size_t approxMaxLength() const shared;
        /// ditto
        @property void approxMaxLength(size_t x) shared;
    }

    /**
    The parent allocator. Depending on whether `ParentAllocator` holds state
    or not, this is a member variable or an alias for
    `ParentAllocator.instance`.
    */
    static if (stateSize!ParentAllocator) shared ParentAllocator parent;
    else alias parent = ParentAllocator.instance;

    mixin(forwardToMember("parent", "expand"));

    private SpinLock lock;

    private struct Node { Node* next; }
    static assert(ParentAllocator.alignment >= Node.alignof);
    private Node* _root;

    /// Standard primitives.
    enum uint alignment = ParentAllocator.alignment;

    /// Ditto
    size_t goodAllocSize(size_t bytes) shared
    {
        if (freeListEligible(bytes)) return maxSize == unbounded ? bytes : max;
        return parent.goodAllocSize(bytes);
    }

    /// Ditto
    static if (hasMember!(ParentAllocator, "owns"))
    Ternary owns(const void[] b) shared const
    {
        return parent.owns(b);
    }

    /// Ditto
    static if (hasMember!(ParentAllocator, "reallocate"))
    bool reallocate(ref void[] b, size_t s) shared
    {
        return parent.reallocate(b, s);
    }

    /// Ditto
    void[] allocate(size_t bytes) shared
    {
        assert(bytes < size_t.max / 2);
        if (!freeListEligible(bytes)) return parent.allocate(bytes);
        if (maxSize != unbounded) bytes = max;

        // Try to pop off the freelist
        lock.lock();
        if (!_root)
        {
            lock.unlock();
            return allocateFresh(bytes);
        }
        else
        {
            auto oldRoot = _root;
            _root = _root.next;
            decNodes();
            lock.unlock();
            return (cast(ubyte*) oldRoot)[0 .. bytes];
        }
    }

    private void[] allocateFresh(const size_t bytes) shared
    {
        assert(bytes == max || max == unbounded);
        return parent.allocate(bytes);
    }

    /// Ditto
    bool deallocate(void[] b) shared
    {
        if (!nodesFull && freeListEligible(b.length))
        {
            auto newRoot = cast(shared Node*) b.ptr;
            lock.lock();
            newRoot.next = _root;
            _root = newRoot;
            incNodes();
            lock.unlock();
            return true;
        }
        static if (hasMember!(ParentAllocator, "deallocate"))
            return parent.deallocate(b);
        else
            return false;
    }

    /// Ditto
    bool deallocateAll() shared
    {
        bool result = false;
        lock.lock();
        scope(exit) lock.unlock();
        static if (hasMember!(ParentAllocator, "deallocateAll"))
        {
            result = parent.deallocateAll();
        }
        else static if (hasMember!(ParentAllocator, "deallocate"))
        {
            result = true;
            for (auto n = _root; n;)
            {
                auto tmp = n.next;
                if (!parent.deallocate((cast(ubyte*) n)[0 .. max]))
                    result = false;
                n = tmp;
            }
        }
        _root = null;
        resetNodes();
        return result;
    }

    /**
    Nonstandard function that minimizes the memory usage of the freelist by
    freeing each element in turn. Defined only if `ParentAllocator` defines
    `deallocate`.
    */
    static if (hasMember!(ParentAllocator, "deallocate") && !unchecked)
    void minimize() shared
    {
        lock.lock();
        scope(exit) lock.unlock();

        for (auto n = _root; n;)
        {
            auto tmp = n.next;
            parent.deallocate((cast(ubyte*) n)[0 .. max]);
            n = tmp;
        }

        _root = null;
        resetNodes();
    }
}

///
@safe unittest
{
    import std.experimental.allocator.common : chooseAtRuntime;
    import std.experimental.allocator.mallocator : Mallocator;

    shared SharedFreeList!(Mallocator, chooseAtRuntime, chooseAtRuntime) a;
    a.setBounds(64, 128);
    assert(a.max == 128);
    assert(a.min == 64);
}

///
@safe unittest
{
    import std.experimental.allocator.common : chooseAtRuntime;
    import std.experimental.allocator.mallocator : Mallocator;

    shared SharedFreeList!(Mallocator, 50, 50, chooseAtRuntime) a;
    // Set the maxSize first so setting the minSize doesn't throw
    a.approxMaxLength = 128;
    assert(a.approxMaxLength  == 128);
    a.approxMaxLength = 1024;
    assert(a.approxMaxLength  == 1024);
    a.approxMaxLength = 1;
    assert(a.approxMaxLength  == 1);
}

@system unittest
{
    import core.thread : ThreadGroup;
    import std.algorithm.comparison : equal;
    import std.experimental.allocator.mallocator : Mallocator;
    import std.range : repeat;

    static shared SharedFreeList!(Mallocator, 64, 128, 10) a;

    assert((() nothrow @safe @nogc => a.goodAllocSize(1))() == platformAlignment);

    auto b = a.allocate(96);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { a.deallocate(b); }();

    void fun()
    {
        auto b = cast(size_t[]) a.allocate(96);
        b[] = cast(size_t) &b;

        assert(b.equal(repeat(cast(size_t) &b, b.length)));
        () nothrow @nogc { a.deallocate(b); }();
    }

    auto tg = new ThreadGroup;
    foreach (i; 0 .. 20)
    {
        tg.create(&fun);
    }

    tg.joinAll();
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    static shared SharedFreeList!(Mallocator, 64, 128, 10) a;
    auto b = a.allocate(100);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { a.deallocate(b); }();
    assert(a.nodes == 1);
    b = [];
    assert((() nothrow @nogc => a.deallocateAll())());
    assert(a.nodes == 0);
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    static shared SharedFreeList!(Mallocator, 64, 128, 10) a;
    auto b = a.allocate(100);
    auto c = a.allocate(100);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { a.deallocate(c); }();
    assert(a.nodes == 1);
    c = [];
    a.minimize();
    assert(a.nodes == 0);
    () nothrow @nogc { a.deallocate(b); }();
    assert(a.nodes == 1);
    b = [];
    a.minimize();
    assert(a.nodes == 0);
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    static shared SharedFreeList!(Mallocator, 64, 128, 10) a;
    auto b = a.allocate(100);
    auto c = a.allocate(100);
    assert(a.nodes == 0);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { a.deallocate(b); }();
    () nothrow @nogc { a.deallocate(c); }();
    assert(a.nodes == 2);
    b = [];
    c = [];
    a.minimize();
    assert(a.nodes == 0);
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    shared SharedFreeList!(Mallocator, chooseAtRuntime, chooseAtRuntime) a;
    scope(exit) assert((() nothrow @nogc => a.deallocateAll())());
    auto c = a.allocate(64);
    assert((() nothrow @nogc => a.reallocate(c, 96))());
    assert(c.length == 96);
    // Ensure deallocate inherits from parent
    () nothrow @nogc { a.deallocate(c); }();
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    shared SharedFreeList!(Mallocator, chooseAtRuntime, chooseAtRuntime, chooseAtRuntime) a;
    scope(exit) assert((() nothrow @nogc => a.deallocateAll())());
    a.allocate(64);
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    shared SharedFreeList!(Mallocator, 30, 40) a;
    scope(exit) assert((() nothrow @nogc => a.deallocateAll())());
    a.allocate(64);
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    shared SharedFreeList!(Mallocator, 30, 40, chooseAtRuntime) a;
    scope(exit) assert((() nothrow @nogc => a.deallocateAll())());
    a.allocate(64);
}

@system unittest
{
    // Pull request #5556
    import std.experimental.allocator.mallocator : Mallocator;
    shared SharedFreeList!(Mallocator, 0, chooseAtRuntime) a;
    scope(exit) assert((() nothrow @nogc => a.deallocateAll())());
    a.max = 64;
    a.allocate(64);
}

@system unittest
{
    // Pull request #5556
    import std.experimental.allocator.mallocator : Mallocator;
    shared SharedFreeList!(Mallocator, chooseAtRuntime, 64) a;
    scope(exit) assert((() nothrow @nogc => a.deallocateAll())());
    a.min = 32;
    a.allocate(64);
}

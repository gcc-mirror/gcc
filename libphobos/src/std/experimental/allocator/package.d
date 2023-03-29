// Written in the D programming language.
/**

High-level interface for allocators. Implements bundled allocation/creation
and destruction/deallocation of data including `struct`s and `class`es,
and also array primitives related to allocation. This module is the entry point
for both making use of allocators and for their documentation.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Make) $(TD
    $(LREF make)
    $(LREF makeArray)
    $(LREF makeMultidimensionalArray)
))
$(TR $(TD Dispose) $(TD
    $(LREF dispose)
    $(LREF disposeMultidimensionalArray)
))
$(TR $(TD Modify) $(TD
    $(LREF expandArray)
    $(LREF shrinkArray)
))
$(TR $(TD Global) $(TD
    $(LREF processAllocator)
    $(LREF theAllocator)
))
$(TR $(TD Class interface) $(TD
    $(LREF CAllocatorImpl)
    $(LREF CSharedAllocatorImpl)
    $(LREF IAllocator)
    $(LREF ISharedAllocator)
))
$(TR $(TD Structs) $(TD
    $(LREF allocatorObject)
    $(LREF RCIAllocator)
    $(LREF RCISharedAllocator)
    $(LREF sharedAllocatorObject)
    $(LREF ThreadLocal)
))
)

Synopsis:
$(RUNNABLE_EXAMPLE
---
// Allocate an int, initialize it with 42
int* p = theAllocator.make!int(42);
assert(*p == 42);
// Destroy and deallocate it
theAllocator.dispose(p);

// Allocate using the global process allocator
p = processAllocator.make!int(100);
assert(*p == 100);
// Destroy and deallocate
processAllocator.dispose(p);
---
)
$(RUNNABLE_EXAMPLE
---
// Create an array of 50 doubles initialized to -1.0
double[] arr = theAllocator.makeArray!double(50, -1.0);
// Append two zeros to it
theAllocator.expandArray(arr, 2, 0.0);
// On second thought, take that back
theAllocator.shrinkArray(arr, 2);
// Destroy and deallocate
theAllocator.dispose(arr);
---
)

$(H2 Layered Structure)

D's allocators have a layered structure in both implementation and documentation:

$(OL
$(LI A high-level, dynamically-typed layer (described further down in this
module). It consists of an interface called $(LREF IAllocator), which concrete
allocators need to implement. The interface primitives themselves are oblivious
to the type of the objects being allocated; they only deal in `void[]`, by
necessity of the interface being dynamic (as opposed to type-parameterized).
Each thread has a current allocator it uses by default, which is a thread-local
variable $(LREF theAllocator) of type $(LREF IAllocator). The process has a
global allocator called $(LREF processAllocator), also of type $(LREF
IAllocator). When a new thread is created, $(LREF processAllocator) is copied
into $(LREF theAllocator). An application can change the objects to which these
references point. By default, at application startup, $(LREF processAllocator)
refers to an object that uses D's garbage collected heap. This layer also
include high-level functions such as $(LREF make) and $(LREF dispose) that
comfortably allocate/create and respectively destroy/deallocate objects. This
layer is all needed for most casual uses of allocation primitives.)

$(LI A mid-level, statically-typed layer for assembling several allocators into
one. It uses properties of the type of the objects being created to route
allocation requests to possibly specialized allocators. This layer is relatively
thin and implemented and documented in the $(MREF
std,experimental,allocator,typed) module. It allows an interested user to e.g.
use different allocators for arrays versus fixed-sized objects, to the end of
better overall performance.)

$(LI A low-level collection of highly generic $(I heap building blocks)$(MDASH)
Lego-like pieces that can be used to assemble application-specific allocators.
The real allocation smarts are occurring at this level. This layer is of
interest to advanced applications that want to configure their own allocators.
A good illustration of typical uses of these building blocks is module $(MREF
std,experimental,allocator,showcase) which defines a collection of frequently-
used preassembled allocator objects. The implementation and documentation entry
point is $(MREF std,experimental,allocator,building_blocks). By design, the
primitives of the static interface have the same signatures as the $(LREF
IAllocator) primitives but are for the most part optional and driven by static
introspection. The parameterized class $(LREF CAllocatorImpl) offers an
immediate and useful means to package a static low-level allocator into an
implementation of $(LREF IAllocator).)

$(LI Core allocator objects that interface with D's garbage collected heap
($(MREF std,experimental,allocator,gc_allocator)), the C `malloc` family
($(MREF std,experimental,allocator,mallocator)), and the OS ($(MREF
std,experimental,allocator,mmap_allocator)). Most custom allocators would
ultimately obtain memory from one of these core allocators.)
)

$(H2 Idiomatic Use of `std.experimental.allocator`)

As of this time, `std.experimental.allocator` is not integrated with D's
built-in operators that allocate memory, such as `new`, array literals, or
array concatenation operators. That means `std.experimental.allocator` is
opt-in$(MDASH)applications need to make explicit use of it.

For casual creation and disposal of dynamically-allocated objects, use $(LREF
make), $(LREF dispose), and the array-specific functions $(LREF makeArray),
$(LREF expandArray), and $(LREF shrinkArray). These use by default D's garbage
collected heap, but open the application to better configuration options. These
primitives work either with `theAllocator` but also with any allocator obtained
by combining heap building blocks. For example:

----
void fun(size_t n)
{
    // Use the current allocator
    int[] a1 = theAllocator.makeArray!int(n);
    scope(exit) theAllocator.dispose(a1);
    ...
}
----

To experiment with alternative allocators, set $(LREF theAllocator) for the
current thread. For example, consider an application that allocates many 8-byte
objects. These are not well supported by the default allocator, so a
$(MREF_ALTTEXT free list allocator,
std,experimental,allocator,building_blocks,free_list) would be recommended.
To install one in `main`, the application would use:

----
void main()
{
    import std.experimental.allocator.building_blocks.free_list
        : FreeList;
    theAllocator = allocatorObject(FreeList!8());
    ...
}
----

$(H3 Saving the `IAllocator` Reference For Later Use)

As with any global resource, setting `theAllocator` and `processAllocator`
should not be done often and casually. In particular, allocating memory with
one allocator and deallocating with another causes undefined behavior.
Typically, these variables are set during application initialization phase and
last through the application.

To avoid this, long-lived objects that need to perform allocations,
reallocations, and deallocations relatively often may want to store a reference
to the allocator object they use throughout their lifetime. Then, instead of
using `theAllocator` for internal allocation-related tasks, they'd use the
internally held reference. For example, consider a user-defined hash table:

----
struct HashTable
{
    private IAllocator allocator;
    this(size_t buckets, IAllocator allocator = theAllocator) {
        this.allocator = allocator;
        ...
    }
    // Getter and setter
    IAllocator allocator() { return allocator; }
    void allocator(IAllocator a) { assert(empty); allocator = a; }
}
----

Following initialization, the `HashTable` object would consistently use its
`allocator` object for acquiring memory. Furthermore, setting
`HashTable.allocator` to point to a different allocator should be legal but
only if the object is empty; otherwise, the object wouldn't be able to
deallocate its existing state.

$(H3 Using Allocators without `IAllocator`)

Allocators assembled from the heap building blocks don't need to go through
`IAllocator` to be usable. They have the same primitives as `IAllocator` and
they work with $(LREF make), $(LREF makeArray), $(LREF dispose) etc. So it
suffice to create allocator objects wherever fit and use them appropriately:

----
void fun(size_t n)
{
    // Use a stack-installed allocator for up to 64KB
    StackFront!65536 myAllocator;
    int[] a2 = myAllocator.makeArray!int(n);
    scope(exit) myAllocator.dispose(a2);
    ...
}
----

In this case, `myAllocator` does not obey the `IAllocator` interface, but
implements its primitives so it can work with `makeArray` by means of duck
typing.

One important thing to note about this setup is that statically-typed assembled
allocators are almost always faster than allocators that go through
`IAllocator`. An important rule of thumb is: "assemble allocator first, adapt
to `IAllocator` after". A good allocator implements intricate logic by means of
template assembly, and gets wrapped with `IAllocator` (usually by means of
$(LREF allocatorObject)) only once, at client level.

Copyright: Andrei Alexandrescu 2013-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/experimental/allocator)

*/

module std.experimental.allocator;

public import std.experimental.allocator.common,
    std.experimental.allocator.typed;

// Fix https://issues.dlang.org/show_bug.cgi?id=17806
// this should always be the first unittest in this module in order to ensure
// that we use the `processAllocator` setter before the getter
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    auto newAlloc = sharedAllocatorObject(Mallocator.instance);
    processAllocator = newAlloc;
    assert(processAllocator is newAlloc);
    processAllocator = sharedAllocatorObject(GCAllocator.instance);
}

// Example in the synopsis above
@system unittest
{
    import std.algorithm.comparison : min, max;
    import std.experimental.allocator.building_blocks.allocator_list
        : AllocatorList;
    import std.experimental.allocator.building_blocks.bitmapped_block
        : BitmappedBlock;
    import std.experimental.allocator.building_blocks.bucketizer : Bucketizer;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.building_blocks.segregator : Segregator;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    alias FList = FreeList!(GCAllocator, 0, unbounded);
    alias A = Segregator!(
        8, FreeList!(GCAllocator, 0, 8),
        128, Bucketizer!(FList, 1, 128, 16),
        256, Bucketizer!(FList, 129, 256, 32),
        512, Bucketizer!(FList, 257, 512, 64),
        1024, Bucketizer!(FList, 513, 1024, 128),
        2048, Bucketizer!(FList, 1025, 2048, 256),
        3584, Bucketizer!(FList, 2049, 3584, 512),
        4072 * 1024, AllocatorList!(
            (n) => BitmappedBlock!(4096)(
                    cast(ubyte[])(GCAllocator.instance.allocate(
                        max(n, 4072 * 1024))))),
        GCAllocator
    );
    A tuMalloc;
    auto b = tuMalloc.allocate(500);
    assert(b.length == 500);
    auto c = tuMalloc.allocate(113);
    assert(c.length == 113);
    assert(tuMalloc.expand(c, 14));
    tuMalloc.deallocate(b);
    tuMalloc.deallocate(c);
}

import std.range.primitives;
import std.traits;
import std.typecons;

/**
Dynamic allocator interface. Code that defines allocators ultimately implements
this interface. This should be used wherever a uniform type is required for
encapsulating various allocator implementations.

Composition of allocators is not recommended at this level due to
inflexibility of dynamic interfaces and inefficiencies caused by cascaded
multiple calls. Instead, compose allocators using the static interface defined
in $(MREF std,experimental,allocator,building_blocks),
then adapt the composed
allocator to `IAllocator` (possibly by using $(LREF CAllocatorImpl) below).

Methods returning `Ternary` return `Ternary.yes` upon success,
`Ternary.no` upon failure, and `Ternary.unknown` if the primitive is not
implemented by the allocator instance.
*/
interface IAllocator
{
nothrow:
    /**
    Returns the alignment offered.
    */
    @property uint alignment();

    /**
    Returns the good allocation size that guarantees zero internal
    fragmentation.
    */
    size_t goodAllocSize(size_t s);

    /**
    Allocates `n` bytes of memory.
    */
    void[] allocate(size_t, TypeInfo ti = null);

    /**
    Allocates `n` bytes of memory with specified alignment `a`. Implementations
    that do not support this primitive should always return `null`.
    */
    void[] alignedAllocate(size_t n, uint a);

    /**
    Allocates and returns all memory available to this allocator.
    Implementations that do not support this primitive should always return
    `null`.
    */
    void[] allocateAll();

    /**
    Expands a memory block in place and returns `true` if successful.
    Implementations that don't support this primitive should always return
    `false`.
    */
    bool expand(ref void[], size_t);

    /// Reallocates a memory block.
    bool reallocate(ref void[], size_t);

    /// Reallocates a memory block with specified alignment.
    bool alignedReallocate(ref void[] b, size_t size, uint alignment);

    /**
    Returns `Ternary.yes` if the allocator owns `b`, `Ternary.no` if
    the allocator doesn't own `b`, and `Ternary.unknown` if ownership
    cannot be determined. Implementations that don't support this primitive
    should always return `Ternary.unknown`.
    */
    Ternary owns(void[] b);

    /**
    Resolves an internal pointer to the full block allocated. Implementations
    that don't support this primitive should always return `Ternary.unknown`.
    */
    Ternary resolveInternalPointer(const void* p, ref void[] result);

    /**
    Deallocates a memory block. Implementations that don't support this
    primitive should always return `false`. A simple way to check that an
    allocator supports deallocation is to call `deallocate(null)`.
    */
    bool deallocate(void[] b);

    /**
    Deallocates all memory. Implementations that don't support this primitive
    should always return `false`.
    */
    bool deallocateAll();

    /**
    Returns `Ternary.yes` if no memory is currently allocated from this
    allocator, `Ternary.no` if some allocations are currently active, or
    `Ternary.unknown` if not supported.
    */
    Ternary empty();

    /**
    Increases the reference count of the concrete class that implements this
    interface.

    For stateless allocators, this does nothing.
    */
    @safe @nogc pure
    void incRef();

    /**
    Decreases the reference count of the concrete class that implements this
    interface.
    When the reference count is `0`, the object self-destructs.

    Returns: `true` if the reference count is greater than `0` and `false` when
    it hits `0`. For stateless allocators, it always returns `true`.
    */
    @safe @nogc pure
    bool decRef();
}

/**
A reference counted struct that wraps the dynamic allocator interface.
This should be used wherever a uniform type is required for encapsulating
various allocator implementations.

Code that defines allocators ultimately implements the $(LREF IAllocator)
interface, possibly by using $(LREF CAllocatorImpl) below, and then build a
`RCIAllocator` out of this.

Composition of allocators is not recommended at this level due to
inflexibility of dynamic interfaces and inefficiencies caused by cascaded
multiple calls. Instead, compose allocators using the static interface defined
in $(A std_experimental_allocator_building_blocks.html,
`std.experimental.allocator.building_blocks`), then adapt the composed
allocator to `RCIAllocator` (possibly by using $(LREF allocatorObject) below).
*/
struct RCIAllocator
{
    private IAllocator _alloc;

nothrow:
    private @nogc pure @safe
    this(this _)(IAllocator alloc)
    {
        assert(alloc);
        _alloc = alloc;
    }

    @nogc pure @safe
    this(this)
    {
        if (_alloc !is null)
        {
            _alloc.incRef();
        }
    }

    @nogc pure @safe
    ~this()
    {
        if (_alloc !is null)
        {
            bool isLast = !_alloc.decRef();
            if (isLast) _alloc = null;
        }
    }

    @nogc pure @safe
    auto ref opAssign()(typeof(this) rhs)
    {
        if (_alloc is rhs._alloc)
        {
            return this;
        }
        // incRef was allready called by rhs posblit, so we're just moving
        // calling dtor is the equivalent of decRef
        __dtor();
        _alloc = rhs._alloc;
        // move
        rhs._alloc = null;
        return this;
    }

    @nogc pure @safe
    bool isNull(this _)()
    {
        return _alloc is null;
    }

    @property uint alignment()
    {
        assert(_alloc);
        return _alloc.alignment();
    }

    size_t goodAllocSize(size_t s)
    {
        assert(_alloc);
        return _alloc.goodAllocSize(s);
    }

    void[] allocate(size_t n, TypeInfo ti = null)
    {
        assert(_alloc);
        return _alloc.allocate(n, ti);
    }

    void[] alignedAllocate(size_t n, uint a)
    {
        assert(_alloc);
        return _alloc.alignedAllocate(n, a);
    }

    void[] allocateAll()
    {
        assert(_alloc);
        return _alloc.allocateAll();
    }

    bool expand(ref void[] b, size_t size)
    {
        assert(_alloc);
        return _alloc.expand(b, size);
    }

    bool reallocate(ref void[] b, size_t size)
    {
        assert(_alloc);
        return _alloc.reallocate(b, size);
    }

    bool alignedReallocate(ref void[] b, size_t size, uint alignment)
    {
        assert(_alloc);
        return _alloc.alignedReallocate(b, size, alignment);
    }

    Ternary owns(void[] b)
    {
        assert(_alloc);
        return _alloc.owns(b);
    }

    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        assert(_alloc);
        return _alloc.resolveInternalPointer(p, result);
    }

    bool deallocate(void[] b)
    {
        assert(_alloc);
        return _alloc.deallocate(b);
    }

    bool deallocateAll()
    {
        assert(_alloc);
        return _alloc.deallocateAll();
    }

    Ternary empty()
    {
        assert(_alloc);
        return _alloc.empty();
    }
}

@system unittest
{
    import std.experimental.allocator.building_blocks.region : BorrowedRegion;
    import std.conv : emplace;

    auto reg = BorrowedRegion!()(new ubyte[1024]);
    auto state = reg.allocate(stateSize!(CAllocatorImpl!(BorrowedRegion!(), Yes.indirect)));
    auto regObj = emplace!(CAllocatorImpl!(BorrowedRegion!(), Yes.indirect))(state, &reg);

    auto rcalloc = RCIAllocator(regObj);
    auto b = rcalloc.allocate(10);
    assert(b.length == 10);

    // The reference counting is zero based
    assert((cast(CAllocatorImpl!(BorrowedRegion!(), Yes.indirect))(rcalloc._alloc)).rc == 1);
    {
        auto rca2 = rcalloc;
        assert((cast(CAllocatorImpl!(BorrowedRegion!(), Yes.indirect))(rcalloc._alloc)).rc == 2);
    }
    assert((cast(CAllocatorImpl!(BorrowedRegion!(), Yes.indirect))(rcalloc._alloc)).rc == 1);
}

@system unittest
{
    import std.conv;
    import std.experimental.allocator.mallocator;
    import std.experimental.allocator.building_blocks.stats_collector;

    alias SCAlloc = StatsCollector!(Mallocator, Options.bytesUsed);
    SCAlloc statsCollectorAlloc;

    ulong bytesUsed = statsCollectorAlloc.bytesUsed;
    assert(bytesUsed == 0);

    {
        auto _allocator = allocatorObject(&statsCollectorAlloc);
        bytesUsed = statsCollectorAlloc.bytesUsed;
        assert(bytesUsed == stateSize!(CAllocatorImpl!(SCAlloc, Yes.indirect)));
    }

    bytesUsed = statsCollectorAlloc.bytesUsed;
    assert(bytesUsed == 0, "RCIAllocator leaks memory; leaked "
            ~ to!string(bytesUsed) ~ " bytes");
}

@system unittest
{
    import std.conv;
    import std.experimental.allocator.mallocator;
    import std.experimental.allocator.building_blocks.stats_collector;

    alias SCAlloc = StatsCollector!(Mallocator, Options.bytesUsed);
    SCAlloc statsCollectorAlloc;

    ulong bytesUsed = statsCollectorAlloc.bytesUsed;
    assert(bytesUsed == 0);

    {
        auto _allocator = allocatorObject(statsCollectorAlloc);

        // Ensure that the allocator was passed through in CAllocatorImpl
        // This allocator was used to allocate the chunk that holds the
        // CAllocatorImpl object; which is it's own wrapper
        bytesUsed = (cast(CAllocatorImpl!(SCAlloc))(_allocator._alloc)).impl.bytesUsed;
        assert(bytesUsed == stateSize!(CAllocatorImpl!(SCAlloc)),
               "RCIAllocator leaks memory; leaked " ~ to!string(bytesUsed) ~ " bytes");
        _allocator.allocate(1);
        bytesUsed = (cast(CAllocatorImpl!(SCAlloc))(_allocator._alloc)).impl.bytesUsed;
        assert(bytesUsed == stateSize!(CAllocatorImpl!(SCAlloc)) + 1,
               "RCIAllocator leaks memory; leaked " ~ to!string(bytesUsed) ~ " bytes");
    }

    bytesUsed = statsCollectorAlloc.bytesUsed;
    assert(bytesUsed == stateSize!(CAllocatorImpl!(SCAlloc)),
            "RCIAllocator leaks memory; leaked "
            ~ to!string(bytesUsed) ~ " bytes");
}

/**
Dynamic shared allocator interface. Code that defines allocators shareable
across threads ultimately implements this interface. This should be used
wherever a uniform type is required for encapsulating various allocator
implementations.

Composition of allocators is not recommended at this level due to
inflexibility of dynamic interfaces and inefficiencies caused by cascaded
multiple calls. Instead, compose allocators using the static interface defined
in $(MREF std,experimental,allocator,building_blocks),
then adapt the composed
allocator to `ISharedAllocator` (possibly by using $(LREF CSharedAllocatorImpl) below).

Methods returning `Ternary` return `Ternary.yes` upon success,
`Ternary.no` upon failure, and `Ternary.unknown` if the primitive is not
implemented by the allocator instance.
*/
interface ISharedAllocator
{
nothrow:
    /**
    Returns the alignment offered.
    */
    @property uint alignment() shared;

    /**
    Returns the good allocation size that guarantees zero internal
    fragmentation.
    */
    size_t goodAllocSize(size_t s) shared;

    /**
    Allocates `n` bytes of memory.
    */
    void[] allocate(size_t, TypeInfo ti = null) shared;

    /**
    Allocates `n` bytes of memory with specified alignment `a`. Implementations
    that do not support this primitive should always return `null`.
    */
    void[] alignedAllocate(size_t n, uint a) shared;

    /**
    Allocates and returns all memory available to this allocator.
    Implementations that do not support this primitive should always return
    `null`.
    */
    void[] allocateAll() shared;

    /**
    Expands a memory block in place and returns `true` if successful.
    Implementations that don't support this primitive should always return
    `false`.
    */
    bool expand(ref void[], size_t) shared;

    /// Reallocates a memory block.
    bool reallocate(ref void[], size_t) shared;

    /// Reallocates a memory block with specified alignment.
    bool alignedReallocate(ref void[] b, size_t size, uint alignment) shared;

    /**
    Returns `Ternary.yes` if the allocator owns `b`, `Ternary.no` if
    the allocator doesn't own `b`, and `Ternary.unknown` if ownership
    cannot be determined. Implementations that don't support this primitive
    should always return `Ternary.unknown`.
    */
    Ternary owns(void[] b) shared;

    /**
    Resolves an internal pointer to the full block allocated. Implementations
    that don't support this primitive should always return `Ternary.unknown`.
    */
    Ternary resolveInternalPointer(const void* p, ref void[] result) shared;

    /**
    Deallocates a memory block. Implementations that don't support this
    primitive should always return `false`. A simple way to check that an
    allocator supports deallocation is to call `deallocate(null)`.
    */
    bool deallocate(void[] b) shared;

    /**
    Deallocates all memory. Implementations that don't support this primitive
    should always return `false`.
    */
    bool deallocateAll() shared;

    /**
    Returns `Ternary.yes` if no memory is currently allocated from this
    allocator, `Ternary.no` if some allocations are currently active, or
    `Ternary.unknown` if not supported.
    */
    Ternary empty() shared;

    /**
    Increases the reference count of the concrete class that implements this
    interface.

    For stateless allocators, this does nothing.
    */
    @safe @nogc pure
    void incRef() shared;

    /**
    Decreases the reference count of the concrete class that implements this
    interface.
    When the reference count is `0`, the object self-destructs.

    For stateless allocators, this does nothing.

    Returns: `true` if the reference count is greater than `0` and `false` when
    it hits `0`. For stateless allocators, it always returns `true`.
    */
    @safe @nogc pure
    bool decRef() shared;
}

/**
A reference counted struct that wraps the dynamic shared allocator interface.
This should be used wherever a uniform type is required for encapsulating
various allocator implementations.

Code that defines allocators shareable across threads ultimately implements the
$(LREF ISharedAllocator) interface, possibly by using
$(LREF CSharedAllocatorImpl) below, and then build a `RCISharedAllocator` out
of this.

Composition of allocators is not recommended at this level due to
inflexibility of dynamic interfaces and inefficiencies caused by cascaded
multiple calls. Instead, compose allocators using the static interface defined
in $(A std_experimental_allocator_building_blocks.html,
`std.experimental.allocator.building_blocks`), then adapt the composed allocator
to `RCISharedAllocator` (possibly by using $(LREF sharedAllocatorObject) below).
*/
shared struct RCISharedAllocator
{
    private ISharedAllocator _alloc;

nothrow:
    private @nogc pure @safe
    this(shared ISharedAllocator alloc)
    {
        assert(alloc);
        _alloc = alloc;
    }

    @nogc pure @safe
    this(this)
    {
        if (_alloc !is null)
        {
            _alloc.incRef();
        }
    }

    @nogc pure @safe
    ~this()
    {
        if (_alloc !is null)
        {
            bool isLast = !_alloc.decRef();
            if (isLast) _alloc = null;
        }
    }

    @nogc pure @safe
    auto ref opAssign()(RCISharedAllocator rhs)
    {
        if (_alloc is rhs._alloc)
        {
            return this;
        }
        // incRef was allready called by rhs posblit, so we're just moving
        if (_alloc !is null)
        {
            _alloc.decRef();
        }
        _alloc = rhs._alloc;
        // move
        rhs._alloc = null;
        return this;
    }

    @nogc pure @safe
    bool isNull(this _)()
    {
        return _alloc is null;
    }

    @property uint alignment()
    {
        assert(_alloc);
        return _alloc.alignment();
    }

    size_t goodAllocSize(size_t s)
    {
        assert(_alloc);
        return _alloc.goodAllocSize(s);
    }

    void[] allocate(size_t n, TypeInfo ti = null)
    {
        assert(_alloc);
        return _alloc.allocate(n, ti);
    }

    void[] alignedAllocate(size_t n, uint a)
    {
        assert(_alloc);
        return _alloc.alignedAllocate(n, a);
    }

    void[] allocateAll()
    {
        assert(_alloc);
        return _alloc.allocateAll();
    }

    bool expand(ref void[] b, size_t size)
    {
        assert(_alloc);
        return _alloc.expand(b, size);
    }

    bool reallocate(ref void[] b, size_t size)
    {
        assert(_alloc);
        return _alloc.reallocate(b, size);
    }

    bool alignedReallocate(ref void[] b, size_t size, uint alignment)
    {
        assert(_alloc);
        return _alloc.alignedReallocate(b, size, alignment);
    }

    Ternary owns(void[] b)
    {
        assert(_alloc);
        return _alloc.owns(b);
    }

    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        assert(_alloc);
        return _alloc.resolveInternalPointer(p, result);
    }

    bool deallocate(void[] b)
    {
        assert(_alloc);
        return _alloc.deallocate(b);
    }

    bool deallocateAll()
    {
        assert(_alloc);
        return _alloc.deallocateAll();
    }

    Ternary empty()
    {
        assert(_alloc);
        return _alloc.empty();
    }
}

private RCISharedAllocator _processAllocator;
private RCIAllocator _threadAllocator;

@nogc nothrow @safe
private ref RCIAllocator setupThreadAllocator()
{
    /*
    Forwards the `_threadAllocator` calls to the `processAllocator`
    */
    static class ThreadAllocator : IAllocator
    {
    nothrow:
        private RCISharedAllocator _allocator;

        @nogc @safe
        this(ref RCISharedAllocator procAlloc)
        {
            _allocator = procAlloc;
        }

        override @property uint alignment()
        {
            return _allocator.alignment();
        }

        override size_t goodAllocSize(size_t s)
        {
            return _allocator.goodAllocSize(s);
        }

        override void[] allocate(size_t n, TypeInfo ti = null)
        {
            return _allocator.allocate(n, ti);
        }

        override void[] alignedAllocate(size_t n, uint a)
        {
            return _allocator.alignedAllocate(n, a);
        }

        override void[] allocateAll()
        {
            return _allocator.allocateAll();
        }

        override bool expand(ref void[] b, size_t size)
        {
            return _allocator.expand(b, size);
        }

        override bool reallocate(ref void[] b, size_t size)
        {
            return _allocator.reallocate(b, size);
        }

        override bool alignedReallocate(ref void[] b, size_t size, uint alignment)
        {
            return _allocator.alignedReallocate(b, size, alignment);
        }

        override Ternary owns(void[] b)
        {
            return _allocator.owns(b);
        }

        override Ternary resolveInternalPointer(const void* p, ref void[] result)
        {
            return _allocator.resolveInternalPointer(p, result);
        }

        override bool deallocate(void[] b)
        {
            return _allocator.deallocate(b);
        }

        override bool deallocateAll()
        {
            return _allocator.deallocateAll();
        }

        override Ternary empty()
        {
            return _allocator.empty();
        }

        @nogc pure @safe
        override void incRef()
        {
            _allocator._alloc.incRef();
        }

        @nogc pure @safe
        override bool decRef()
        {
            return _allocator._alloc.decRef();
        }
    }

    assert(_threadAllocator.isNull);
    import core.lifetime : emplace;
    static ulong[stateSize!(ThreadAllocator).divideRoundUp(ulong.sizeof)] _threadAllocatorState;
    () @trusted {
        _threadAllocator = RCIAllocator(emplace!(ThreadAllocator)(_threadAllocatorState[], processAllocator()));
    }();
    return _threadAllocator;
}

// Fix threadAllocator bug: the threadAllocator should hold an internal reference
// to the processAllocator that it's using
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    auto a = sharedAllocatorObject(Mallocator.instance);
    auto buf = theAllocator.allocate(42);
    processAllocator = a;
    theAllocator.deallocate(buf);
}

/**
Gets/sets the allocator for the current thread. This is the default allocator
that should be used for allocating thread-local memory. For allocating memory
to be shared across threads, use `processAllocator` (below). By default,
`theAllocator` ultimately fetches memory from `processAllocator`, which
in turn uses the garbage collected heap.
*/
@nogc nothrow @safe
@property ref RCIAllocator theAllocator()
{
    alias p = _threadAllocator;
    return !p.isNull() ? p : setupThreadAllocator();
}

/// Ditto
nothrow @system @nogc
@property void theAllocator(RCIAllocator a)
{
    assert(!a.isNull);
    _threadAllocator = a;
}

///
@system unittest
{
    // Install a new allocator that is faster for 128-byte allocations.
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    auto oldAllocator = theAllocator;
    scope(exit) theAllocator = oldAllocator;
    theAllocator = allocatorObject(FreeList!(GCAllocator, 128)());
    // Use the now changed allocator to allocate an array
    const ubyte[] arr = theAllocator.makeArray!ubyte(128);
    assert(arr.ptr);
    //...
}

/**
Gets/sets the allocator for the current process. This allocator must be used
for allocating memory shared across threads. Objects created using this
allocator can be cast to `shared`.
*/
@nogc nothrow @trusted
@property ref RCISharedAllocator processAllocator()
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.concurrency : initOnce;

    static RCISharedAllocator* forceAttributes()
    {
        return &initOnce!_processAllocator(
                sharedAllocatorObject(GCAllocator.instance));
    }

    return *(cast(RCISharedAllocator* function() @nogc nothrow)(&forceAttributes))();
}

/// Ditto
@nogc nothrow @system
@property void processAllocator(ref RCISharedAllocator a)
{
    assert(!a.isNull);
    processAllocator() = a;
}

@system unittest
{
    import core.exception : AssertError;
    import std.exception : assertThrown;
    import std.experimental.allocator.building_blocks.free_list : SharedFreeList;
    import std.experimental.allocator.mallocator : Mallocator;

    assert(!processAllocator.isNull);
    assert(!theAllocator.isNull);

    testAllocatorObject(processAllocator);
    testAllocatorObject(theAllocator);

    shared SharedFreeList!(Mallocator, chooseAtRuntime, chooseAtRuntime) sharedFL;
    RCISharedAllocator sharedFLObj = sharedAllocatorObject(sharedFL);
    alias SharedAllocT = CSharedAllocatorImpl!(
            shared SharedFreeList!(
                Mallocator, chooseAtRuntime, chooseAtRuntime));

    assert((cast(SharedAllocT)(sharedFLObj._alloc)).rc == 1);
    assert(!sharedFLObj.isNull);
    testAllocatorObject(sharedFLObj);

    // Test processAllocator setter
    RCISharedAllocator oldProcessAllocator = processAllocator;
    processAllocator = sharedFLObj;
    assert((cast(SharedAllocT)(sharedFLObj._alloc)).rc == 2);
    assert(processAllocator._alloc is sharedFLObj._alloc);

    testAllocatorObject(processAllocator);
    testAllocatorObject(theAllocator);
    assertThrown!AssertError(processAllocator = RCISharedAllocator(null));

    // Restore initial processAllocator state
    processAllocator = oldProcessAllocator;
    assert((cast(SharedAllocT)(sharedFLObj._alloc)).rc == 1);
    assert(processAllocator is oldProcessAllocator);

    RCISharedAllocator indirectShFLObj = sharedAllocatorObject(&sharedFL);
    testAllocatorObject(indirectShFLObj);
    alias IndirectSharedAllocT = CSharedAllocatorImpl!(
            shared SharedFreeList!(
                Mallocator, chooseAtRuntime, chooseAtRuntime)
            , Yes.indirect);

    assert((cast(IndirectSharedAllocT)(indirectShFLObj._alloc)).rc == 1);

    RCIAllocator indirectMallocator = allocatorObject(&Mallocator.instance);
    testAllocatorObject(indirectMallocator);
}

/**
Dynamically allocates (using `alloc`) and then creates in the memory
allocated an object of type `T`, using `args` (if any) for its
initialization. Initialization occurs in the memory allocated and is otherwise
semantically the same as `T(args)`.
(Note that using `alloc.make!(T[])` creates a pointer to an (empty) array
of `T`s, not an array. To use an allocator to allocate and initialize an
array, use `alloc.makeArray!T` described below.)

Params:
T = Type of the object being created.
alloc = The allocator used for getting the needed memory. It may be an object
implementing the static interface for allocators, or an `IAllocator`
reference.
args = Optional arguments used for initializing the created object. If not
present, the object is default constructed.

Returns: If `T` is a class type, returns a reference to the created `T`
object. Otherwise, returns a `T*` pointing to the created object. In all
cases, returns `null` if allocation failed.

Throws: If `T`'s constructor throws, deallocates the allocated memory and
propagates the exception.
*/
auto make(T, Allocator, A...)(auto ref Allocator alloc, auto ref A args)
{
    import std.algorithm.comparison : max;
    static if (!is(T == class) && !is(T == interface) && A.length == 0
        && __traits(compiles, {T t;}) && __traits(isZeroInit, T)
        && is(typeof(alloc.allocateZeroed(size_t.max))))
    {
        auto m = alloc.allocateZeroed(max(T.sizeof, 1));
        return (() @trusted => cast(T*) m.ptr)();
    }
    else
    {
        import core.internal.lifetime : emplaceRef;
        import core.lifetime : emplace;

        auto m = alloc.allocate(max(stateSize!T, 1));
        if (!m.ptr) return null;

        // make can only be @safe if emplace or emplaceRef is `pure`
        auto construct()
        {
            static if (is(T == class)) return emplace!T(m, args);
            else
            {
                // Assume cast is safe as allocation succeeded for `stateSize!T`
                auto p = () @trusted { return cast(T*) m.ptr; }();
                emplaceRef!T(*p, args);
                return p;
            }
        }

        scope(failure)
        {
            static if (is(typeof(() pure { return construct(); })))
            {
                // Assume deallocation is safe because:
                // 1) in case of failure, `m` is the only reference to this memory
                // 2) `m` is known to originate from `alloc`
                () @trusted { alloc.deallocate(m); }();
            }
            else
            {
                alloc.deallocate(m);
            }
        }

        return construct();
    }
}

///
@system unittest
{
    // Dynamically allocate one integer
    const int* p1 = theAllocator.make!int;
    // It's implicitly initialized with its .init value
    assert(*p1 == 0);
    // Dynamically allocate one double, initialize to 42.5
    const double* p2 = theAllocator.make!double(42.5);
    assert(*p2 == 42.5);

    // Dynamically allocate a struct
    static struct Point
    {
        int x, y, z;
    }
    // Use the generated constructor taking field values in order
    const Point* p = theAllocator.make!Point(1, 2);
    assert(p.x == 1 && p.y == 2 && p.z == 0);

    // Dynamically allocate a class object
    static class Customer
    {
        uint id = uint.max;
        this() {}
        this(uint id) { this.id = id; }
        // ...
    }
    Customer cust = theAllocator.make!Customer;
    assert(cust.id == uint.max); // default initialized
    cust = theAllocator.make!Customer(42);
    assert(cust.id == 42);

    // explicit passing of outer pointer
    static class Outer
    {
        int x = 3;
        class Inner
        {
            auto getX() { return x; }
        }
    }
    auto outer = theAllocator.make!Outer();
    auto inner = theAllocator.make!(Outer.Inner)(outer);
    assert(outer.x == inner.getX);
}

// https://issues.dlang.org/show_bug.cgi?id=15639
// https://issues.dlang.org/show_bug.cgi?id=15772
@system unittest
{
    abstract class Foo {}
    class Bar: Foo {}
    static assert(!is(typeof(theAllocator.make!Foo)));
    static assert( is(typeof(theAllocator.make!Bar)));
}

@system unittest
{
    void test(Allocator)(auto ref Allocator alloc)
    {
        const int* a = alloc.make!int(10);
        assert(*a == 10);

        struct A
        {
            int x;
            string y;
            double z;
        }

        A* b = alloc.make!A(42);
        assert(b.x == 42);
        assert(b.y is null);
        import std.math.traits : isNaN;
        assert(b.z.isNaN);

        b = alloc.make!A(43, "44", 45);
        assert(b.x == 43);
        assert(b.y == "44");
        assert(b.z == 45);

        static class B
        {
            int x;
            string y;
            double z;
            this(int _x, string _y = null, double _z = double.init)
            {
                x = _x;
                y = _y;
                z = _z;
            }
        }

        B c = alloc.make!B(42);
        assert(c.x == 42);
        assert(c.y is null);
        assert(c.z.isNaN);

        c = alloc.make!B(43, "44", 45);
        assert(c.x == 43);
        assert(c.y == "44");
        assert(c.z == 45);

        const parray = alloc.make!(int[]);
        assert((*parray).empty);
    }

    import std.experimental.allocator.gc_allocator : GCAllocator;
    test(GCAllocator.instance);
    test(theAllocator);
}

// Attribute propagation
nothrow @safe @nogc unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    alias alloc = Mallocator.instance;

    void test(T, Args...)(auto ref Args args)
    {
        auto k = alloc.make!T(args);
        () @trusted { alloc.dispose(k); }();
    }

    test!int;
    test!(int*);
    test!int(0);
    test!(int*)(null);
}

// should be pure with the GCAllocator
/*pure nothrow*/ @safe unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;

    alias alloc = GCAllocator.instance;

    void test(T, Args...)(auto ref Args args)
    {
        auto k = alloc.make!T(args);
        (a) @trusted { a.dispose(k); }(alloc);
    }

    test!int();
    test!(int*);
    test!int(0);
    test!(int*)(null);
}

// Verify that making an object by calling an impure constructor is not @safe
nothrow @safe @nogc unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    static struct Pure { this(int) pure nothrow @nogc @safe {} }

    cast(void) Mallocator.instance.make!Pure(0);

    static int g = 0;
    static struct Impure { this(int) nothrow @nogc @safe {
        g++;
    } }
    static assert(!__traits(compiles, cast(void) Mallocator.instance.make!Impure(0)));
}

// test failure with a pure, failing struct
@safe unittest
{
    import std.exception : assertThrown, enforce;

    // this struct can't be initialized
    struct InvalidStruct
    {
        this(int b)
        {
            enforce(1 == 2);
        }
    }
    import std.experimental.allocator.mallocator : Mallocator;
    assertThrown(make!InvalidStruct(Mallocator.instance, 42));
}

// test failure with an impure, failing struct
@system unittest
{
    import std.exception : assertThrown, enforce;
    static int g;
    struct InvalidImpureStruct
    {
        this(int b)
        {
            g++;
            enforce(1 == 2);
        }
    }
    import std.experimental.allocator.mallocator : Mallocator;
    assertThrown(make!InvalidImpureStruct(Mallocator.instance, 42));
}

// Don't allow zero-ctor-args `make` for structs with `@disable this();`
@system unittest
{
    struct NoDefaultCtor
    {
        int i;
        @disable this();
    }
    import std.experimental.allocator.mallocator : Mallocator;
    static assert(!__traits(compiles, make!NoDefaultCtor(Mallocator.instance)),
        "Don't allow zero-ctor-args `make` for structs with `@disable this();`");
}

// https://issues.dlang.org/show_bug.cgi?id=18937
@safe unittest
{
    static struct S
    {
        ubyte[16 * 1024] data;
    }

    static struct SomeAllocator
    {
        ubyte[] allocate(size_t) { return []; }
        void deallocate(void[]) {}
    }

    auto x = SomeAllocator().make!S();
}

private void fillWithMemcpy(T)(scope void[] array, auto ref T filler) nothrow
if (T.sizeof == 1)
{
    import core.stdc.string : memset;
    import std.traits : CopyConstness;
    if (!array.length) return;
    memset(array.ptr, *cast(CopyConstness!(T*, ubyte*)) &filler, array.length);
}

private void fillWithMemcpy(T)(scope void[] array, auto ref T filler) nothrow
if (T.sizeof != 1)
{
    import core.stdc.string : memcpy;
    import std.algorithm.comparison : min;
    if (!array.length) return;
    memcpy(array.ptr, &filler, T.sizeof);
    // Fill the array from the initialized portion of itself exponentially.
    for (size_t offset = T.sizeof; offset < array.length; )
    {
        size_t extent = min(offset, array.length - offset);
        memcpy(array.ptr + offset, array.ptr, extent);
        offset += extent;
    }
}

@system unittest
{
    // Test T.sizeof == 1 path of fillWithMemcpy.
    ubyte[] a;
    fillWithMemcpy(a, ubyte(42));
    assert(a.length == 0);
    a = [ 1, 2, 3, 4, 5 ];
    fillWithMemcpy(a, ubyte(42));
    assert(a == [ 42, 42, 42, 42, 42]);
}

@system unittest
{
    int[] a;
    fillWithMemcpy(a, 42);
    assert(a.length == 0);
    a = [ 1, 2, 3, 4, 5 ];
    fillWithMemcpy(a, 42);
    assert(a == [ 42, 42, 42, 42, 42]);
}

//Make shared object
@system unittest
{
    import core.atomic : atomicLoad;
    auto psi = theAllocator.make!(shared(int))(10);
    assert(10 == (*psi).atomicLoad());
}

private T[] uninitializedFillDefault(T)(T[] array) nothrow
{
    static if (__traits(isZeroInit, T))
    {
        import core.stdc.string : memset;
        if (array !is null)
            memset(array.ptr, 0, T.sizeof * array.length);
        return array;
    }
    else static if (is(immutable T == immutable char) || is(immutable T == immutable wchar))
    {
        import core.stdc.string : memset;
        if (array !is null)
            memset(array.ptr, 0xff, T.sizeof * array.length);
        return array;
    }
    else
    {
        T t = T.init;
        fillWithMemcpy(array, t);
        return array;
    }
}

pure nothrow @nogc
@system unittest
{
    static struct S { int x = 42; @disable this(this); }

    int[5] expected = [42, 42, 42, 42, 42];
    S[5] arr = void;
    uninitializedFillDefault(arr);
    assert((cast(int*) arr.ptr)[0 .. arr.length] == expected);
}

@system unittest
{
    int[] a = [1, 2, 4];
    uninitializedFillDefault(a);
    assert(a == [0, 0, 0]);

    char[] b = [1, 2, 4];
    uninitializedFillDefault(b);
    assert(b == [0xff, 0xff, 0xff]);

    wchar[] c = [1, 2, 4];
    uninitializedFillDefault(c);
    assert(c == [0xffff, 0xffff, 0xffff]);
}

@system unittest
{
    static struct P { float x = 0; float y = 0; }

    static assert(__traits(isZeroInit, P));
    P[] a = [P(10, 11), P(20, 21), P(40, 41)];
    uninitializedFillDefault(a);
    assert(a == [P.init, P.init, P.init]);
}

/**
Create an array of `T` with `length` elements using `alloc`. The array is either default-initialized, filled with copies of `init`, or initialized with values fetched from `range`.

Params:
T = element type of the array being created
alloc = the allocator used for getting memory
length = length of the newly created array
init = element used for filling the array
range = range used for initializing the array elements

Returns:
The newly-created array, or `null` if either `length` was `0` or
allocation failed.

Throws:
The first two overloads throw only if `alloc`'s primitives do. The
overloads that involve copy initialization deallocate memory and propagate the
exception if the copy operation throws.
*/
T[] makeArray(T, Allocator)(auto ref Allocator alloc, size_t length)
{
    if (!length) return null;
    static if (T.sizeof <= 1)
    {
        const nAlloc = length * T.sizeof;
    }
    else
    {
        import core.checkedint : mulu;
        bool overflow;
        const nAlloc = mulu(length, T.sizeof, overflow);
        if (overflow) return null;
    }

    static if (__traits(isZeroInit, T) && hasMember!(Allocator, "allocateZeroed"))
    {
        auto m = alloc.allocateZeroed(nAlloc);
        return (() @trusted => cast(T[]) m)();
    }
    else
    {
        auto m = alloc.allocate(nAlloc);
        if (!m.ptr) return null;
        alias U = Unqual!T;
        return () @trusted { return cast(T[]) uninitializedFillDefault(cast(U[]) m); }();
    }
}

@system unittest
{
    void test1(A)(auto ref A alloc)
    {
        int[] a = alloc.makeArray!int(0);
        assert(a.length == 0 && a.ptr is null);
        a = alloc.makeArray!int(5);
        assert(a.length == 5);
        static immutable cheatsheet = [0, 0, 0, 0, 0];
        assert(a == cheatsheet);
    }

    void test2(A)(auto ref A alloc)
    {
        static struct S { int x = 42; @disable this(this); }
        S[] arr = alloc.makeArray!S(5);
        assert(arr.length == 5);
        int[] arrInt = () @trusted { return (cast(int*) arr.ptr)[0 .. 5]; }();
        static immutable res = [42, 42, 42, 42, 42];
        assert(arrInt == res);
    }

    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;
    (alloc) /*pure nothrow*/ @safe { test1(alloc); test2(alloc);} (GCAllocator.instance);
    (alloc) nothrow @safe @nogc { test1(alloc); test2(alloc);} (Mallocator.instance);
    test2(theAllocator);
}

@system unittest
{
    import std.algorithm.comparison : equal;
    auto a = theAllocator.makeArray!(shared int)(5);
    static assert(is(typeof(a) == shared(int)[]));
    assert(a.length == 5);
    assert(a.equal([0, 0, 0, 0, 0]));

    auto b = theAllocator.makeArray!(const int)(5);
    static assert(is(typeof(b) == const(int)[]));
    assert(b.length == 5);
    assert(b.equal([0, 0, 0, 0, 0]));

    auto c = theAllocator.makeArray!(immutable int)(5);
    static assert(is(typeof(c) == immutable(int)[]));
    assert(c.length == 5);
    assert(c.equal([0, 0, 0, 0, 0]));
}

// https://issues.dlang.org/show_bug.cgi?id=19085 - makeArray with void
@system unittest
{
    auto b = theAllocator.makeArray!void(5);
    scope(exit) theAllocator.dispose(b);
    auto c = cast(ubyte[]) b;
    assert(c.length == 5);
    assert(c == [0, 0, 0, 0, 0]); // default initialization
}

private enum hasPurePostblit(T) = !hasElaborateCopyConstructor!T ||
    is(typeof(() pure { T.init.__xpostblit(); }));

private enum hasPureDtor(T) = !hasElaborateDestructor!T ||
    is(typeof(() pure { T.init.__xdtor(); }));

// `true` when postblit and destructor of T cannot escape references to itself
private enum canSafelyDeallocPostRewind(T) = hasPurePostblit!T && hasPureDtor!T;

/// Ditto
T[] makeArray(T, Allocator)(auto ref Allocator alloc, size_t length, T init)
{
    if (!length) return null;
    auto m = alloc.allocate(T.sizeof * length);
    if (!m.ptr) return null;
    auto result = () @trusted { return cast(T[]) m; } ();
    import std.traits : hasElaborateCopyConstructor;
    static if (hasElaborateCopyConstructor!T)
    {
        scope(failure)
        {
            static if (canSafelyDeallocPostRewind!T)
                () @trusted { alloc.deallocate(m); } ();
            else
                alloc.deallocate(m);
        }

        size_t i = 0;
        static if (hasElaborateDestructor!T)
        {
            scope (failure)
            {
                foreach (j; 0 .. i)
                {
                    destroy(result[j]);
                }
            }
        }
        import core.lifetime : emplace;
        for (; i < length; ++i)
        {
            emplace!T(&result[i], init);
        }
    }
    else
    {
        alias U = Unqual!T;
        () @trusted { fillWithMemcpy(cast(U[]) result, *(cast(U*) &init)); }();
    }
    return result;
}

///
@system unittest
{
    import std.algorithm.comparison : equal;
    static void test(T)()
    {
        T[] a = theAllocator.makeArray!T(2);
        assert(a.equal([0, 0]));
        a = theAllocator.makeArray!T(3, 42);
        assert(a.equal([42, 42, 42]));
        import std.range : only;
        a = theAllocator.makeArray!T(only(42, 43, 44));
        assert(a.equal([42, 43, 44]));
    }
    test!int();
    test!(shared int)();
    test!(const int)();
    test!(immutable int)();
}

@system unittest
{
    void test(T)(in T initialValue)
    {
        auto t = theAllocator.makeArray!T(100, initialValue);
        //auto t = theAllocator.makeArray(100, initialValue); // works well with the old code
    }

    const int init = 3;
    test(init);
}

@system unittest
{
    void test(A)(auto ref A alloc)
    {
        long[] a = alloc.makeArray!long(0, 42);
        assert(a.length == 0 && a.ptr is null);
        a = alloc.makeArray!long(5, 42);
        assert(a.length == 5);
        assert(a == [ 42, 42, 42, 42, 42 ]);
    }
    import std.experimental.allocator.gc_allocator : GCAllocator;
    (alloc) /*pure nothrow*/ @safe { test(alloc); } (GCAllocator.instance);
    test(theAllocator);
}

// test failure with a pure, failing struct
@safe unittest
{
    import std.exception : assertThrown, enforce;

    struct NoCopy
    {
        @disable this();

        this(int b){}

        // can't be copied
        this(this)
        {
            enforce(1 == 2);
        }
    }
    import std.experimental.allocator.mallocator : Mallocator;
    assertThrown(makeArray!NoCopy(Mallocator.instance, 10, NoCopy(42)));
}

// test failure with an impure, failing struct
@system unittest
{
    import std.exception : assertThrown, enforce;

    static int i = 0;
    struct Singleton
    {
        @disable this();

        this(int b){}

        // can't be copied
        this(this)
        {
            enforce(i++ == 0);
        }

        ~this()
        {
            i--;
        }
    }
    import std.experimental.allocator.mallocator : Mallocator;
    assertThrown(makeArray!Singleton(Mallocator.instance, 10, Singleton(42)));
}

/// Ditto
Unqual!(ElementEncodingType!R)[] makeArray(Allocator, R)(auto ref Allocator alloc, R range)
if (isInputRange!R && !isInfinite!R)
{
    alias T = Unqual!(ElementEncodingType!R);
    return makeArray!(T, Allocator, R)(alloc, range);
}

/// Ditto
T[] makeArray(T, Allocator, R)(auto ref Allocator alloc, R range)
if (isInputRange!R && !isInfinite!R)
{
    static if (isForwardRange!R || hasLength!R)
    {
        static if (hasLength!R || isNarrowString!R)
            immutable length = range.length;
        else
            immutable length = range.save.walkLength;

        if (!length) return null;
        auto m = alloc.allocate(T.sizeof * length);
        if (!m.ptr) return null;
        auto result = () @trusted { return cast(T[]) m; } ();

        size_t i = 0;
        scope (failure)
        {
            foreach (j; 0 .. i)
            {
                auto p = () @trusted { return cast(Unqual!T*) &result[j]; }();
                destroy(p);
            }

            static if (canSafelyDeallocPostRewind!T)
                () @trusted { alloc.deallocate(m); } ();
            else
                alloc.deallocate(m);
        }

        import core.internal.lifetime : emplaceRef;
        static if (isNarrowString!R || isRandomAccessRange!R)
        {
            foreach (j; 0 .. range.length)
            {
                emplaceRef!T(result[i++], range[j]);
            }
        }
        else
        {
            for (; !range.empty; range.popFront, ++i)
            {
                emplaceRef!T(result[i], range.front);
            }
        }

        return result;
    }
    else
    {
        // Estimated size
        size_t estimated = 8;
        auto m = alloc.allocate(T.sizeof * estimated);
        if (!m.ptr) return null;
        auto result = () @trusted { return cast(T[]) m; } ();

        size_t initialized = 0;
        void bailout()
        {
            foreach (i; 0 .. initialized + 1)
            {
                destroy(result[i]);
            }

            static if (canSafelyDeallocPostRewind!T)
                () @trusted { alloc.deallocate(m); } ();
            else
                alloc.deallocate(m);
        }
        scope (failure) bailout;

        for (; !range.empty; range.popFront, ++initialized)
        {
            if (initialized == estimated)
            {
                // Need to reallocate
                static if (hasPurePostblit!T)
                    auto success = () @trusted { return alloc.reallocate(m, T.sizeof * (estimated *= 2)); } ();
                else
                    auto success = alloc.reallocate(m, T.sizeof * (estimated *= 2));
                if (!success)
                {
                    bailout;
                    return null;
                }
                result = () @trusted { return cast(T[]) m; } ();
            }
            import core.internal.lifetime : emplaceRef;
            emplaceRef(result[initialized], range.front);
        }

        if (initialized < estimated)
        {
            // Try to shrink memory, no harm if not possible
            static if (hasPurePostblit!T)
                auto success = () @trusted { return alloc.reallocate(m, T.sizeof * initialized); } ();
            else
                auto success = alloc.reallocate(m, T.sizeof * initialized);
            if (success)
                result = () @trusted { return cast(T[]) m; } ();
        }

        return result[0 .. initialized];
    }
}

@system unittest
{
    void test(A)(auto ref A alloc)
    {
        long[] a = alloc.makeArray!long((int[]).init);
        assert(a.length == 0 && a.ptr is null);
        a = alloc.makeArray!long([5, 42]);
        assert(a.length == 2);
        assert(a == [ 5, 42]);

        // we can also infer the type
        auto b = alloc.makeArray([4.0, 2.0]);
        static assert(is(typeof(b) == double[]));
        assert(b == [4.0, 2.0]);
    }
    import std.experimental.allocator.gc_allocator : GCAllocator;
    (alloc) pure nothrow @safe { test(alloc); } (GCAllocator.instance);
    test(theAllocator);
}

// infer types for strings
@system unittest
{
    void test(A)(auto ref A alloc)
    {
        auto c = alloc.makeArray("fooπ😜");
        static assert(is(typeof(c) == char[]));
        assert(c == "fooπ😜");

        auto d = alloc.makeArray("fooπ😜"d);
        static assert(is(typeof(d) == dchar[]));
        assert(d == "fooπ😜");

        auto w = alloc.makeArray("fooπ😜"w);
        static assert(is(typeof(w) == wchar[]));
        assert(w == "fooπ😜");
    }

    import std.experimental.allocator.gc_allocator : GCAllocator;
    (alloc) pure nothrow @safe { test(alloc); } (GCAllocator.instance);
    test(theAllocator);
}

/*pure*/ nothrow @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.internal.test.dummyrange;
    import std.range : iota;
    foreach (DummyType; AllDummyRanges)
    {
        (alloc) pure nothrow @safe
        {
            DummyType d;
            auto arr = alloc.makeArray(d);
            assert(arr.length == 10);
            assert(arr.equal(iota(1, 11)));
        } (GCAllocator.instance);
    }
}

// test failure with a pure, failing struct
@safe unittest
{
    import std.exception : assertThrown, enforce;

    struct NoCopy
    {
        int b;

        @disable this();

        this(int b)
        {
            this.b = b;
        }

        // can't be copied
        this(this)
        {
            enforce(b < 3, "there can only be three elements");
        }
    }
    import std.experimental.allocator.mallocator : Mallocator;
    auto arr = [NoCopy(1), NoCopy(2), NoCopy(3)];
    assertThrown(makeArray!NoCopy(Mallocator.instance, arr));

    struct NoCopyRange
    {
        static j = 0;
        bool empty()
        {
            return j > 5;
        }

        auto front()
        {
            return NoCopy(j);
        }

        void popFront()
        {
            j++;
        }
    }
    makeArray!NoCopy(Mallocator.instance, NoCopyRange()); // rvalue elements are forwarded/moved
}

// test failure with an impure, failing struct
@system unittest
{
    import std.exception : assertThrown, enforce;

    static i = 0;
    static maxElements = 2;
    struct NoCopy
    {
        int val;
        @disable this();

        this(int b){
            this.val = i++;
        }

        // can't be copied
        this(this)
        {
            enforce(i++ < maxElements, "there can only be four elements");
        }
    }

    import std.experimental.allocator.mallocator : Mallocator;
    auto arr = [NoCopy(1), NoCopy(2)];
    assertThrown(makeArray!NoCopy(Mallocator.instance, arr));

    i = 0;
    maxElements = 0; // disallow any postblit
    static j = 0;

    struct NoCopyRange
    {
        bool empty()
        {
            return j > 100;
        }

        auto front()
        {
            return NoCopy(1);
        }

        void popFront()
        {
            j++;
        }
    }

    auto arr2 = makeArray!NoCopy(Mallocator.instance, NoCopyRange());
    assert(i == j && i == 101); // all 101 rvalue elements forwarded/moved
}

version (StdUnittest)
{
    private struct ForcedInputRange(T)
    {
        T[]* array;
        pure nothrow @safe @nogc:
        bool empty() { return !array || (*array).empty; }
        ref T front() { return (*array)[0]; }
        void popFront() { *array = (*array)[1 .. $]; }
    }
}

@system unittest
{
    import std.array : array;
    import std.range : iota;
    int[] arr = iota(10).array;

    void test(A)(auto ref A alloc)
    {
        ForcedInputRange!int r;
        long[] a = alloc.makeArray!long(r);
        assert(a.length == 0 && a.ptr is null);
        auto arr2 = arr;
        r.array = () @trusted { return &arr2; } ();
        a = alloc.makeArray!long(r);
        assert(a.length == 10);
        assert(a == iota(10).array);
    }
    import std.experimental.allocator.gc_allocator : GCAllocator;
    (alloc) pure nothrow @safe { test(alloc); } (GCAllocator.instance);
    test(theAllocator);
}

/**
Grows `array` by appending `delta` more elements. The needed memory is
allocated using `alloc`. The extra elements added are either default-
initialized, filled with copies of `init`, or initialized with values
fetched from `range`.

Params:
T = element type of the array being created
alloc = the allocator used for getting memory
array = a reference to the array being grown
delta = number of elements to add (upon success the new length of `array` is
$(D array.length + delta))
init = element used for filling the array
range = range used for initializing the array elements

Returns:
`true` upon success, `false` if memory could not be allocated. In the
latter case `array` is left unaffected.

Throws:
The first two overloads throw only if `alloc`'s primitives do. The
overloads that involve copy initialization deallocate memory and propagate the
exception if the copy operation throws.
*/
bool expandArray(T, Allocator)(auto ref Allocator alloc, ref T[] array,
        size_t delta)
{
    if (!delta) return true;
    if (array is null) return false;
    immutable oldLength = array.length;
    void[] buf = array;
    if (!alloc.reallocate(buf, buf.length + T.sizeof * delta)) return false;
    array = cast(T[]) buf;
    array[oldLength .. $].uninitializedFillDefault;
    return true;
}

@system unittest
{
    void test(A)(auto ref A alloc)
    {
        auto arr = alloc.makeArray!int([1, 2, 3]);
        assert(alloc.expandArray(arr, 3));
        assert(arr == [1, 2, 3, 0, 0, 0]);
    }
    import std.experimental.allocator.gc_allocator : GCAllocator;
    test(GCAllocator.instance);
    test(theAllocator);
}

/// Ditto
bool expandArray(T, Allocator)(auto ref Allocator alloc, ref T[] array,
    size_t delta, auto ref T init)
{
    if (!delta) return true;
    if (array is null) return false;
    void[] buf = array;
    if (!alloc.reallocate(buf, buf.length + T.sizeof * delta)) return false;
    immutable oldLength = array.length;
    array = cast(T[]) buf;
    scope(failure) array[oldLength .. $].uninitializedFillDefault;
    import std.algorithm.mutation : uninitializedFill;
    array[oldLength .. $].uninitializedFill(init);
    return true;
}

@system unittest
{
    void test(A)(auto ref A alloc)
    {
        auto arr = alloc.makeArray!int([1, 2, 3]);
        assert(alloc.expandArray(arr, 3, 1));
        assert(arr == [1, 2, 3, 1, 1, 1]);
    }
    import std.experimental.allocator.gc_allocator : GCAllocator;
    test(GCAllocator.instance);
    test(theAllocator);
}

/// Ditto
bool expandArray(T, Allocator, R)(auto ref Allocator alloc, ref T[] array,
        R range)
if (isInputRange!R)
{
    if (array is null) return false;
    static if (isForwardRange!R)
    {
        immutable delta = walkLength(range.save);
        if (!delta) return true;
        immutable oldLength = array.length;

        // Reallocate support memory
        void[] buf = array;
        if (!alloc.reallocate(buf, buf.length + T.sizeof * delta))
        {
            return false;
        }
        array = cast(T[]) buf;
        // At this point we're committed to the new length.

        auto toFill = array[oldLength .. $];
        scope (failure)
        {
            // Fill the remainder with default-constructed data
            toFill.uninitializedFillDefault;
        }

        for (; !range.empty; range.popFront, toFill = toFill[1 .. $])
        {
            assert(toFill.length > 0);
            import core.lifetime : emplace;
            emplace!T(&toFill[0], range.front);
        }
        assert(toFill.length == 0);
    }
    else
    {
        scope(failure)
        {
            // The last element didn't make it, fill with default
            array[$ - 1 .. $].uninitializedFillDefault;
        }
        void[] buf = array;
        for (; !range.empty; range.popFront)
        {
            if (!alloc.reallocate(buf, buf.length + T.sizeof))
            {
                array = cast(T[]) buf;
                return false;
            }
            import core.lifetime : emplace;
            emplace!T(buf[$ - T.sizeof .. $], range.front);
        }

        array = cast(T[]) buf;
    }
    return true;
}

///
@system unittest
{
    auto arr = theAllocator.makeArray!int([1, 2, 3]);
    assert(theAllocator.expandArray(arr, 2));
    assert(arr == [1, 2, 3, 0, 0]);
    import std.range : only;
    assert(theAllocator.expandArray(arr, only(4, 5)));
    assert(arr == [1, 2, 3, 0, 0, 4, 5]);
}

@system unittest
{
    auto arr = theAllocator.makeArray!int([1, 2, 3]);
    ForcedInputRange!int r;
    int[] b = [ 1, 2, 3, 4 ];
    auto temp = b;
    r.array = &temp;
    assert(theAllocator.expandArray(arr, r));
    assert(arr == [1, 2, 3, 1, 2, 3, 4]);
}

// Regression test for https://issues.dlang.org/show_bug.cgi?id=20929
@system unittest
{
    static void test(Char, Allocator)(auto ref Allocator alloc)
    {
        auto arr = alloc.makeArray!Char(1, Char('f'));

        import std.utf : byUTF;
        auto forwardRange = "oo".byUTF!Char();
        static assert(isForwardRange!(typeof(forwardRange)));
        // Test the forward-range code-path.
        assert(alloc.expandArray(arr, forwardRange));

        assert(arr == "foo");

        immutable(Char)[] temp = "bar";
        auto inputRange = ForcedInputRange!(immutable(Char))(&temp);
        // Test the input-range code-path.
        assert(alloc.expandArray(arr, inputRange));

        assert(arr == "foobar");
    }

    import std.experimental.allocator.gc_allocator : GCAllocator;
    test!char(GCAllocator.instance);
    test!wchar(GCAllocator.instance);
    test!char(theAllocator);
    test!wchar(theAllocator);
}

/**
Shrinks an array by `delta` elements.

If $(D array.length < delta), does nothing and returns `false`. Otherwise,
destroys the last $(D array.length - delta) elements in the array and then
reallocates the array's buffer. If reallocation fails, fills the array with
default-initialized data.

Params:
T = element type of the array being created
alloc = the allocator used for getting memory
array = a reference to the array being shrunk
delta = number of elements to remove (upon success the new length of `array` is $(D array.length - delta))

Returns:
`true` upon success, `false` if memory could not be reallocated. In the latter
case, the slice $(D array[$ - delta .. $]) is left with default-initialized
elements.

Throws:
The first two overloads throw only if `alloc`'s primitives do. The
overloads that involve copy initialization deallocate memory and propagate the
exception if the copy operation throws.
*/
bool shrinkArray(T, Allocator)(auto ref Allocator alloc,
        ref T[] array, size_t delta)
{
    if (delta > array.length) return false;

    // Destroy elements. If a destructor throws, fill the already destroyed
    // stuff with the default initializer.
    {
        size_t destroyed;
        scope(failure)
        {
            array[$ - delta .. $][0 .. destroyed].uninitializedFillDefault;
        }
        foreach (ref e; array[$ - delta .. $])
        {
            e.destroy;
            ++destroyed;
        }
    }

    if (delta == array.length)
    {
        alloc.deallocate(array);
        array = null;
        return true;
    }

    void[] buf = array;
    if (!alloc.reallocate(buf, buf.length - T.sizeof * delta))
    {
        // urgh, at least fill back with default
        array[$ - delta .. $].uninitializedFillDefault;
        return false;
    }
    array = cast(T[]) buf;
    return true;
}

///
@system unittest
{
    int[] a = theAllocator.makeArray!int(100, 42);
    assert(a.length == 100);
    assert(theAllocator.shrinkArray(a, 98));
    assert(a.length == 2);
    assert(a == [42, 42]);
}

@system unittest
{
    void test(A)(auto ref A alloc)
    {
        long[] a = alloc.makeArray!long((int[]).init);
        assert(a.length == 0 && a.ptr is null);
        a = alloc.makeArray!long(100, 42);
        assert(alloc.shrinkArray(a, 98));
        assert(a.length == 2);
        assert(a == [ 42, 42]);
    }
    import std.experimental.allocator.gc_allocator : GCAllocator;
    test(GCAllocator.instance);
    test(theAllocator);
}

/**

Destroys and then deallocates (using `alloc`) the object pointed to by a
pointer, the class object referred to by a `class` or `interface`
reference, or an entire array. It is assumed the respective entities had been
allocated with the same allocator.

*/
void dispose(A, T)(auto ref A alloc, auto ref T* p)
{
    static if (hasElaborateDestructor!T)
    {
        destroy(*p);
    }
    alloc.deallocate((cast(void*) p)[0 .. T.sizeof]);
    static if (__traits(isRef, p))
        p = null;
}

/// Ditto
void dispose(A, T)(auto ref A alloc, auto ref T p)
if (is(T == class) || is(T == interface))
{
    if (!p) return;
    static if (is(T == interface))
    {
        version (Windows)
        {
            import core.sys.windows.unknwn : IUnknown;
            static assert(!is(T: IUnknown), "COM interfaces can't be destroyed in "
                ~ __PRETTY_FUNCTION__);
        }
        auto ob = cast(Object) p;
    }
    else
        alias ob = p;
    auto support = (cast(void*) ob)[0 .. typeid(ob).initializer.length];
    destroy(p);
    alloc.deallocate(support);
    static if (__traits(isRef, p))
        p = null;
}

/// Ditto
void dispose(A, T)(auto ref A alloc, auto ref T[] array)
{
    static if (hasElaborateDestructor!(typeof(array[0])))
    {
        foreach (ref e; array)
        {
            destroy(e);
        }
    }
    alloc.deallocate(array);
    static if (__traits(isRef, array))
        array = null;
}

@system unittest
{
    static int x;
    static interface I
    {
        void method();
    }
    static class A : I
    {
        int y;
        override void method() { x = 21; }
        ~this() { x = 42; }
    }
    static class B : A
    {
    }
    auto a = theAllocator.make!A;
    a.method();
    assert(x == 21);
    theAllocator.dispose(a);
    assert(x == 42);

    B b = theAllocator.make!B;
    b.method();
    assert(x == 21);
    theAllocator.dispose(b);
    assert(x == 42);

    I i = theAllocator.make!B;
    i.method();
    assert(x == 21);
    theAllocator.dispose(i);
    assert(x == 42);

    int[] arr = theAllocator.makeArray!int(43);
    theAllocator.dispose(arr);
}

// https://issues.dlang.org/show_bug.cgi?id=16512
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    int* i = Mallocator.instance.make!int(0);
    Mallocator.instance.dispose(i);
    assert(i is null);

    Object o = Mallocator.instance.make!Object();
    Mallocator.instance.dispose(o);
    assert(o is null);

    uint* u = Mallocator.instance.make!uint(0);
    Mallocator.instance.dispose((){return u;}());
    assert(u !is null);

    uint[] ua = Mallocator.instance.makeArray!uint([0,1,2]);
    Mallocator.instance.dispose(ua);
    assert(ua is null);
}

// https://issues.dlang.org/show_bug.cgi?id=15721
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    interface Foo {}
    class Bar: Foo {}

    Bar bar;
    Foo foo;
    bar = Mallocator.instance.make!Bar;
    foo = cast(Foo) bar;
    Mallocator.instance.dispose(foo);
}

/**
Allocates a multidimensional array of elements of type T.

Params:
N = number of dimensions
T = element type of an element of the multidimensional arrat
alloc = the allocator used for getting memory
lengths = static array containing the size of each dimension

Returns:
An N-dimensional array with individual elements of type T.
*/
auto makeMultidimensionalArray(T, Allocator, size_t N)(auto ref Allocator alloc, size_t[N] lengths...)
{
    static if (N == 1)
    {
        return makeArray!T(alloc, lengths[0]);
    }
    else
    {
        alias E = typeof(makeMultidimensionalArray!(T, Allocator, N - 1)(alloc, lengths[1 .. $]));
        auto ret = makeArray!E(alloc, lengths[0]);
        foreach (ref e; ret)
            e = makeMultidimensionalArray!(T, Allocator, N - 1)(alloc, lengths[1 .. $]);
        return ret;
    }
}

///
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    auto mArray = Mallocator.instance.makeMultidimensionalArray!int(2, 3, 6);

    // deallocate when exiting scope
    scope(exit)
    {
        Mallocator.instance.disposeMultidimensionalArray(mArray);
    }

    assert(mArray.length == 2);
    foreach (lvl2Array; mArray)
    {
        assert(lvl2Array.length == 3);
        foreach (lvl3Array; lvl2Array)
            assert(lvl3Array.length == 6);
    }
}

/**
Destroys and then deallocates a multidimensional array, assuming it was
created with makeMultidimensionalArray and the same allocator was used.

Params:
T = element type of an element of the multidimensional array
alloc = the allocator used for getting memory
array = the multidimensional array that is to be deallocated
*/
void disposeMultidimensionalArray(T, Allocator)(auto ref Allocator alloc, auto ref T[] array)
{
    static if (isArray!T)
    {
        foreach (ref e; array)
            disposeMultidimensionalArray(alloc, e);
    }

    dispose(alloc, array);
    static if (__traits(isRef, array))
        array = null;
}

///
@system unittest
{
    struct TestAllocator
    {
        import std.experimental.allocator.common : platformAlignment;
        import std.experimental.allocator.mallocator : Mallocator;

        alias allocator = Mallocator.instance;

        private static struct ByteRange
        {
            void* ptr;
            size_t length;
        }

        private ByteRange[] _allocations;

        enum uint alignment = platformAlignment;

        void[] allocate(size_t numBytes)
        {
             auto ret = allocator.allocate(numBytes);
             _allocations ~= ByteRange(ret.ptr, ret.length);
             return ret;
        }

        bool deallocate(void[] bytes)
        {
            import std.algorithm.mutation : remove;
            import std.algorithm.searching : canFind;

            bool pred(ByteRange other)
            { return other.ptr == bytes.ptr && other.length == bytes.length; }

            assert(_allocations.canFind!pred);

             _allocations = _allocations.remove!pred;
             return allocator.deallocate(bytes);
        }

        ~this()
        {
            assert(!_allocations.length);
        }
    }

    TestAllocator allocator;

    auto mArray = allocator.makeMultidimensionalArray!int(2, 3, 5, 6, 7, 2);

    allocator.disposeMultidimensionalArray(mArray);
}

/**

Returns a dynamically-typed `CAllocator` built around a given statically-
typed allocator `a` of type `A`. Passing a pointer to the allocator
creates a dynamic allocator around the allocator pointed to by the pointer,
without attempting to copy or move it. Passing the allocator by value or
reference behaves as follows.

$(UL
$(LI If `A` has no state, the resulting object is allocated in static
shared storage.)
$(LI If `A` has state, the result will $(REF move, std,algorithm,mutation)
the supplied allocator $(D A a) within. The result itself is allocated in its
own statically-typed allocator.)
)

*/
RCIAllocator allocatorObject(A)(auto ref A a)
if (!isPointer!A)
{
    import core.lifetime : emplace;
    static if (stateSize!A == 0)
    {
        enum s = stateSize!(CAllocatorImpl!A).divideRoundUp(ulong.sizeof);
        __gshared ulong[s] state;
        __gshared RCIAllocator result;
        if (result.isNull)
        {
            // Don't care about a few races
            result = RCIAllocator(emplace!(CAllocatorImpl!A)(state[]));
        }
        assert(!result.isNull);
        return result;
    }
    else
    {
        auto state = a.allocate(stateSize!(CAllocatorImpl!A));
        import std.algorithm.mutation : move;
        import std.traits : hasMember;
        static if (hasMember!(A, "deallocate"))
        {
            scope(failure) a.deallocate(state);
        }
        auto tmp = cast(CAllocatorImpl!A) emplace!(CAllocatorImpl!A)(state);
        move(a, tmp.impl);
        return RCIAllocator(tmp);
    }
}

/// Ditto
RCIAllocator allocatorObject(A)(A* pa)
{
    assert(pa);
    import core.lifetime : emplace;
    auto state = pa.allocate(stateSize!(CAllocatorImpl!(A, Yes.indirect)));
    import std.traits : hasMember;
    static if (hasMember!(A, "deallocate"))
    {
        scope(failure) pa.deallocate(state);
    }
    return RCIAllocator(emplace!(CAllocatorImpl!(A, Yes.indirect))
                            (state, pa));
}

///
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    RCIAllocator a = allocatorObject(Mallocator.instance);
    auto b = a.allocate(100);
    assert(b.length == 100);
    assert(a.deallocate(b));

    // The in-situ region must be used by pointer
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    auto r = InSituRegion!1024();
    a = allocatorObject(&r);
    b = a.allocate(200);
    assert(b.length == 200);
    // In-situ regions can deallocate the last allocation
    assert(a.deallocate(b));
}

@system unittest
{
    import std.conv;
    import std.experimental.allocator.mallocator;
    import std.experimental.allocator.building_blocks.stats_collector;

    alias SCAlloc = StatsCollector!(Mallocator, Options.bytesUsed);
    SCAlloc statsCollectorAlloc;
    assert(statsCollectorAlloc.bytesUsed == 0);

    auto _allocator = allocatorObject(statsCollectorAlloc);
    // Ensure that the allocator was passed through in CAllocatorImpl
    // This allocator was used to allocate the chunk that holds the
    // CAllocatorImpl object; which is it's own wrapper
    assert((cast(CAllocatorImpl!(SCAlloc))(_allocator._alloc)).impl.bytesUsed
            == stateSize!(CAllocatorImpl!(SCAlloc)));
    _allocator.allocate(1);
    assert((cast(CAllocatorImpl!(SCAlloc))(_allocator._alloc)).impl.bytesUsed
            == stateSize!(CAllocatorImpl!(SCAlloc)) + 1);
}

/**

Returns a dynamically-typed `CSharedAllocator` built around a given statically-
typed allocator `a` of type `A`. Passing a pointer to the allocator
creates a dynamic allocator around the allocator pointed to by the pointer,
without attempting to copy or move it. Passing the allocator by value or
reference behaves as follows.

$(UL
$(LI If `A` has no state, the resulting object is allocated in static
shared storage.)
$(LI If `A` has state and is copyable, the result will
$(REF move, std,algorithm,mutation) the supplied allocator $(D A a) within.
The result itself is allocated in its own statically-typed allocator.)
$(LI If `A` has state and is not copyable, the result will move the
passed-in argument into the result. The result itself is allocated in its own
statically-typed allocator.)
)

*/
//nothrow @safe
//nothrow @nogc @safe
nothrow
RCISharedAllocator sharedAllocatorObject(A)(auto ref A a)
if (!isPointer!A)
{
    import core.lifetime : emplace;
    static if (stateSize!A == 0)
    {
        enum s = stateSize!(CSharedAllocatorImpl!A).divideRoundUp(ulong.sizeof);
        static shared ulong[s] state;
        static RCISharedAllocator result;
        if (result.isNull)
        {
            // Don't care about a few races
            result = RCISharedAllocator(
                    (cast(shared CSharedAllocatorImpl!A)(
                        emplace!(CSharedAllocatorImpl!A)(
                            (() @trusted => cast(ulong[]) state[])()))));
        }
        assert(!result.isNull);
        return result;
    }
    else static if (is(typeof({ shared A b = a; shared A c = b; }))) // copyable
    {
        auto state = a.allocate(stateSize!(CSharedAllocatorImpl!A));
        import std.algorithm.mutation : move;
        import std.traits : hasMember;
        static if (hasMember!(A, "deallocate"))
        {
            scope(failure) a.deallocate(state);
        }
        auto tmp = emplace!(shared CSharedAllocatorImpl!A)(state);
        move(a, tmp.impl);
        return RCISharedAllocator(tmp);
    }
    else // the allocator object is not copyable
    {
        assert(0, "Not yet implemented");
    }
}

/// Ditto
RCISharedAllocator sharedAllocatorObject(A)(A* pa)
{
    assert(pa);
    import core.lifetime : emplace;
    auto state = pa.allocate(stateSize!(CSharedAllocatorImpl!(A, Yes.indirect)));
    import std.traits : hasMember;
    static if (hasMember!(A, "deallocate"))
    {
        scope(failure) pa.deallocate(state);
    }
    return RCISharedAllocator(emplace!(shared CSharedAllocatorImpl!(A, Yes.indirect))(state, pa));
}


/**

Implementation of `IAllocator` using `Allocator`. This adapts a
statically-built allocator type to `IAllocator` that is directly usable by
non-templated code.

Usually `CAllocatorImpl` is used indirectly by calling $(LREF theAllocator).
*/
class CAllocatorImpl(Allocator, Flag!"indirect" indirect = No.indirect)
    : IAllocator
{
    import std.traits : hasMember;

    static if (stateSize!Allocator) private size_t rc = 1;

    /**
    The implementation is available as a public member.
    */
    static if (indirect)
    {
    nothrow:
        private Allocator* pimpl;

        @nogc pure @safe
        ref Allocator impl()
        {
            return *pimpl;
        }

        @nogc pure @safe
        this(Allocator* pa)
        {
            pimpl = pa;
        }
    }
    else
    {
        static if (stateSize!Allocator) Allocator impl;
        else alias impl = Allocator.instance;
    }

nothrow:
    /// Returns `impl.alignment`.
    override @property uint alignment()
    {
        return impl.alignment;
    }

    /**
    Returns `impl.goodAllocSize(s)`.
    */
    override size_t goodAllocSize(size_t s)
    {
        return impl.goodAllocSize(s);
    }

    /**
    Returns `impl.allocate(s)`.
    */
    override void[] allocate(size_t s, TypeInfo ti = null)
    {
        return impl.allocate(s);
    }

    /**
    If `impl.alignedAllocate` exists, calls it and returns the result.
    Otherwise, always returns `null`.
    */
    override void[] alignedAllocate(size_t s, uint a)
    {
        static if (hasMember!(Allocator, "alignedAllocate"))
            return impl.alignedAllocate(s, a);
        else
            return null;
    }

    /**
    If `Allocator` implements `owns`, forwards to it. Otherwise, returns
    `Ternary.unknown`.
    */
    override Ternary owns(void[] b)
    {
        static if (hasMember!(Allocator, "owns")) return impl.owns(b);
        else return Ternary.unknown;
    }

    /// Returns $(D impl.expand(b, s)) if defined, `false` otherwise.
    override bool expand(ref void[] b, size_t s)
    {
        static if (hasMember!(Allocator, "expand"))
            return impl.expand(b, s);
        else
            return s == 0;
    }

    /// Returns $(D impl.reallocate(b, s)).
    override bool reallocate(ref void[] b, size_t s)
    {
        return impl.reallocate(b, s);
    }

    /// Forwards to `impl.alignedReallocate` if defined, `false` otherwise.
    bool alignedReallocate(ref void[] b, size_t s, uint a)
    {
        static if (!hasMember!(Allocator, "alignedAllocate"))
        {
            return false;
        }
        else
        {
            return impl.alignedReallocate(b, s, a);
        }
    }

    // Undocumented for now
    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        static if (hasMember!(Allocator, "resolveInternalPointer"))
        {
            return impl.resolveInternalPointer(p, result);
        }
        else
        {
            return Ternary.unknown;
        }
    }

    /**
    If `impl.deallocate` is not defined, returns `false`. Otherwise it forwards
    the call.
    */
    override bool deallocate(void[] b)
    {
        static if (hasMember!(Allocator, "deallocate"))
        {
            return impl.deallocate(b);
        }
        else
        {
            return false;
        }
    }

    /**
    Calls `impl.deallocateAll()` and returns the result if defined,
    otherwise returns `false`.
    */
    override bool deallocateAll()
    {
        static if (hasMember!(Allocator, "deallocateAll"))
        {
            return impl.deallocateAll();
        }
        else
        {
            return false;
        }
    }

    /**
    Forwards to `impl.empty()` if defined, otherwise returns `Ternary.unknown`.
    */
    override Ternary empty()
    {
        static if (hasMember!(Allocator, "empty"))
        {
            return Ternary(impl.empty);
        }
        else
        {
            return Ternary.unknown;
        }
    }

    /**
    Returns `impl.allocateAll()` if present, `null` otherwise.
    */
    override void[] allocateAll()
    {
        static if (hasMember!(Allocator, "allocateAll"))
        {
            return impl.allocateAll();
        }
        else
        {
            return null;
        }
    }

    @nogc nothrow pure @safe
    override void incRef()
    {
        static if (stateSize!Allocator) ++rc;
    }

    @nogc nothrow pure @trusted
    override bool decRef()
    {
        static if (stateSize!Allocator)
        {
            import core.stdc.string : memcpy;

            if (rc == 1)
            {
                static if (indirect)
                {
                    Allocator* tmp = pimpl;
                }
                else
                {
                    Allocator tmp;
                    memcpy(&tmp, &this.impl, Allocator.sizeof);
                }
                void[] support = (cast(void*) this)[0 .. stateSize!(typeof(this))];
                tmp.deallocate(support);
                return false;
            }

            --rc;
            return true;
        }
        else
        {
            return true;
        }
    }
}

/**

Implementation of `ISharedAllocator` using `Allocator`. This adapts a
statically-built, shareable across threads, allocator type to `ISharedAllocator`
that is directly usable by non-templated code.

Usually `CSharedAllocatorImpl` is used indirectly by calling
$(LREF processAllocator).
*/
class CSharedAllocatorImpl(Allocator, Flag!"indirect" indirect = No.indirect)
    : ISharedAllocator
{
    import std.traits : hasMember;
    import core.atomic : atomicOp, atomicLoad;

    static if (stateSize!Allocator) shared size_t rc = 1;

    /**
    The implementation is available as a public member.
    */
    static if (indirect)
    {
    nothrow:
        private shared Allocator* pimpl;

        @nogc pure @safe
        ref Allocator impl() shared
        {
            return *pimpl;
        }

        @nogc pure @safe
        this(Allocator* pa) shared
        {
            pimpl = pa;
        }
    }
    else
    {
        static if (stateSize!Allocator) shared Allocator impl;
        else alias impl = Allocator.instance;
    }

nothrow:
    /// Returns `impl.alignment`.
    override @property uint alignment() shared
    {
        return impl.alignment;
    }

    /**
    Returns `impl.goodAllocSize(s)`.
    */
    override size_t goodAllocSize(size_t s) shared
    {
        return impl.goodAllocSize(s);
    }

    /**
    Returns `impl.allocate(s)`.
    */
    override void[] allocate(size_t s, TypeInfo ti = null) shared
    {
        return impl.allocate(s);
    }

    /**
    If `impl.alignedAllocate` exists, calls it and returns the result.
    Otherwise, always returns `null`.
    */
    override void[] alignedAllocate(size_t s, uint a) shared
    {
        static if (hasMember!(Allocator, "alignedAllocate"))
            return impl.alignedAllocate(s, a);
        else
            return null;
    }

    /**
    If `Allocator` implements `owns`, forwards to it. Otherwise, returns
    `Ternary.unknown`.
    */
    override Ternary owns(void[] b) shared
    {
        static if (hasMember!(Allocator, "owns")) return impl.owns(b);
        else return Ternary.unknown;
    }

    /// Returns $(D impl.expand(b, s)) if defined, `false` otherwise.
    override bool expand(ref void[] b, size_t s) shared
    {
        static if (hasMember!(Allocator, "expand"))
            return impl.expand(b, s);
        else
            return s == 0;
    }

    /// Returns $(D impl.reallocate(b, s)).
    override bool reallocate(ref void[] b, size_t s) shared
    {
        return impl.reallocate(b, s);
    }

    /// Forwards to `impl.alignedReallocate` if defined, `false` otherwise.
    bool alignedReallocate(ref void[] b, size_t s, uint a) shared
    {
        static if (!hasMember!(Allocator, "alignedAllocate"))
        {
            return false;
        }
        else
        {
            return impl.alignedReallocate(b, s, a);
        }
    }

    // Undocumented for now
    Ternary resolveInternalPointer(const void* p, ref void[] result) shared
    {
        static if (hasMember!(Allocator, "resolveInternalPointer"))
        {
            return impl.resolveInternalPointer(p, result);
        }
        else
        {
            return Ternary.unknown;
        }
    }

    /**
    If `impl.deallocate` is not defined, returns `false`. Otherwise it forwards
    the call.
    */
    override bool deallocate(void[] b) shared
    {
        static if (hasMember!(Allocator, "deallocate"))
        {
            return impl.deallocate(b);
        }
        else
        {
            return false;
        }
    }

    /**
    Calls `impl.deallocateAll()` and returns the result if defined,
    otherwise returns `false`.
    */
    override bool deallocateAll() shared
    {
        static if (hasMember!(Allocator, "deallocateAll"))
        {
            return impl.deallocateAll();
        }
        else
        {
            return false;
        }
    }

    /**
    Forwards to `impl.empty()` if defined, otherwise returns `Ternary.unknown`.
    */
    override Ternary empty() shared
    {
        static if (hasMember!(Allocator, "empty"))
        {
            return Ternary(impl.empty);
        }
        else
        {
            return Ternary.unknown;
        }
    }

    /**
    Returns `impl.allocateAll()` if present, `null` otherwise.
    */
    override void[] allocateAll() shared
    {
        static if (hasMember!(Allocator, "allocateAll"))
        {
            return impl.allocateAll();
        }
        else
        {
            return null;
        }
    }

    @nogc nothrow pure @safe
    override void incRef() shared
    {
        static if (stateSize!Allocator) atomicOp!"+="(rc, 1);
    }

    @nogc nothrow pure @trusted
    override bool decRef() shared
    {
        static if (stateSize!Allocator)
        {
            import core.stdc.string : memcpy;

            // rc starts as 1 to avoid comparing with size_t(0) - 1
            if (atomicOp!"-="(rc, 1) == 0)
            {
                static if (indirect)
                {
                    Allocator* tmp = pimpl;
                }
                else
                {
                    Allocator tmp;
                    memcpy(cast(void*) &tmp, cast(void*) &this.impl, Allocator.sizeof);
                    Allocator empty;
                    memcpy(cast(void*) &this.impl, cast(void*) &empty, Allocator.sizeof);
                }
                void[] support = (cast(void*) this)[0 .. stateSize!(typeof(this))];
                (cast(bool delegate(void[]) @nogc nothrow pure)(&tmp.deallocate))(support);
                return false;
            }
            return true;
        }
        else
        {
            return true;
        }
    }
}


// Example in intro above
@system unittest
{
    // Allocate an int, initialize it with 42
    int* p = theAllocator.make!int(42);
    assert(*p == 42);

    // Destroy and deallocate it
    theAllocator.dispose(p);

    // Allocate using the global process allocator
    p = processAllocator.make!int(100);
    assert(*p == 100);

    // Destroy and deallocate
    processAllocator.dispose(p);

    // Create an array of 50 doubles initialized to -1.0
    double[] arr = theAllocator.makeArray!double(50, -1.0);

    // Check internal pointer
    void[] result;
    assert(theAllocator.resolveInternalPointer(null, result) == Ternary.no);
    Ternary r = theAllocator.resolveInternalPointer(arr.ptr, result);
    assert(result.ptr is arr.ptr && result.length >= arr.length);

    // Append two zeros to it
    theAllocator.expandArray(arr, 2, 0.0);
    // On second thought, take that back
    theAllocator.shrinkArray(arr, 2);
    // Destroy and deallocate
    theAllocator.dispose(arr);
}

/**

Stores an allocator object in thread-local storage (i.e. non-`shared` D
global). `ThreadLocal!A` is a subtype of `A` so it appears to implement
`A`'s allocator primitives.

`A` must hold state, otherwise `ThreadLocal!A` refuses instantiation. This
means e.g. `ThreadLocal!Mallocator` does not work because `Mallocator`'s
state is not stored as members of `Mallocator`, but instead is hidden in the
C library implementation.

*/
struct ThreadLocal(A)
{
    static assert(stateSize!A,
        typeof(A).stringof
        ~ " does not have state so it cannot be used with ThreadLocal");

    /**
    The allocator instance.
    */
    static A instance;

    /**
    `ThreadLocal!A` is a subtype of `A` so it appears to implement `A`'s
    allocator primitives.
    */
    alias instance this;

    /**
    `ThreadLocal` disables all constructors. The intended usage is
    `ThreadLocal!A.instance`.
    */
    @disable this();
    /// Ditto
    @disable this(this);
}

///
@system
unittest
{
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;

    static assert(!is(ThreadLocal!Mallocator));
    static assert(!is(ThreadLocal!GCAllocator));
    alias Allocator = ThreadLocal!(FreeList!(GCAllocator, 0, 8));
    auto b = Allocator.instance.allocate(5);
    static assert(__traits(hasMember, Allocator, "allocate"));
}

/*
(Not public.)

A binary search tree that uses no allocation of its own. Instead, it relies on
user code to allocate nodes externally. Then `EmbeddedTree`'s primitives wire
the nodes appropriately.

Warning: currently `EmbeddedTree` is not using rebalancing, so it may
degenerate. A red-black tree implementation storing the color with one of the
pointers is planned for the future.
*/
private struct EmbeddedTree(T, alias less)
{
    static struct Node
    {
        T payload;
        Node* left, right;
    }

    private Node* root;

    private Node* insert(Node* n, ref Node* backref)
    {
        backref = n;
        n.left = n.right = null;
        return n;
    }

    Node* find(Node* data)
    {
        for (auto n = root; n; )
        {
            if (less(data, n))
            {
                n = n.left;
            }
            else if (less(n, data))
            {
                n = n.right;
            }
            else
            {
                return n;
            }
        }
        return null;
    }

    Node* insert(Node* data)
    {
        if (!root)
        {
            root = data;
            data.left = data.right = null;
            return root;
        }
        auto n = root;
        for (;;)
        {
            if (less(data, n))
            {
                if (!n.left)
                {
                    // Found insertion point
                    return insert(data, n.left);
                }
                n = n.left;
            }
            else if (less(n, data))
            {
                if (!n.right)
                {
                    // Found insertion point
                    return insert(data, n.right);
                }
                n = n.right;
            }
            else
            {
                // Found
                return n;
            }
            if (!n) return null;
        }
    }

    Node* remove(Node* data)
    {
        auto n = root;
        Node* parent = null;
        for (;;)
        {
            if (!n) return null;
            if (less(data, n))
            {
                parent = n;
                n = n.left;
            }
            else if (less(n, data))
            {
                parent = n;
                n = n.right;
            }
            else
            {
                // Found
                remove(n, parent);
                return n;
            }
        }
    }

    private void remove(Node* n, Node* parent)
    {
        assert(n);
        assert(!parent || parent.left == n || parent.right == n);
        Node** referrer = parent
            ? (parent.left == n ? &parent.left : &parent.right)
            : &root;
        if (!n.left)
        {
            *referrer = n.right;
        }
        else if (!n.right)
        {
            *referrer = n.left;
        }
        else
        {
            // Find the leftmost child in the right subtree
            auto leftmost = n.right;
            Node** leftmostReferrer = &n.right;
            while (leftmost.left)
            {
                leftmostReferrer = &leftmost.left;
                leftmost = leftmost.left;
            }
            // Unlink leftmost from there
            *leftmostReferrer = leftmost.right;
            // Link leftmost in lieu of n
            leftmost.left = n.left;
            leftmost.right = n.right;
            *referrer = leftmost;
        }
    }

    Ternary empty() const
    {
        return Ternary(!root);
    }

    void dump()
    {
        import std.stdio : writeln;
        writeln(typeid(this), " @ ", cast(void*) &this);
        dump(root, 3);
    }

    void dump(Node* r, uint indent)
    {
        import std.stdio : write, writeln;
        import std.range : repeat;
        import std.array : array;

        write(repeat(' ', indent).array);
        if (!r)
        {
            writeln("(null)");
            return;
        }
        writeln(r.payload, " @ ", cast(void*) r);
        dump(r.left, indent + 3);
        dump(r.right, indent + 3);
    }

    void assertSane()
    {
        static bool isBST(Node* r, Node* lb, Node* ub)
        {
            if (!r) return true;
            if (lb && !less(lb, r)) return false;
            if (ub && !less(r, ub)) return false;
            return isBST(r.left, lb, r) &&
                isBST(r.right, r, ub);
        }
        if (isBST(root, null, null)) return;
        dump;
        assert(0);
    }
}

@system
unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;

    alias a = GCAllocator.instance;
    alias Tree = EmbeddedTree!(int, (a, b) => a.payload < b.payload);
    Tree t;
    assert(t.empty == Ternary.yes);
    int[] vals = [ 6, 3, 9, 1, 0, 2, 8, 11 ];
    foreach (v; vals)
    {
        auto n = new Tree.Node(v, null, null);
        assert(t.insert(n));
        assert(n);
        t.assertSane;
    }
    assert(t.empty != Ternary.yes);
    foreach (v; vals)
    {
        Tree.Node n = { v };
        assert(t.remove(&n));
        t.assertSane;
    }
    assert(t.empty == Ternary.yes);
}

/*

`InternalPointersTree` adds a primitive on top of another allocator: calling
`resolveInternalPointer(p)` returns the block within which the internal
pointer `p` lies. Pointers right after the end of allocated blocks are also
considered internal.

The implementation stores three additional words with each allocation (one for
the block size and two for search management).

*/
private struct InternalPointersTree(Allocator)
{
    import std.experimental.allocator.building_blocks.affix_allocator : AffixAllocator;

    alias Tree = EmbeddedTree!(size_t,
        (a, b) => cast(void*) a + a.payload < cast(void*) b);
    alias Parent = AffixAllocator!(Allocator, Tree.Node);

    // Own state
    private Tree blockMap;

    alias alignment = Parent.alignment;

    /**
    The implementation is available as a public member.
    */
    static if (stateSize!Parent) Parent parent;
    else alias parent = Parent.instance;

    /// Allocator API.
    void[] allocate(size_t bytes)
    {
        auto r = parent.allocate(bytes);
        if (!r.ptr) return r;
        Tree.Node* n = &parent.prefix(r);
        n.payload = bytes;
        blockMap.insert(n) || assert(0);
        return r;
    }

    /// Ditto
    bool deallocate(void[] b)
    {
        if (!b.ptr) return true;
        Tree.Node* n = &parent.prefix(b);
        blockMap.remove(n) || assert(false);
        parent.deallocate(b);
        return true;
    }

    /// Ditto
    static if (hasMember!(Allocator, "reallocate"))
    bool reallocate(ref void[] b, size_t s)
    {
        auto n = &parent.prefix(b);
        assert(n.payload == b.length);
        blockMap.remove(n) || assert(0);
        if (!parent.reallocate(b, s))
        {
            // Failed, must reinsert the same node in the tree
            assert(n.payload == b.length);
            blockMap.insert(n) || assert(0);
            return false;
        }
        // Insert the new node
        n = &parent.prefix(b);
        n.payload = s;
        blockMap.insert(n) || assert(0);
        return true;
    }

    /// Ditto
    Ternary owns(void[] b)
    {
        void[] result;
        return resolveInternalPointer(b.ptr, result);
    }

    /// Ditto
    Ternary empty()
    {
        return Ternary(blockMap.empty);
    }

    /** Returns the block inside which `p` resides, or `null` if the
    pointer does not belong.
    */
    pure nothrow @safe @nogc
    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        // Must define a custom find
        Tree.Node* find()
        {
            for (auto n = blockMap.root; n; )
            {
                if (p < n)
                {
                    n = n.left;
                }
                else if ((() @trusted => p > (cast(void*) (n + 1)) + n.payload)())
                {
                    n = n.right;
                }
                else
                {
                    return n;
                }
            }
            return null;
        }

        auto n = find();
        if (!n) return Ternary.no;
        result = (() @trusted => (cast(void*) (n + 1))[0 .. n.payload])();
        return Ternary.yes;
    }
}

@system
unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.random : randomCover;

    InternalPointersTree!(Mallocator) a;
    int[] vals = [ 6, 3, 9, 1, 2, 8, 11 ];
    void[][] allox;
    foreach (v; vals)
    {
        allox ~= a.allocate(v);
    }
    a.blockMap.assertSane;

    foreach (b; allox)
    {
        () pure nothrow @safe {
            void[] p;
            Ternary r = (() @nogc => a.resolveInternalPointer(&b[0], p))();
            assert(&p[0] == &b[0] && p.length >= b.length);
            r = a.resolveInternalPointer((() @trusted => &b[0] + b.length)(), p);

            /* This line randomly fails on MacOS 12.x x64
             * https://issues.dlang.org/show_bug.cgi?id=22660
             * Commenting it out until someone can fix it.
             */
            //assert(&p[0] == &b[0] && p.length >= b.length);

            r = a.resolveInternalPointer((() @trusted => &b[0] + b.length / 2)(), p);
            assert(&p[0] == &b[0] && p.length >= b.length);
            auto bogus = new void[b.length];
            assert(a.resolveInternalPointer(&bogus[0], p) == Ternary.no);
        }();
    }

    foreach (b; allox.randomCover)
    {
        () nothrow @nogc { a.deallocate(b); }();
    }

    assert(a.empty == Ternary.yes);
}

//version (std_allocator_benchmark)
@system
unittest
{
    import std.experimental.allocator.building_blocks.null_allocator : NullAllocator;
    import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.experimental.allocator.building_blocks.segregator : Segregator;
    import std.experimental.allocator.building_blocks.bucketizer : Bucketizer;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;

    static void testSpeed(A)()
    {
        static if (stateSize!A) A a;
        else alias a = A.instance;

        void[][128] bufs;

        import std.random;
        foreach (i; 0 .. 100_000)
        {
            auto j = uniform(0, bufs.length);
            switch (uniform(0, 2))
            {
            case 0:
                () nothrow @nogc { a.deallocate(bufs[j]); }();
                bufs[j] = a.allocate(uniform(0, 4096));
                break;
            case 1:
                () nothrow @nogc { a.deallocate(bufs[j]); }();
                bufs[j] = null;
                break;
            default:
                assert(0);
            }
        }
    }

    import std.algorithm.comparison : max;

    alias FList = FreeList!(GCAllocator, 0, unbounded);
    alias A = Segregator!(
        8, FreeList!(GCAllocator, 0, 8),
        128, Bucketizer!(FList, 1, 128, 16),
        256, Bucketizer!(FList, 129, 256, 32),
        512, Bucketizer!(FList, 257, 512, 64),
        1024, Bucketizer!(FList, 513, 1024, 128),
        2048, Bucketizer!(FList, 1025, 2048, 256),
        3584, Bucketizer!(FList, 2049, 3584, 512),
        4072 * 1024, AllocatorList!(
            (size_t n) => BitmappedBlock!(4096)(cast(ubyte[]) GCAllocator.instance.allocate(
                max(n, 4072 * 1024)))),
        GCAllocator
    );

    import std.stdio;
    import std.conv : to;
    import std.datetime.stopwatch;
    import std.algorithm.iteration : map;

    if (false) writeln(benchmark!(
        testSpeed!NullAllocator,
        testSpeed!Mallocator,
        testSpeed!GCAllocator,
        testSpeed!(ThreadLocal!A),
        testSpeed!(A),
    )(20)[].map!(t => t.to!Duration));
}

@system
unittest
{
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    import std.experimental.allocator.building_blocks.fallback_allocator : FallbackAllocator;
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.experimental.allocator.mallocator : Mallocator;

    auto a = allocatorObject(Mallocator.instance);
    auto b = a.allocate(100);
    assert(b.length == 100);

    FreeList!(GCAllocator, 0, 8) fl;
    auto sa = allocatorObject(fl);
    b = a.allocate(101);
    assert(b.length == 101);

    FallbackAllocator!(InSituRegion!(10240, 64), GCAllocator) fb;
    // Doesn't work yet...
    //a = allocatorObject(fb);
    //b = a.allocate(102);
    //assert(b.length == 102);
}

///
@system
unittest
{
    import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;
    import std.experimental.allocator.building_blocks.bitmapped_block : BitmappedBlock;
    import std.experimental.allocator.building_blocks.segregator : Segregator;
    import std.experimental.allocator.building_blocks.bucketizer : Bucketizer;
    import std.experimental.allocator.building_blocks.free_list : FreeList;
    import std.experimental.allocator.gc_allocator : GCAllocator;

    /// Define an allocator bound to the built-in GC.
    auto alloc = allocatorObject(GCAllocator.instance);
    auto b = alloc.allocate(42);
    assert(b.length == 42);
    assert(alloc.deallocate(b));

    import std.algorithm.comparison : max;
    // Define an elaborate allocator and bind it to the class API.
    alias FList = FreeList!(GCAllocator, 0, unbounded);
    alias A = ThreadLocal!(
        Segregator!(
            8, FreeList!(GCAllocator, 0, 8),
            128, Bucketizer!(FList, 1, 128, 16),
            256, Bucketizer!(FList, 129, 256, 32),
            512, Bucketizer!(FList, 257, 512, 64),
            1024, Bucketizer!(FList, 513, 1024, 128),
            2048, Bucketizer!(FList, 1025, 2048, 256),
            3584, Bucketizer!(FList, 2049, 3584, 512),
            4072 * 1024, AllocatorList!(
                (n) => BitmappedBlock!(4096)(cast(ubyte[]) GCAllocator.instance.allocate(
                    max(n, 4072 * 1024)))),
            GCAllocator
        )
    );

    auto alloc2 = allocatorObject(A.instance);
    b = alloc2.allocate(101);
    assert(alloc2.deallocate(b));
}

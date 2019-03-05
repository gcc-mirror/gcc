/**
$(H2 Assembling Your Own Allocator)

In addition to defining the interfaces above, this package also implements
untyped composable memory allocators. They are $(I untyped) because they deal
exclusively in $(D void[]) and have no notion of what type the memory allocated
would be destined for. They are $(I composable) because the included allocators
are building blocks that can be assembled in complex nontrivial allocators.

$(P Unlike the allocators for the C and C++ programming languages, which manage
the allocated size internally, these allocators require that the client
maintains (or knows $(I a priori)) the allocation size for each piece of memory
allocated. Put simply, the client must pass the allocated size upon
deallocation. Storing the size in the _allocator has significant negative
performance implications, and is virtually always redundant because client code
needs knowledge of the allocated size in order to avoid buffer overruns. (See
more discussion in a $(HTTP open-
std.org/JTC1/SC22/WG21/docs/papers/2013/n3536.html, proposal) for sized
deallocation in C++.) For this reason, allocators herein traffic in $(D void[])
as opposed to $(D void*).)

$(P In order to be usable as an _allocator, a type should implement the
following methods with their respective semantics. Only $(D alignment) and  $(D
allocate) are required. If any of the other methods is missing, the _allocator
is assumed to not have that capability (for example some allocators do not offer
manual deallocation of memory). Allocators should NOT implement
unsupported methods to always fail. For example, an allocator that lacks the
capability to implement `alignedAllocate` should not define it at all (as
opposed to defining it to always return `null` or throw an exception). The
missing implementation statically informs other components about the
allocator's capabilities and allows them to make design decisions accordingly.)

$(BOOKTABLE ,
$(TR $(TH Method name) $(TH Semantics))

$(TR $(TDC uint alignment;, $(POST $(RES) > 0)) $(TD Returns the minimum
alignment of all data returned by the allocator. An allocator may implement $(D
alignment) as a statically-known $(D enum) value only. Applications that need
dynamically-chosen alignment values should use the $(D alignedAllocate) and $(D
alignedReallocate) APIs.))

$(TR $(TDC size_t goodAllocSize(size_t n);, $(POST $(RES) >= n)) $(TD Allocators
customarily allocate memory in discretely-sized chunks. Therefore, a request for
$(D n) bytes may result in a larger allocation. The extra memory allocated goes
unused and adds to the so-called $(HTTP goo.gl/YoKffF,internal fragmentation).
The function $(D goodAllocSize(n)) returns the actual number of bytes that would
be allocated upon a request for $(D n) bytes. This module defines a default
implementation that returns $(D n) rounded up to a multiple of the allocator's
alignment.))

$(TR $(TDC void[] allocate(size_t s);, $(POST $(RES) is null || $(RES).length ==
s)) $(TD If $(D s == 0), the call may return any empty slice (including $(D
null)). Otherwise, the call allocates $(D s) bytes of memory and returns the
allocated block, or $(D null) if the request could not be satisfied.))

$(TR $(TDC void[] alignedAllocate(size_t s, uint a);, $(POST $(RES) is null ||
$(RES).length == s)) $(TD Similar to `allocate`, with the additional
guarantee that the memory returned is aligned to at least `a` bytes. `a`
must be a power of 2.))

$(TR $(TDC void[] allocateAll();) $(TD Offers all of allocator's memory to the
caller, so it's usually defined by fixed-size allocators. If the allocator is
currently NOT managing any memory, then $(D allocateAll()) shall allocate and
return all memory available to the allocator, and subsequent calls to all
allocation primitives should not succeed (e.g. $(D allocate) shall return $(D
null) etc). Otherwise, $(D allocateAll) only works on a best-effort basis, and
the allocator is allowed to return $(D null) even if does have available memory.
Memory allocated with $(D allocateAll) is not otherwise special (e.g. can be
reallocated or deallocated with the usual primitives, if defined).))

$(TR $(TDC bool expand(ref void[] b, size_t delta);, $(POST !$(RES) || b.length
== $(I old)(b).length + delta)) $(TD Expands $(D b) by $(D delta) bytes. If $(D
delta == 0), succeeds without changing $(D b). If $(D b is null), returns
`false` (the null pointer cannot be expanded in place). Otherwise, $(D
b) must be a buffer previously allocated with the same allocator. If expansion
was successful, $(D expand) changes $(D b)'s length to $(D b.length + delta) and
returns $(D true). Upon failure, the call effects no change upon the allocator
object, leaves $(D b) unchanged, and returns $(D false).))

$(TR $(TDC bool reallocate(ref void[] b, size_t s);, $(POST !$(RES) || b.length
== s)) $(TD Reallocates $(D b) to size $(D s), possibly moving memory around.
$(D b) must be $(D null) or a buffer allocated with the same allocator. If
reallocation was successful, $(D reallocate) changes $(D b) appropriately and
returns $(D true). Upon failure, the call effects no change upon the allocator
object, leaves $(D b) unchanged, and returns $(D false). An allocator should
implement $(D reallocate) if it can derive some advantage from doing so;
otherwise, this module defines a $(D reallocate) free function implemented in
terms of $(D expand), $(D allocate), and $(D deallocate).))

$(TR $(TDC bool alignedReallocate(ref void[] b,$(BR) size_t s, uint a);, $(POST
!$(RES) || b.length == s)) $(TD Similar to $(D reallocate), but guarantees the
reallocated memory is aligned at $(D a) bytes. The buffer must have been
originated with a call to $(D alignedAllocate). $(D a) must be a power of 2
greater than $(D (void*).sizeof). An allocator should implement $(D
alignedReallocate) if it can derive some advantage from doing so; otherwise,
this module defines a $(D alignedReallocate) free function implemented in terms
of $(D expand), $(D alignedAllocate), and $(D deallocate).))

$(TR $(TDC Ternary owns(void[] b);) $(TD Returns `Ternary.yes` if `b` has been
allocated with this allocator. An allocator should define this method only if it
can decide on ownership precisely and fast (in constant time, logarithmic time,
or linear time with a low multiplication factor). Traditional allocators such as
the C heap do not define such functionality. If $(D b is null), the allocator
shall return `Ternary.no`, i.e. no allocator owns the `null` slice.))

$(TR $(TDC Ternary resolveInternalPointer(void* p, ref void[] result);) $(TD If
`p` is a pointer somewhere inside a block allocated with this allocator,
`result` holds a pointer to the beginning of the allocated block and returns
`Ternary.yes`. Otherwise, `result` holds `null` and returns `Ternary.no`.
If the pointer points immediately after an allocated block, the result is
implementation defined.))

$(TR $(TDC bool deallocate(void[] b);) $(TD If $(D b is null), does
nothing and returns `true`. Otherwise, deallocates memory previously allocated
with this allocator and returns `true` if successful, `false` otherwise. An
implementation that would not support deallocation (i.e. would always return
`false` should not define this primitive at all.)))

$(TR $(TDC bool deallocateAll();, $(POST empty)) $(TD Deallocates all memory
allocated with this allocator. If an allocator implements this method, it must
specify whether its destructor calls it, too.))

$(TR $(TDC Ternary empty();) $(TD Returns `Ternary.yes` if and only if the
allocator holds no memory (i.e. no allocation has occurred, or all allocations
have been deallocated).))

$(TR $(TDC static Allocator instance;, $(POST instance $(I is a valid)
Allocator $(I object))) $(TD Some allocators are $(I monostate), i.e. have only
an instance and hold only global state. (Notable examples are C's own
`malloc`-based allocator and D's garbage-collected heap.) Such allocators must
define a static $(D instance) instance that serves as the symbolic placeholder
for the global instance of the allocator. An allocator should not hold state
and define `instance` simultaneously. Depending on whether the allocator is
thread-safe or not, this instance may be $(D shared).))
)

$(H2 Sample Assembly)

The example below features an _allocator modeled after $(HTTP goo.gl/m7329l,
jemalloc), which uses a battery of free-list allocators spaced so as to keep
internal fragmentation to a minimum. The $(D FList) definitions specify no
bounds for the freelist because the $(D Segregator) does all size selection in
advance.

Sizes through 3584 bytes are handled via freelists of staggered sizes. Sizes
from 3585 bytes through 4072 KB are handled by a $(D BitmappedBlock) with a
block size of 4 KB. Sizes above that are passed direct to the $(D GCAllocator).

----
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
            () => BitmappedBlock!(GCAllocator, 4096)(4072 * 1024)),
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
----

$(H2 Allocating memory for sharing across threads)

One allocation pattern used in multithreaded applications is to share memory
across threads, and to deallocate blocks in a different thread than the one that
allocated it.

All allocators in this module accept and return $(D void[]) (as opposed to
$(D shared void[])). This is because at the time of allocation, deallocation, or
reallocation, the memory is effectively not $(D shared) (if it were, it would
reveal a bug at the application level).

The issue remains of calling $(D a.deallocate(b)) from a different thread than
the one that allocated $(D b). It follows that both threads must have access to
the same instance $(D a) of the respective allocator type. By definition of D,
this is possible only if $(D a) has the $(D shared) qualifier. It follows that
the allocator type must implement $(D allocate) and $(D deallocate) as $(D
shared) methods. That way, the allocator commits to allowing usable $(D shared)
instances.

Conversely, allocating memory with one non-$(D shared) allocator, passing it
across threads (by casting the obtained buffer to $(D shared)), and later
deallocating it in a different thread (either with a different allocator object
or with the same allocator object after casting it to $(D shared)) is illegal.

$(H2 Building Blocks)

$(P The table below gives a synopsis of predefined allocator building blocks,
with their respective modules. Either `import` the needed modules individually,
or `import` `std.experimental.building_blocks`, which imports them all
`public`ly. The building blocks can be assembled in unbounded ways and also
combined with your own. For a collection of typical and useful preassembled
allocators and for inspiration in defining more such assemblies, refer to
$(MREF std,experimental,allocator,showcase).)

$(BOOKTABLE,
$(TR $(TH Allocator$(BR)) $(TH Description))

$(TR $(TDC2 NullAllocator, null_allocator) $(TD Very good at doing absolutely nothing. A good
starting point for defining other allocators or for studying the API.))

$(TR $(TDC3 GCAllocator, gc_allocator) $(TD The system-provided garbage-collector allocator.
This should be the default fallback allocator tapping into system memory. It
offers manual $(D free) and dutifully collects litter.))

$(TR $(TDC3 Mallocator, mallocator) $(TD The C heap _allocator, a.k.a. $(D
malloc)/$(D realloc)/$(D free). Use sparingly and only for code that is unlikely
to leak.))

$(TR $(TDC3 AlignedMallocator, mallocator) $(TD Interface to OS-specific _allocators that
support specifying alignment:
$(HTTP man7.org/linux/man-pages/man3/posix_memalign.3.html, $(D posix_memalign))
on Posix and $(HTTP msdn.microsoft.com/en-us/library/fs9stz4e(v=vs.80).aspx,
$(D __aligned_xxx)) on Windows.))

$(TR $(TDC2 AffixAllocator, affix_allocator) $(TD Allocator that allows and manages allocating
extra prefix and/or a suffix bytes for each block allocated.))

$(TR $(TDC2 BitmappedBlock, bitmapped_block) $(TD Organizes one contiguous chunk of memory in
equal-size blocks and tracks allocation status at the cost of one bit per
block.))

$(TR $(TDC2 FallbackAllocator, fallback_allocator) $(TD Allocator that combines two other allocators
 - primary and fallback. Allocation requests are first tried with primary, and
 upon failure are passed to the fallback. Useful for small and fast allocators
 fronting general-purpose ones.))

$(TR $(TDC2 FreeList, free_list) $(TD Allocator that implements a $(HTTP
wikipedia.org/wiki/Free_list, free list) on top of any other allocator. The
preferred size, tolerance, and maximum elements are configurable at compile- and
run time.))

$(TR $(TDC2 SharedFreeList, free_list) $(TD Same features as $(D FreeList), but packaged as
a $(D shared) structure that is accessible to several threads.))

$(TR $(TDC2 FreeTree, free_tree) $(TD Allocator similar to $(D FreeList) that uses a
binary search tree to adaptively store not one, but many free lists.))

$(TR $(TDC2 Region, region) $(TD Region allocator organizes a chunk of memory as a
simple bump-the-pointer allocator.))

$(TR $(TDC2 InSituRegion, region) $(TD Region holding its own allocation, most often on
the stack. Has statically-determined size.))

$(TR $(TDC2 SbrkRegion, region) $(TD Region using $(D $(LINK2 https://en.wikipedia.org/wiki/Sbrk,
sbrk)) for allocating memory.))

$(TR $(TDC3 MmapAllocator, mmap_allocator) $(TD Allocator using
            $(D $(LINK2 https://en.wikipedia.org/wiki/Mmap, mmap)) directly.))

$(TR $(TDC2 StatsCollector, stats_collector) $(TD Collect statistics about any other
allocator.))

$(TR $(TDC2 Quantizer, quantizer) $(TD Allocates in coarse-grained quantas, thus
improving performance of reallocations by often reallocating in place. The drawback is higher memory consumption because of allocated and unused memory.))

$(TR $(TDC2 AllocatorList, allocator_list) $(TD Given an allocator factory, lazily creates as
many allocators as needed to satisfy allocation requests. The allocators are
stored in a linked list. Requests for allocation are satisfied by searching the
list in a linear manner.))

$(TR $(TDC2 Segregator, segregator) $(TD Segregates allocation requests by size
and dispatches them to distinct allocators.))

$(TR $(TDC2 Bucketizer, bucketizer) $(TD Divides allocation sizes in discrete buckets and
uses an array of allocators, one per bucket, to satisfy requests.))

$(COMMENT $(TR $(TDC2 InternalPointersTree) $(TD Adds support for resolving internal
pointers on top of another allocator.)))
)

Macros:
MYREF2 = $(REF_SHORT $1, std,experimental,allocator,building_blocks,$2)
MYREF3 = $(REF_SHORT $1, std,experimental,allocator,$2)
TDC = $(TDNW $(D $1)$+)
TDC2 = $(TDNW $(D $(MYREF2 $1,$+))$(BR)$(SMALL
$(D std.experimental.allocator.building_blocks.$2)))
TDC3 = $(TDNW $(D $(MYREF3 $1,$+))$(BR)$(SMALL
$(D std.experimental.allocator.$2)))
RES = $(I result)
POST = $(BR)$(SMALL $(I Post:) $(BLUE $(D $0)))
*/

module std.experimental.allocator.building_blocks;

public import
    std.experimental.allocator.building_blocks.affix_allocator,
    std.experimental.allocator.building_blocks.allocator_list,
    std.experimental.allocator.building_blocks.bucketizer,
    std.experimental.allocator.building_blocks.fallback_allocator,
    std.experimental.allocator.building_blocks.free_list,
    std.experimental.allocator.building_blocks.free_tree,
    std.experimental.allocator.gc_allocator,
    std.experimental.allocator.building_blocks.bitmapped_block,
    std.experimental.allocator.building_blocks.kernighan_ritchie,
    std.experimental.allocator.mallocator,
    std.experimental.allocator.mmap_allocator,
    std.experimental.allocator.building_blocks.null_allocator,
    std.experimental.allocator.building_blocks.quantizer,
    std.experimental.allocator.building_blocks.region,
    std.experimental.allocator.building_blocks.segregator,
    std.experimental.allocator.building_blocks.stats_collector;

///
module std.experimental.allocator.building_blocks.bitmapped_block;

import std.experimental.allocator.building_blocks.null_allocator;
import std.experimental.allocator.common;

/**

$(D BitmappedBlock) implements a simple heap consisting of one contiguous area
of memory organized in blocks, each of size $(D theBlockSize). A block is a unit
of allocation. A bitmap serves as bookkeeping data, more precisely one bit per
block indicating whether that block is currently allocated or not.

Passing $(D NullAllocator) as $(D ParentAllocator) (the default) means user code
manages allocation of the memory block from the outside; in that case
$(D BitmappedBlock) must be constructed with a $(D void[]) preallocated block and
has no responsibility regarding the lifetime of its support underlying storage.
If another allocator type is passed, $(D BitmappedBlock) defines a destructor that
uses the parent allocator to release the memory block. That makes the combination of $(D AllocatorList), $(D BitmappedBlock), and a back-end allocator such as $(D MmapAllocator) a simple and scalable solution for memory allocation.

There are advantages to storing bookkeeping data separated from the payload
(as opposed to e.g. using $(D AffixAllocator) to store metadata together with
each allocation). The layout is more compact (overhead is one bit per block),
searching for a free block during allocation enjoys better cache locality, and
deallocation does not touch memory around the payload being deallocated (which
is often cold).

Allocation requests are handled on a first-fit basis. Although linear in
complexity, allocation is in practice fast because of the compact bookkeeping
representation, use of simple and fast bitwise routines, and caching of the
first available block position. A known issue with this general approach is
fragmentation, partially mitigated by coalescing. Since $(D BitmappedBlock) does
not need to maintain the allocated size, freeing memory implicitly coalesces
free blocks together. Also, tuning $(D blockSize) has a considerable impact on
both internal and external fragmentation.

The size of each block can be selected either during compilation or at run
time. Statically-known block sizes are frequent in practice and yield slightly
better performance. To choose a block size statically, pass it as the $(D
blockSize) parameter as in $(D BitmappedBlock!(Allocator, 4096)). To choose a block
size parameter, use $(D BitmappedBlock!(Allocator, chooseAtRuntime)) and pass the
block size to the constructor.

*/
struct BitmappedBlock(size_t theBlockSize, uint theAlignment = platformAlignment,
    ParentAllocator = NullAllocator)
{
    import std.conv : text;
    import std.traits : hasMember;
    import std.typecons : Ternary;
    import std.typecons : tuple, Tuple;

    @system unittest
    {
        import std.algorithm.comparison : max;
        import std.experimental.allocator.mallocator : AlignedMallocator;
        auto m = cast(ubyte[])(AlignedMallocator.instance.alignedAllocate(1024 * 64,
                                max(theAlignment, cast(uint) size_t.sizeof)));
        scope(exit) AlignedMallocator.instance.deallocate(m);
        testAllocator!(() => BitmappedBlock(m));
    }
    static assert(theBlockSize > 0 && theAlignment.isGoodStaticAlignment);
    static assert(theBlockSize == chooseAtRuntime
        || theBlockSize % theAlignment == 0,
        "Block size must be a multiple of the alignment");

    /**
    If $(D blockSize == chooseAtRuntime), $(D BitmappedBlock) offers a read/write
    property $(D blockSize). It must be set before any use of the allocator.
    Otherwise (i.e. $(D theBlockSize) is a legit constant), $(D blockSize) is
    an alias for $(D theBlockSize). Whether constant or variable, must also be
    a multiple of $(D alignment). This constraint is $(D assert)ed statically
    and dynamically.
    */
    static if (theBlockSize != chooseAtRuntime)
    {
        alias blockSize = theBlockSize;
    }
    else
    {
        @property uint blockSize() { return _blockSize; }
        @property void blockSize(uint s)
        {
            assert(!_control && s % alignment == 0);
            _blockSize = s;
        }
        private uint _blockSize;
    }

    static if (is(ParentAllocator == NullAllocator))
    {
        private enum parentAlignment = platformAlignment;
    }
    else
    {
        private alias parentAlignment = ParentAllocator.alignment;
        static assert(parentAlignment >= ulong.alignof);
    }

    /**
    The _alignment offered is user-configurable statically through parameter
    $(D theAlignment), defaulted to $(D platformAlignment).
    */
    alias alignment = theAlignment;

    // state {
    /**
    The _parent allocator. Depending on whether $(D ParentAllocator) holds state
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
    }
    private uint _blocks;
    private BitVector _control;
    private void[] _payload;
    private size_t _startIdx;
    // }

    private size_t totalAllocation(size_t capacity)
    {
        auto blocks = capacity.divideRoundUp(blockSize);
        auto leadingUlongs = blocks.divideRoundUp(64);
        import std.algorithm.comparison : min;
        immutable initialAlignment = min(parentAlignment,
            1U << trailingZeros(leadingUlongs * 8));
        auto maxSlack = alignment <= initialAlignment
            ? 0
            : alignment - initialAlignment;
        //writeln(maxSlack);
        return leadingUlongs * 8 + maxSlack + blockSize * blocks;
    }

    /**
    Constructs a block allocator given a hunk of memory, or a desired capacity
    in bytes.

    $(UL
    $(LI If $(D ParentAllocator) is $(D NullAllocator), only the constructor
    taking $(D data) is defined and the user is responsible for freeing $(D
    data) if desired.)
    $(LI Otherwise, both constructors are defined. The $(D data)-based
    constructor assumes memory has been allocated with the parent allocator.
    The $(D capacity)-based constructor uses $(D ParentAllocator) to allocate
    an appropriate contiguous hunk of memory. Regardless of the constructor
    used, the destructor releases the memory by using $(D
    ParentAllocator.deallocate).)
    )
    */
    this(ubyte[] data)
    {
        immutable a = data.ptr.effectiveAlignment;
        assert(a >= size_t.alignof || !data.ptr,
            "Data must be aligned properly");

        immutable ulong totalBits = data.length * 8;
        immutable ulong bitsPerBlock = blockSize * 8 + 1;
        // Get a first estimate
        import std.conv : to;
        _blocks = to!uint(totalBits / bitsPerBlock);

        // Reality is a bit more complicated, iterate until a good number of
        // blocks found.
        for (; _blocks; --_blocks)
        {
            immutable controlWords = _blocks.divideRoundUp(64);
            auto payload = data[controlWords * 8 .. $].roundStartToMultipleOf(
                alignment);
            if (payload.length < _blocks * blockSize)
            {
                // Overestimated
                continue;
            }
            _control = BitVector((cast(ulong*) data.ptr)[0 .. controlWords]);
            _control[] = 0;
            _payload = payload;
            break;
        }
    }

    /// Ditto
    static if (!is(ParentAllocator == NullAllocator))
    this(size_t capacity)
    {
        size_t toAllocate = totalAllocation(capacity);
        auto data = cast(ubyte[])(parent.allocate(toAllocate));
        this(data);
        assert(_blocks * blockSize >= capacity);
    }

    /**
    If $(D ParentAllocator) is not $(D NullAllocator) and defines $(D
    deallocate), the destructor is defined to deallocate the block held.
    */
    static if (!is(ParentAllocator == NullAllocator)
        && hasMember!(ParentAllocator, "deallocate"))
    ~this()
    {
        auto start = _control.rep.ptr, end = _payload.ptr + _payload.length;
        parent.deallocate(start[0 .. end - start]);
    }

    /*
    Adjusts the memoized _startIdx to the leftmost control word that has at
    least one zero bit. Assumes all control words to the left of $(D
    _control[_startIdx]) are already occupied.
    */
    private void adjustStartIdx()
    {
        while (_startIdx < _control.rep.length
            && _control.rep[_startIdx] == ulong.max)
        {
            ++_startIdx;
        }
    }

    /*
    Returns the blocks corresponding to the control bits starting at word index
    wordIdx and bit index msbIdx (MSB=0) for a total of howManyBlocks.
    */
    private void[] blocksFor(size_t wordIdx, uint msbIdx, size_t howManyBlocks)
    {
        assert(msbIdx <= 63);
        const start = (wordIdx * 64 + msbIdx) * blockSize;
        const end = start + blockSize * howManyBlocks;
        if (end <= _payload.length) return _payload[start .. end];
        // This could happen if we have more control bits than available memory.
        // That's possible because the control bits are rounded up to fit in
        // 64-bit words.
        return null;
    }

    /**
    Returns the actual bytes allocated when $(D n) bytes are requested, i.e.
    $(D n.roundUpToMultipleOf(blockSize)).
    */
    size_t goodAllocSize(size_t n)
    {
        return n.roundUpToMultipleOf(blockSize);
    }

    /**
    Allocates $(D s) bytes of memory and returns it, or $(D null) if memory
    could not be allocated.

    The following information might be of help with choosing the appropriate
    block size. Actual allocation occurs in sizes multiple of the block size.
    Allocating one block is the fastest because only one 0 bit needs to be
    found in the metadata. Allocating 2 through 64 blocks is the next cheapest
    because it affects a maximum of two $(D ulong)s in the metadata.
    Allocations greater than 64 blocks require a multiword search through the
    metadata.
    */
    @trusted void[] allocate(const size_t s)
    {
        const blocks = s.divideRoundUp(blockSize);
        void[] result = void;

    switcharoo:
        switch (blocks)
        {
        case 1:
            // inline code here for speed
            // find the next available block
            foreach (i; _startIdx .. _control.rep.length)
            {
                const w = _control.rep[i];
                if (w == ulong.max) continue;
                uint j = leadingOnes(w);
                assert(j < 64);
                assert((_control.rep[i] & ((1UL << 63) >> j)) == 0);
                _control.rep[i] |= (1UL << 63) >> j;
                if (i == _startIdx)
                {
                    adjustStartIdx();
                }
                result = blocksFor(i, j, 1);
                break switcharoo;
            }
            goto case 0; // fall through
        case 0:
            return null;
        case 2: .. case 64:
            result = smallAlloc(cast(uint) blocks);
            break;
        default:
            result = hugeAlloc(blocks);
            break;
        }
        return result.ptr ? result.ptr[0 .. s] : null;
    }

    /**
    Allocates a block with specified alignment $(D a). The alignment must be a
    power of 2. If $(D a <= alignment), function forwards to $(D allocate).
    Otherwise, it attempts to overallocate and then adjust the result for
    proper alignment. In the worst case the slack memory is around two blocks.
    */
    void[] alignedAllocate(size_t n, uint a)
    {
        import std.math : isPowerOf2;
        assert(a.isPowerOf2);
        if (a <= alignment) return allocate(n);

        // Overallocate to make sure we can get an aligned block
        auto b = allocate((n + a - alignment).roundUpToMultipleOf(blockSize));
        if (!b.ptr) return null;
        auto result = b.roundStartToMultipleOf(a);
        assert(result.length >= n);
        result = result.ptr[0 .. n]; // final result

        // Free any blocks that might be slack at the beginning
        auto slackHeadingBlocks = (result.ptr - b.ptr) / blockSize;
        if (slackHeadingBlocks)
        {
            deallocate(b[0 .. slackHeadingBlocks * blockSize]);
        }

        // Free any blocks that might be slack at the end
        auto slackTrailingBlocks = ((b.ptr + b.length)
            - (result.ptr + result.length)) / blockSize;
        if (slackTrailingBlocks)
        {
            deallocate(b[$ - slackTrailingBlocks * blockSize .. $]);
        }

        return result;
    }

    /**
    If the $(D BitmappedBlock) object is empty (has no active allocation), allocates
    all memory within and returns a slice to it. Otherwise, returns $(D null)
    (i.e. no attempt is made to allocate the largest available block).
    */
    void[] allocateAll()
    {
        if (empty != Ternary.yes) return null;
        _control[] = 1;
        return _payload;
    }

    /**
    Returns `Ternary.yes` if `b` belongs to the `BitmappedBlock` object,
    `Ternary.no` otherwise. Never returns `Ternary.unkown`. (This
    method is somewhat tolerant in that accepts an interior slice.)
    */
    Ternary owns(void[] b) const
    {
        //if (!b.ptr) return Ternary.no;
        assert(b.ptr !is null || b.length == 0, "Corrupt block.");
        return Ternary(b.ptr >= _payload.ptr
            && b.ptr + b.length <= _payload.ptr + _payload.length);
    }

    /*
    Tries to allocate "blocks" blocks at the exact position indicated by the
    position wordIdx/msbIdx (msbIdx counts from MSB, i.e. MSB has index 0). If
    it succeeds, fills "result" with the result and returns tuple(size_t.max,
    0). Otherwise, returns a tuple with the next position to search.
    */
    private Tuple!(size_t, uint) allocateAt(size_t wordIdx, uint msbIdx,
            size_t blocks, ref void[] result)
    {
        assert(blocks > 0);
        assert(wordIdx < _control.rep.length);
        assert(msbIdx <= 63);
        if (msbIdx + blocks <= 64)
        {
            // Allocation should fit this control word
            if (setBitsIfZero(_control.rep[wordIdx],
                    cast(uint) (64 - msbIdx - blocks), 63 - msbIdx))
            {
                // Success
                result = blocksFor(wordIdx, msbIdx, blocks);
                return tuple(size_t.max, 0u);
            }
            // Can't allocate, make a suggestion
            return msbIdx + blocks == 64
                ? tuple(wordIdx + 1, 0u)
                : tuple(wordIdx, cast(uint) (msbIdx + blocks));
        }
        // Allocation spans two control words or more
        immutable mask = ulong.max >> msbIdx;
        if (_control.rep[wordIdx] & mask)
        {
            // We can't allocate the rest of this control word,
            // return a suggestion.
            return tuple(wordIdx + 1, 0u);
        }
        // We can allocate the rest of this control word, but we first need to
        // make sure we can allocate the tail.
        if (wordIdx + 1 == _control.rep.length)
        {
            // No more memory
            return tuple(_control.rep.length, 0u);
        }
        auto hint = allocateAt(wordIdx + 1, 0, blocks - 64 + msbIdx, result);
        if (hint[0] == size_t.max)
        {
            // We did it!
            _control.rep[wordIdx] |= mask;
            result = blocksFor(wordIdx, msbIdx, blocks);
            return tuple(size_t.max, 0u);
        }
        // Failed, return a suggestion that skips this whole run.
        return hint;
    }

    /* Allocates as many blocks as possible at the end of the blocks indicated
    by wordIdx. Returns the number of blocks allocated. */
    private uint allocateAtTail(size_t wordIdx)
    {
        assert(wordIdx < _control.rep.length);
        const available = trailingZeros(_control.rep[wordIdx]);
        _control.rep[wordIdx] |= ulong.max >> available;
        return available;
    }

    private void[] smallAlloc(uint blocks)
    {
        assert(blocks >= 2 && blocks <= 64, text(blocks));
        foreach (i; _startIdx .. _control.rep.length)
        {
            // Test within the current 64-bit word
            const v = _control.rep[i];
            if (v == ulong.max) continue;
            auto j = findContigOnes(~v, blocks);
            if (j < 64)
            {
                // yay, found stuff
                setBits(_control.rep[i], 64 - j - blocks, 63 - j);
                return blocksFor(i, j, blocks);
            }
            // Next, try allocations that cross a word
            auto available = trailingZeros(v);
            if (available == 0) continue;
            if (i + 1 >= _control.rep.length) break;
            assert(available < blocks); // otherwise we should have found it
            auto needed = blocks - available;
            assert(needed > 0 && needed < 64);
            if (allocateAtFront(i + 1, needed))
            {
                // yay, found a block crossing two words
                _control.rep[i] |= (1UL << available) - 1;
                return blocksFor(i, 64 - available, blocks);
            }
        }
        return null;
    }

    private void[] hugeAlloc(size_t blocks)
    {
        assert(blocks > 64);
        if (_startIdx == _control._rep.length)
        {
            assert(_control.allAre1);
            return null;
        }
        auto i = _control.findZeros(blocks, _startIdx * 64);
        if (i == i.max) return null;
        // Allocate those bits
        _control[i .. i + blocks] = 1;
        return _payload[cast(size_t) (i * blockSize)
            .. cast(size_t) ((i + blocks) * blockSize)];
    }

    // Rounds sizeInBytes to a multiple of blockSize.
    private size_t bytes2blocks(size_t sizeInBytes)
    {
        return (sizeInBytes + blockSize - 1) / blockSize;
    }

    /* Allocates given blocks at the beginning blocks indicated by wordIdx.
    Returns true if allocation was possible, false otherwise. */
    private bool allocateAtFront(size_t wordIdx, uint blocks)
    {
        assert(wordIdx < _control.rep.length && blocks >= 1 && blocks <= 64);
        const mask = (1UL << (64 - blocks)) - 1;
        if (_control.rep[wordIdx] > mask) return false;
        // yay, works
        _control.rep[wordIdx] |= ~mask;
        return true;
    }

    /**
    Expands an allocated block in place.
    */
    @trusted bool expand(ref void[] b, immutable size_t delta)
    {
        // Dispose with trivial corner cases
        if (delta == 0) return true;
        if (b is null) return false;

        /* To simplify matters, refuse to expand buffers that don't start at a block start (this may be the case for blocks allocated with alignedAllocate).
        */
        if ((b.ptr - _payload.ptr) % blockSize) return false;

        const blocksOld = bytes2blocks(b.length);
        const blocksNew = bytes2blocks(b.length + delta);
        assert(blocksOld <= blocksNew);

        // Possibly we have enough slack at the end of the block!
        if (blocksOld == blocksNew)
        {
            b = b.ptr[0 .. b.length + delta];
            return true;
        }

        assert((b.ptr - _payload.ptr) % blockSize == 0);
        const blockIdx = (b.ptr - _payload.ptr) / blockSize;
        const blockIdxAfter = blockIdx + blocksOld;

        // Try the maximum
        const wordIdx = blockIdxAfter / 64,
            msbIdx = cast(uint) (blockIdxAfter % 64);
        void[] p;
        auto hint = allocateAt(wordIdx, msbIdx,  blocksNew - blocksOld, p);
        if (hint[0] != size_t.max)
        {
            return false;
        }
        // Expansion successful
        assert(p.ptr == b.ptr + blocksOld * blockSize,
            text(p.ptr, " != ", b.ptr + blocksOld * blockSize));
        b = b.ptr[0 .. b.length + delta];
        return true;
    }

    /**
    Reallocates a previously-allocated block. Contractions occur in place.
    */
    @system bool reallocate(ref void[] b, size_t newSize)
    {
        if (!b.ptr)
        {
            b = allocate(newSize);
            return b.length == newSize;
        }
        if (newSize == 0)
        {
            deallocate(b);
            b = null;
            return true;
        }
        if (newSize < b.length)
        {
            // Shrink. Will shrink in place by deallocating the trailing part.
            auto newCapacity = bytes2blocks(newSize) * blockSize;
            deallocate(b[newCapacity .. $]);
            b = b[0 .. newSize];
            return true;
        }
        // Go the slow route
        return .reallocate(this, b, newSize);
    }

    /**
    Reallocates a block previously allocated with $(D alignedAllocate). Contractions do not occur in place.
    */
    @system bool alignedReallocate(ref void[] b, size_t newSize, uint a)
    {
        if (newSize == 0)
        {
            deallocate(b);
            b = null;
            return true;
        }
        // Go the slow route
        return .alignedReallocate(this, b, newSize, a);
    }

    /**
    Deallocates a block previously allocated with this allocator.
    */
    bool deallocate(void[] b)
    {
        if (b is null) return true;

        // Locate position
        immutable pos = b.ptr - _payload.ptr;
        immutable blockIdx = pos / blockSize;

        // Adjust pointer, might be inside a block due to alignedAllocate
        auto begin = _payload.ptr + blockIdx * blockSize,
            end = b.ptr + b.length;
        b = begin[0 .. end - begin];
        // Round up size to multiple of block size
        auto blocks = b.length.divideRoundUp(blockSize);

        // Get into details
        auto wordIdx = blockIdx / 64, msbIdx = cast(uint) (blockIdx % 64);
        if (_startIdx > wordIdx) _startIdx = wordIdx;

        // Three stages: heading bits, full words, leftover bits
        if (msbIdx)
        {
            if (blocks + msbIdx <= 64)
            {
                resetBits(_control.rep[wordIdx],
                    cast(uint) (64 - msbIdx - blocks),
                    63 - msbIdx);
                return true;
            }
            else
            {
                _control.rep[wordIdx] &= ulong.max << 64 - msbIdx;
                blocks -= 64 - msbIdx;
                ++wordIdx;
                msbIdx = 0;
            }
        }

        // Stage 2: reset one word at a time
        for (; blocks >= 64; blocks -= 64)
        {
            _control.rep[wordIdx++] = 0;
        }

        // Stage 3: deal with leftover bits, if any
        assert(wordIdx <= _control.rep.length);
        if (blocks)
        {
            _control.rep[wordIdx] &= ulong.max >> blocks;
        }
        return true;
    }

    /**
    Forcibly deallocates all memory allocated by this allocator, making it
    available for further allocations. Does not return memory to $(D
    ParentAllocator).
    */
    bool deallocateAll()
    {
        _control[] = 0;
        _startIdx = 0;
        return true;
    }

    /**
    Returns `Ternary.yes` if no memory is currently allocated with this
    allocator, otherwise `Ternary.no`. This method never returns
    `Ternary.unknown`.
    */
    Ternary empty()
    {
        return Ternary(_control.allAre0());
    }

    void dump()
    {
        import std.stdio : writefln, writeln;
        writefln("%s @ %s {", typeid(this), cast(void*) _control._rep.ptr);
        scope(exit) writeln("}");
        assert(_payload.length == blockSize * _blocks);
        assert(_control.length >= _blocks);
        writefln("  _startIdx=%s; blockSize=%s; blocks=%s",
            _startIdx, blockSize, _blocks);
        if (!_control.length) return;
        uint blockCount = 1;
        bool inAllocatedStore = _control[0];
        void* start = _payload.ptr;
        for (size_t i = 1;; ++i)
        {
            if (i >= _blocks || _control[i] != inAllocatedStore)
            {
                writefln("  %s block at 0x%s, length: %s (%s*%s)",
                    inAllocatedStore ? "Busy" : "Free",
                    cast(void*) start,
                    blockCount * blockSize,
                    blockCount, blockSize);
                if (i >= _blocks) break;
                assert(i < _control.length);
                inAllocatedStore = _control[i];
                start = _payload.ptr + blockCount * blockSize;
                blockCount = 1;
            }
            else
            {
                ++blockCount;
            }
        }
    }
}

///
@system unittest
{
    // Create a block allocator on top of a 10KB stack region.
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    import std.traits : hasMember;
    InSituRegion!(10_240, 64) r;
    auto a = BitmappedBlock!(64, 64)(cast(ubyte[])(r.allocateAll()));
    static assert(hasMember!(InSituRegion!(10_240, 64), "allocateAll"));
    const b = a.allocate(100);
    assert(b.length == 100);
}

@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    testAllocator!(() => BitmappedBlock!(64, 8, GCAllocator)(1024 * 64));
}

@system unittest
{
    static void testAllocateAll(size_t bs)(uint blocks, uint blocksAtATime)
    {
        import std.algorithm.comparison : min;
        assert(bs);
        import std.experimental.allocator.gc_allocator : GCAllocator;
        auto a = BitmappedBlock!(bs, min(bs, platformAlignment))(
            cast(ubyte[])(GCAllocator.instance.allocate((blocks * bs * 8 +
                        blocks) / 8))
        );
        import std.conv : text;
        assert(blocks >= a._blocks, text(blocks, " < ", a._blocks));
        blocks = a._blocks;

        // test allocation of 0 bytes
        auto x = a.allocate(0);
        assert(x is null);
        // test allocation of 1 byte
        x = a.allocate(1);
        assert(x.length == 1 || blocks == 0,
            text(x.ptr, " ", x.length, " ", a));
        a.deallocateAll();

        bool twice = true;

    begin:
        foreach (i; 0 .. blocks / blocksAtATime)
        {
            auto b = a.allocate(bs * blocksAtATime);
            assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
        }
        assert(a.allocate(bs * blocksAtATime) is null);
        assert(a.allocate(1) is null);

        // Now deallocate all and do it again!
        a.deallocateAll();

        // Test deallocation

        auto v = new void[][blocks / blocksAtATime];
        foreach (i; 0 .. blocks / blocksAtATime)
        {
            auto b = a.allocate(bs * blocksAtATime);
            assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
            v[i] = b;
        }
        assert(a.allocate(bs * blocksAtATime) is null);
        assert(a.allocate(1) is null);

        foreach (i; 0 .. blocks / blocksAtATime)
        {
            a.deallocate(v[i]);
        }

        foreach (i; 0 .. blocks / blocksAtATime)
        {
            auto b = a.allocate(bs * blocksAtATime);
            assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
            v[i] = b;
        }

        foreach (i; 0 .. v.length)
        {
            a.deallocate(v[i]);
        }

        if (twice)
        {
            twice = false;
            goto begin;
        }

        a.deallocateAll;

        // test expansion
        if (blocks >= blocksAtATime)
        {
            foreach (i; 0 .. blocks / blocksAtATime - 1)
            {
                auto b = a.allocate(bs * blocksAtATime);
                assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
                (cast(ubyte[]) b)[] = 0xff;
                a.expand(b, blocksAtATime * bs)
                    || assert(0, text(i));
                (cast(ubyte[]) b)[] = 0xfe;
                assert(b.length == bs * blocksAtATime * 2, text(i, ": ", b.length));
                a.reallocate(b, blocksAtATime * bs) || assert(0);
                assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
            }
        }
    }

    testAllocateAll!(1)(0, 1);
    testAllocateAll!(1)(8, 1);
    testAllocateAll!(4096)(128, 1);

    testAllocateAll!(1)(0, 2);
    testAllocateAll!(1)(128, 2);
    testAllocateAll!(4096)(128, 2);

    testAllocateAll!(1)(0, 4);
    testAllocateAll!(1)(128, 4);
    testAllocateAll!(4096)(128, 4);

    testAllocateAll!(1)(0, 3);
    testAllocateAll!(1)(24, 3);
    testAllocateAll!(3008)(100, 1);
    testAllocateAll!(3008)(100, 3);

    testAllocateAll!(1)(0, 128);
    testAllocateAll!(1)(128 * 1, 128);
    testAllocateAll!(128 * 20)(13 * 128, 128);
}

// Test totalAllocation
@safe unittest
{
    BitmappedBlock!(8, 8, NullAllocator) h1;
    assert(h1.totalAllocation(1) >= 8);
    assert(h1.totalAllocation(64) >= 64);
    assert(h1.totalAllocation(8 * 64) >= 8 * 64);
    assert(h1.totalAllocation(8 * 63) >= 8 * 63);
    assert(h1.totalAllocation(8 * 64 + 1) >= 8 * 65);

    BitmappedBlock!(64, 8, NullAllocator) h2;
    assert(h2.totalAllocation(1) >= 64);
    assert(h2.totalAllocation(64 * 64) >= 64 * 64);

    BitmappedBlock!(4096, 4096, NullAllocator) h3;
    assert(h3.totalAllocation(1) >= 4096);
    assert(h3.totalAllocation(64 * 4096) >= 64 * 4096);
    assert(h3.totalAllocation(64 * 4096 + 1) >= 65 * 4096);
}

// BitmappedBlockWithInternalPointers
/**

A $(D BitmappedBlock) with additional structure for supporting $(D
resolveInternalPointer). To that end, $(D BitmappedBlockWithInternalPointers) adds a
bitmap (one bit per block) that marks object starts. The bitmap itself has
variable size and is allocated together with regular allocations.

The time complexity of $(D resolveInternalPointer) is $(BIGOH k), where $(D k)
is the size of the object within which the internal pointer is looked up.

*/
struct BitmappedBlockWithInternalPointers(
    size_t theBlockSize, uint theAlignment = platformAlignment,
    ParentAllocator = NullAllocator)
{
    import std.conv : text;
    import std.typecons : Ternary;
    @system unittest
    {
        import std.experimental.allocator.mallocator : AlignedMallocator;
        auto m = cast(ubyte[])(AlignedMallocator.instance.alignedAllocate(1024 * 64,
            theAlignment));
        scope(exit) AlignedMallocator.instance.deallocate(m);
        testAllocator!(() => BitmappedBlockWithInternalPointers(m));
    }

    // state {
    private BitmappedBlock!(theBlockSize, theAlignment, NullAllocator) _heap;
    private BitVector _allocStart;
    // }

    /**
    Constructors accepting desired capacity or a preallocated buffer, similar
    in semantics to those of $(D BitmappedBlock).
    */
    this(ubyte[] data)
    {
        _heap = BitmappedBlock!(theBlockSize, theAlignment, ParentAllocator)(data);
    }

    /// Ditto
    static if (!is(ParentAllocator == NullAllocator))
    this(size_t capacity)
    {
        // Add room for the _allocStart vector
        _heap = BitmappedBlock!(theBlockSize, theAlignment, ParentAllocator)
            (capacity + capacity.divideRoundUp(64));
    }

    // Makes sure there's enough room for _allocStart
    private bool ensureRoomForAllocStart(size_t len)
    {
        if (_allocStart.length >= len) return true;
        // Must ensure there's room
        immutable oldLength = _allocStart.rep.length;
        immutable bits = len.roundUpToMultipleOf(64);
        void[] b = _allocStart.rep;
        if (!_heap.reallocate(b, bits / 8)) return false;
        assert(b.length * 8 == bits, text(b.length * 8, " != ", bits));
        _allocStart = BitVector(cast(ulong[]) b);
        assert(_allocStart.rep.length * 64 == bits);
        _allocStart.rep[oldLength .. $] = ulong.max;
        return true;
    }

    /**
    Allocator primitives.
    */
    alias alignment = theAlignment;

    /// Ditto
    size_t goodAllocSize(size_t n)
    {
        return n.roundUpToMultipleOf(_heap.blockSize);
    }

    /// Ditto
    void[] allocate(size_t bytes)
    {
        auto r = _heap.allocate(bytes);
        if (!r.ptr) return r;
        immutable block = (r.ptr - _heap._payload.ptr) / _heap.blockSize;
        immutable blocks =
            (r.length + _heap.blockSize - 1) / _heap.blockSize;
        if (!ensureRoomForAllocStart(block + blocks))
        {
            // Failed, free r and bailout
            _heap.deallocate(r);
            return null;
        }
        assert(block < _allocStart.length);
        assert(block + blocks <= _allocStart.length);
        // Mark the _allocStart bits
        assert(blocks > 0);
        _allocStart[block] = 1;
        _allocStart[block + 1 .. block + blocks] = 0;
        assert(block + blocks == _allocStart.length
            || _allocStart[block + blocks] == 1);
        return r;
    }

    /// Ditto
    void[] allocateAll()
    {
        auto r = _heap.allocateAll();
        if (!r.ptr) return r;
        // Carve space at the end for _allocStart
        auto p = alignDownTo(r.ptr + r.length - 8, ulong.alignof);
        r = r[0 .. p - r.ptr];
        // Initialize _allocStart
        _allocStart = BitVector(cast(ulong[]) p[0 .. 8]);
        _allocStart[] = 0;
        immutable block = (r.ptr - _heap._payload.ptr) / _heap.blockSize;
        assert(block < _allocStart.length);
        _allocStart[block] = 1;
        return r;
    }

    /// Ditto
    bool expand(ref void[] b, size_t bytes)
    {
        if (!bytes) return true;
        if (b is null) return false;
        immutable oldBlocks =
            (b.length + _heap.blockSize - 1) / _heap.blockSize;
        assert(oldBlocks);
        immutable newBlocks =
            (b.length + bytes + _heap.blockSize - 1) / _heap.blockSize;
        assert(newBlocks >= oldBlocks);
        immutable block = (b.ptr - _heap._payload.ptr) / _heap.blockSize;
        assert(_allocStart[block]);
        if (!ensureRoomForAllocStart(block + newBlocks)
                || !_heap.expand(b, bytes))
        {
            return false;
        }
        // Zero only the expanded bits
        _allocStart[block + oldBlocks .. block + newBlocks] = 0;
        assert(_allocStart[block]);
        return true;
    }

    /// Ditto
    bool deallocate(void[] b)
    {
        // No need to touch _allocStart here - except for the first bit, it's
        // meaningless in freed memory. The first bit is already 1.
        return _heap.deallocate(b);
        // TODO: one smart thing to do is reduce memory occupied by
        // _allocStart if we're freeing the rightmost block.
    }

    /// Ditto
    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        if (p < _heap._payload.ptr
            || p >= _heap._payload.ptr + _heap._payload.length)
        {
            return Ternary.no;
        }
        // Find block start
        auto block = (p - _heap._payload.ptr) / _heap.blockSize;
        if (block >= _allocStart.length) return Ternary.no;
        // Within an allocation, must find the 1 just to the left of it
        auto i = _allocStart.find1Backward(block);
        if (i == i.max) return Ternary.no;
        auto j = _allocStart.find1(i + 1);
        result = _heap._payload.ptr[cast(size_t) (_heap.blockSize * i)
                                    .. cast(size_t) (_heap.blockSize * j)];
        return Ternary.yes;
    }

    /// Ditto
    Ternary empty()
    {
        return _heap.empty;
    }

    // Currently unused
    private void markAllAsUnused()
    {
        // Mark all deallocated memory with 1 so we minimize damage created by
        // false pointers. TODO: improve speed.
        foreach (i, ref e; _allocStart.rep)
        {
            // Set to 1 all bits in _allocStart[i] that were 0 in control, and
            // leave the others unchanged.
            // (0, 0) => 1; (0, 1) => 0; (1, 0) => 1; (1, 1) => 1
            e |= ~_heap._control.rep[i];
        }
        // Now zero all control bits
        _heap._control[] = 0;
        // EXCEPT for the _allocStart block itself
        markAsUsed(_allocStart.rep);
    }

    // Currently unused
    private bool markAsUsed(void[] b)
    {
        // Locate position
        immutable pos = b.ptr - _heap._payload.ptr;
        assert(pos % _heap.blockSize == 0);
        auto blockIdx = pos / _heap.blockSize;
        if (_heap._control[blockIdx]) return false;
        // Round up size to multiple of block size
        auto blocks = b.length.divideRoundUp(_heap.blockSize);
        _heap._control[blockIdx .. blockIdx + blocks] = 1;
        return true;
    }

    // Currently unused
    private void doneMarking()
    {
        // Nothing to do, what's free stays free.
    }
}

@system unittest
{
    import std.typecons : Ternary;

    auto h = BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024]);
    auto b = h.allocate(123);
    assert(b.length == 123);

    void[] p;
    Ternary r = h.resolveInternalPointer(b.ptr + 17, p);
    assert(p.ptr is b.ptr);
    assert(p.length >= b.length);
    b = h.allocate(4096);

    h.resolveInternalPointer(b.ptr, p);
    assert(p is b);

    h.resolveInternalPointer(b.ptr + 11, p);
    assert(p is b);

    void[] unchanged = p;
    h.resolveInternalPointer(b.ptr - 40_970, p);
    assert(p is unchanged);

    assert(h.expand(b, 1));
    assert(b.length == 4097);
    h.resolveInternalPointer(b.ptr + 4096, p);
    assert(p.ptr is b.ptr);
}

/**
Returns the number of most significant ones before a zero can be found in $(D
x). If $(D x) contains no zeros (i.e. is equal to $(D ulong.max)), returns 64.
*/
private uint leadingOnes(ulong x)
{
    uint result = 0;
    while (cast(long) x < 0)
    {
        ++result;
        x <<= 1;
    }
    return result;
}

@system unittest
{
    assert(leadingOnes(0) == 0);
    assert(leadingOnes(~0UL) == 64);
    assert(leadingOnes(0xF000_0000_0000_0000) == 4);
    assert(leadingOnes(0xE400_0000_0000_0000) == 3);
    assert(leadingOnes(0xC700_0200_0000_0000) == 2);
    assert(leadingOnes(0x8000_0030_0000_0000) == 1);
    assert(leadingOnes(0x2000_0000_0000_0000) == 0);
}

/**
Finds a run of contiguous ones in $(D x) of length at least $(D n).
*/
private uint findContigOnes(ulong x, uint n)
{
    while (n > 1)
    {
        immutable s = n >> 1;
        x &= x << s;
        n -= s;
    }
    return leadingOnes(~x);
}

@system unittest
{
    assert(findContigOnes(0x0000_0000_0000_0300, 2) == 54);

    assert(findContigOnes(~0UL, 1) == 0);
    assert(findContigOnes(~0UL, 2) == 0);
    assert(findContigOnes(~0UL, 32) == 0);
    assert(findContigOnes(~0UL, 64) == 0);
    assert(findContigOnes(0UL, 1) == 64);

    assert(findContigOnes(0x4000_0000_0000_0000, 1) == 1);
    assert(findContigOnes(0x0000_0F00_0000_0000, 4) == 20);
}

/*
Unconditionally sets the bits from lsb through msb in w to zero.
*/
private void setBits(ref ulong w, uint lsb, uint msb)
{
    assert(lsb <= msb && msb < 64);
    const mask = (ulong.max << lsb) & (ulong.max >> (63 - msb));
    w |= mask;
}

@system unittest
{
    ulong w;
    w = 0; setBits(w, 0, 63); assert(w == ulong.max);
    w = 0; setBits(w, 1, 63); assert(w == ulong.max - 1);
    w = 6; setBits(w, 0, 1); assert(w == 7);
    w = 6; setBits(w, 3, 3); assert(w == 14);
}

/* Are bits from lsb through msb in w zero? If so, make then 1
and return the resulting w. Otherwise, just return 0.
*/
private bool setBitsIfZero(ref ulong w, uint lsb, uint msb)
{
    assert(lsb <= msb && msb < 64);
    const mask = (ulong.max << lsb) & (ulong.max >> (63 - msb));
    if (w & mask) return false;
    w |= mask;
    return true;
}

// Assigns bits in w from lsb through msb to zero.
private void resetBits(ref ulong w, uint lsb, uint msb)
{
    assert(lsb <= msb && msb < 64);
    const mask = (ulong.max << lsb) & (ulong.max >> (63 - msb));
    w &= ~mask;
}

/*
Bit disposition is MSB=0 (leftmost, big endian).
*/
private struct BitVector
{
    ulong[] _rep;

    auto rep() { return _rep; }

    this(ulong[] data) { _rep = data; }

    void opSliceAssign(bool b) { _rep[] = b ? ulong.max : 0; }

    void opSliceAssign(bool b, ulong x, ulong y)
    {
        assert(x <= y && y <= _rep.length * 64);
        if (x == y) return;
        --y;
        assert(x / 64 <= size_t.max);
        immutable i1 = cast(size_t) (x / 64);
        immutable uint b1 = 63 - x % 64;
        assert(y / 64 <= size_t.max);
        immutable i2 = cast(size_t) (y / 64);
        immutable uint b2 = 63 - y % 64;
        assert(i1 <= i2 && i2 < _rep.length);
        if (i1 == i2)
        {
            // Inside the same word
            assert(b1 >= b2);
            if (b) setBits(_rep[i1], b2, b1);
            else resetBits(_rep[i1], b2, b1);
        }
        else
        {
            // Spans multiple words
            assert(i1 < i2);
            if (b) setBits(_rep[i1], 0, b1);
            else resetBits(_rep[i1], 0, b1);
            _rep[i1 + 1 .. i2] = b;
            if (b) setBits(_rep[i2], b2, 63);
            else resetBits(_rep[i2], b2, 63);
        }
    }

    bool opIndex(ulong x)
    {
        assert(x < length);
        return (_rep[cast(size_t) (x / 64)]
            & (0x8000_0000_0000_0000UL >> (x % 64))) != 0;
    }

    void opIndexAssign(bool b, ulong x)
    {
        assert(x / 64 <= size_t.max);
        immutable i = cast(size_t) (x / 64);
        immutable j = 0x8000_0000_0000_0000UL >> (x % 64);
        if (b) _rep[i] |= j;
        else _rep[i] &= ~j;
    }

    ulong length() const
    {
        return _rep.length * 64;
    }

    /* Returns the index of the first 1 to the right of i (including i itself),
    or length if not found.
    */
    ulong find1(ulong i)
    {
        assert(i < length);
        assert(i / 64 <= size_t.max);
        auto w = cast(size_t) (i / 64);
        immutable b = i % 64; // 0 through 63, 0 when i == 0
        immutable mask = ulong.max >> b;
        if (auto current = _rep[w] & mask)
        {
            // Great, found
            return w * 64 + leadingOnes(~current);
        }
        // The current word doesn't have the solution, find the leftmost 1
        // going to the right.
        for (++w; w < _rep.length; ++w)
        {
            if (auto current = _rep[w])
            {
                return w * 64 + leadingOnes(~current);
            }
        }
        return length;
    }

    /* Returns the index of the first 1 to the left of i (including i itself),
    or ulong.max if not found.
    */
    ulong find1Backward(ulong i)
    {
        assert(i < length);
        auto w = cast(size_t) (i / 64);
        immutable b = 63 - (i % 64); // 0 through 63, 63 when i == 0
        immutable mask = ~((1UL << b) - 1);
        assert(mask != 0);
        // First, let's see if the current word has a bit larger than ours.
        if (auto currentWord = _rep[w] & mask)
        {
            // Great, this word contains the result.
            return w * 64 + 63 - currentWord.trailingZeros;
        }
        // The current word doesn't have the solution, find the rightmost 1
        // going to the left.
        while (w >= 1)
        {
            --w;
            if (auto currentWord = _rep[w])
                return w * 64 + (63 - currentWord.trailingZeros);
        }
        return ulong.max;
    }

    /// Are all bits zero?
    bool allAre0() const
    {
        foreach (w; _rep) if (w) return false;
        return true;
    }

    /// Are all bits one?
    bool allAre1() const
    {
        foreach (w; _rep) if (w != ulong.max) return false;
        return true;
    }

    ulong findZeros(immutable size_t howMany, ulong start)
    {
        assert(start < length);
        assert(howMany > 64);
        auto i = cast(size_t) (start / 64);
        while (_rep[i] & 1)
        {
            // No trailing zeros in this word, try the next one
            if (++i == _rep.length) return ulong.max;
            start = i * 64;
        }
        // Adjust start to have only trailing zeros after it
        auto prefixLength = 64;
        while (_rep[i] & (ulong.max >> (64 - prefixLength)))
        {
            assert(prefixLength > 0);
            --prefixLength;
            ++start;
        }

        assert(howMany > prefixLength);
        auto needed = howMany - prefixLength;
        for (++i; needed >= 64; needed -= 64, ++i)
        {
            if (i >= _rep.length) return ulong.max;
            if (_rep[i] != 0) return findZeros(howMany, i * 64);
        }
        // Leftover < 64 bits
        assert(needed < 64);
        if (!needed) return start;
        if (i >= _rep.length) return ulong.max;
        if (leadingOnes(~_rep[i]) >= needed) return start;
        return findZeros(howMany, i * 64);
    }
}

@system unittest
{
    auto v = BitVector(new ulong[10]);
    assert(v.length == 640);

    v[] = 0;
    v[53] = 1;
    assert(v[52] == 0);
    assert(v[53] == 1);
    assert(v[54] == 0);

    v[] = 0;
    v[53 .. 55] = 1;
    assert(v[52] == 0);
    assert(v[53] == 1);
    assert(v[54] == 1);
    assert(v[55] == 0);

    v[] = 0;
    v[2 .. 65] = 1;
    assert(v.rep[0] == 0x3FFF_FFFF_FFFF_FFFF);
    assert(v.rep[1] == 0x8000_0000_0000_0000);
    assert(v.rep[2] == 0);

    v[] = 0;
    assert(v.find1Backward(0) == ulong.max);
    assert(v.find1Backward(43) == ulong.max);
    assert(v.find1Backward(83) == ulong.max);

    v[0] = 1;
    assert(v.find1Backward(0) == 0);
    assert(v.find1Backward(43) == 0);
    import std.conv : text;
    assert(v.find1Backward(83) == 0, text(v.find1Backward(83)));

    v[0] = 0;
    v[101] = 1;
    assert(v.find1Backward(0) == ulong.max);
    assert(v.find1Backward(43) == ulong.max);
    assert(v.find1Backward(83) == ulong.max);
    assert(v.find1Backward(100) == ulong.max);
    assert(v.find1Backward(101) == 101);
    assert(v.find1Backward(553) == 101);

    v[0 .. v.length] = 0;
    v[v.length .. v.length] = 0;
    v[0 .. 0] = 0;

    v[] = 0;
    assert(v.find1(0) == v.length);
    v[139] = 1;
    assert(v.find1(0) == 139);
    assert(v.find1(100) == 139);
    assert(v.find1(138) == 139);
    assert(v.find1(139) == 139);
    assert(v.find1(140) == v.length);

    v[] = 0;
    assert(v.findZeros(100, 0) == 0);
    foreach (i; 0 .. 500)
        assert(v.findZeros(100, i) == i, text(v.findZeros(100, i), " != ", i));
    assert(v.findZeros(540, 99) == 99);
    assert(v.findZeros(99, 540) == 540);
    assert(v.findZeros(540, 100) == 100);
    assert(v.findZeros(640, 0) == 0);
    assert(v.findZeros(641, 1) == ulong.max);
    assert(v.findZeros(641, 100) == ulong.max);
}

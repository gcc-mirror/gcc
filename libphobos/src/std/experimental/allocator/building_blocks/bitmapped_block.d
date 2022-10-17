// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/bitmapped_block.d)
*/
module std.experimental.allocator.building_blocks.bitmapped_block;

import std.experimental.allocator.building_blocks.null_allocator;
import std.experimental.allocator.common;
import std.typecons : Flag, Yes, No;


// Common implementation for shared and non-shared versions of the BitmappedBlock
private mixin template BitmappedBlockImpl(bool isShared, bool multiBlock)
{
    import std.conv : text;
    import std.traits : hasMember;
    import std.typecons : Ternary;
    import std.typecons : tuple, Tuple;

    static if (isShared && multiBlock)
    import core.internal.spinlock : SpinLock;

    static assert(theBlockSize > 0 && theAlignment.isGoodStaticAlignment);
    static assert(theBlockSize == chooseAtRuntime ||
        theBlockSize % theAlignment == 0, "Block size must be a multiple of the alignment");

    static if (theBlockSize != chooseAtRuntime)
    {
        alias blockSize = theBlockSize;
    }
    else
    {
        // It is the caller's responsibilty to synchronize this with
        // allocate/deallocate in shared environments
        @property uint blockSize() { return _blockSize; }
        @property void blockSize(uint s)
        {
            static if (multiBlock)
            {
                assert((cast(BitVector) _control).length == 0 && s % alignment == 0);
            }
            else
            {
                assert(_control.length == 0 && s % alignment == 0);
            }
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

    alias alignment = theAlignment;

    static if (stateSize!ParentAllocator)
    {
        ParentAllocator parent;
    }
    else
    {
        alias parent = ParentAllocator.instance;
    }

    private size_t _blocks;
    private void[] _payload;
    private size_t _startIdx;

    // For multiblock, '_control' is a BitVector, otherwise just a regular ulong[]
    static if (multiBlock)
    {
        // Keeps track of first block which has never been used in an allocation.
        // All blocks which are located right to the '_freshBit', should have never been
        // allocated
        private ulong _freshBit;
        private BitVector _control;
    }
    else
    {
        private ulong[] _control;
    }

    static if (multiBlock && isShared)
    {
        SpinLock lock = SpinLock(SpinLock.Contention.brief);
    }

    pure nothrow @safe @nogc
    private size_t totalAllocation(size_t capacity)
    {
        auto blocks = capacity.divideRoundUp(blockSize);
        auto leadingUlongs = blocks.divideRoundUp(64);
        import std.algorithm.comparison : min;
        immutable initialAlignment = min(parentAlignment,
            1U << min(31U, trailingZeros(leadingUlongs * 8)));
        auto maxSlack = alignment <= initialAlignment
            ? 0
            : alignment - initialAlignment;
        return leadingUlongs * 8 + maxSlack + blockSize * blocks;
    }

    this(ubyte[] data)
    {
        immutable a = data.ptr.effectiveAlignment;
        assert(a >= size_t.alignof || !data.ptr,
            "Data must be aligned properly");

        immutable ulong totalBits = data.length * 8;
        immutable ulong bitsPerBlock = blockSize * 8 + 1;
        _blocks = totalBits / bitsPerBlock;

        // Reality is a bit more complicated, iterate until a good number of
        // blocks found.
        size_t localBlocks;
        for (localBlocks = _blocks; localBlocks; --localBlocks)
        {
            immutable controlWords = localBlocks.divideRoundUp(64);
            auto payload = data[controlWords * 8 .. $].roundStartToMultipleOf(
                alignment);
            if (payload.length < localBlocks * blockSize)
            {
                // Overestimated
                continue;
            }

            // Need the casts for shared versions
            static if (multiBlock)
            {
                _control = cast(typeof(_control)) BitVector((cast(ulong*) data.ptr)[0 .. controlWords]);
                (cast(BitVector) _control)[] = 0;
            }
            else
            {
                _control = (cast(typeof(_control.ptr)) data.ptr)[0 .. controlWords];
                _control[] = 0;
            }

            _payload = cast(typeof(_payload)) payload;
            break;
        }

        _blocks = cast(typeof(_blocks)) localBlocks;
    }

    static if (chooseAtRuntime == theBlockSize)
    this(ubyte[] data, uint blockSize)
    {
        this._blockSize = blockSize;
        this(data);
    }

    static if (!is(ParentAllocator == NullAllocator) && !stateSize!ParentAllocator)
    this(size_t capacity)
    {
        size_t toAllocate = totalAllocation(capacity);
        auto data = cast(ubyte[])(parent.allocate(toAllocate));
        this(data);
        assert(_blocks * blockSize >= capacity);
    }

    static if (!is(ParentAllocator == NullAllocator) && stateSize!ParentAllocator)
    this(ParentAllocator parent, size_t capacity)
    {
        this.parent = parent;
        size_t toAllocate = totalAllocation(capacity);
        auto data = cast(ubyte[])(parent.allocate(toAllocate));
        this(data);
    }

    static if (!is(ParentAllocator == NullAllocator) &&
        chooseAtRuntime == theBlockSize &&
        !stateSize!ParentAllocator)
    this(size_t capacity, uint blockSize)
    {
        this._blockSize = blockSize;
        this(capacity);
    }

    static if (!is(ParentAllocator == NullAllocator) &&
        chooseAtRuntime == theBlockSize &&
        stateSize!ParentAllocator)
    this(ParentAllocator parent, size_t capacity, uint blockSize)
    {
        this._blockSize = blockSize;
        this(parent, capacity);
    }

    static if (!is(ParentAllocator == NullAllocator)
        && hasMember!(ParentAllocator, "deallocate"))
    ~this()
    {
        // multiblock bitmapped blocks use a BitVector
        static if (multiBlock)
        {
            void* start = cast(void*) _control.rep.ptr;
        }
        else
        {
            void* start = cast(void*) _control.ptr;
        }
        void* end = cast(void*) (_payload.ptr + _payload.length);
        parent.deallocate(start[0 .. end - start]);
    }

    pure nothrow @safe @nogc
    size_t goodAllocSize(size_t n)
    {
        return n.roundUpToMultipleOf(blockSize);
    }

    // Implementation of the 'multiBlock' BitmappedBlock
    // For the shared version, the methods are protected by a common lock
    static if (multiBlock)
    {
        /*
        Adjusts the memoized _startIdx to the leftmost control word that has at
        least one zero bit. Assumes all control words to the left of $(D
        _control[_startIdx]) are already occupied.
        */
        private void adjustStartIdx()
        {
            while (_startIdx < _control.rep.length && _control.rep[_startIdx] == ulong.max)
            {
                static if (isShared)
                {
                    // Shared demands atomic increment, however this is protected
                    // by a lock. Regular increment is fine
                    auto localStart = _startIdx + 1;
                    _startIdx = localStart;
                }
                else
                {
                    ++_startIdx;
                }
            }
        }

        /*
        Based on the latest allocated bit, 'newBit', it adjusts '_freshBit'
        */
        pure nothrow @safe @nogc
        private void adjustFreshBit(const ulong newBit)
        {
            import std.algorithm.comparison : max;
            static if (isShared)
            {
                auto localFreshBit = max(newBit, _freshBit);
                _freshBit = localFreshBit;
            }
            else
            {
                _freshBit = max(newBit, _freshBit);
            }
        }

        /*
        Returns the blocks corresponding to the control bits starting at word index
        wordIdx and bit index msbIdx (MSB=0) for a total of howManyBlocks.
        */
        @trusted
        private void[] blocksFor(this _)(size_t wordIdx, uint msbIdx, size_t howManyBlocks)
        {
            assert(msbIdx <= 63);
            const start = (wordIdx * 64 + msbIdx) * blockSize;
            const end = start + blockSize * howManyBlocks;
            if (start == end) return null;
            if (end <= _payload.length) return cast(void[]) _payload[start .. end];
            // This could happen if we have more control bits than available memory.
            // That's possible because the control bits are rounded up to fit in
            // 64-bit words.
            return null;
        }

        static if (isShared)
        nothrow @safe @nogc
        void[] allocate(const size_t s)
        {
            lock.lock();
            scope(exit) lock.unlock();

            return allocateImpl(s);
        }

        static if (!isShared)
        pure nothrow @safe @nogc
        void[] allocate(const size_t s)
        {
            return allocateImpl(s);
        }


        // If shared, this is protected by a lock inside 'allocate'
        pure nothrow @trusted @nogc
        private void[] allocateImpl(const size_t s)
        {
            const blocks = s.divideRoundUp(blockSize);
            void[] result;

        Lswitch:
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
                    assert(j < 64, "Invalid number of blocks");
                    assert((_control.rep[i] & ((1UL << 63) >> j)) == 0, "Corrupted bitmap");
                    static if (isShared)
                    {
                        // Need the cast because shared does not recognize the lock
                        *(cast(ulong*) &_control._rep[i]) |= (1UL << 63) >> j;
                    }
                    else
                    {
                        _control.rep[i] |= (1UL << 63) >> j;
                    }
                    if (i == _startIdx)
                    {
                        adjustStartIdx();
                    }
                    result = blocksFor(i, j, 1);
                    break Lswitch;
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
            if (result)
            {
                adjustFreshBit((result.ptr - _payload.ptr) / blockSize + blocks);
            }
            return result.ptr ? result.ptr[0 .. s] : null;
        }

        @trusted void[] allocateFresh(const size_t s)
        {
            static if (isShared)
            {
                lock.lock();
                scope(exit) lock.unlock();
            }

            const blocks = s.divideRoundUp(blockSize);

            void[] result = blocksFor(cast(size_t) (_freshBit / 64),
                cast(uint) (_freshBit % 64), blocks);
            if (result)
            {
                (cast(BitVector) _control)[_freshBit .. _freshBit + blocks] = 1;
                static if (isShared)
                {
                    ulong localFreshBit = _freshBit;
                    localFreshBit += blocks;
                    _freshBit = localFreshBit;
                }
                else
                {
                    _freshBit += blocks;
                }
            }
            return result;
        }

        void[] alignedAllocate(size_t n, uint a)
        {
            static if (isShared)
            {
                lock.lock();
                scope(exit) lock.unlock();
            }

            return alignedAllocateImpl(n, a);
        }

        // If shared, this is protected by a lock inside 'alignedAllocate'
        private void[] alignedAllocateImpl(size_t n, uint a)
        {
            import std.math.traits : isPowerOf2;
            assert(a.isPowerOf2);
            if (a <= alignment) return allocate(n);

            // Overallocate to make sure we can get an aligned block
            auto b = allocateImpl((n + a - alignment).roundUpToMultipleOf(blockSize));
            if (!b.ptr) return null;
            auto result = b.roundStartToMultipleOf(a);
            assert(result.length >= n);
            result = result.ptr[0 .. n]; // final result

            // Free any blocks that might be slack at the beginning
            auto slackHeadingBlocks = (result.ptr - b.ptr) / blockSize;
            if (slackHeadingBlocks)
            {
                deallocateImpl(b[0 .. slackHeadingBlocks * blockSize]);
            }

            // Free any blocks that might be slack at the end
            auto slackTrailingBlocks = ((b.ptr + b.length)
                - (result.ptr + result.length)) / blockSize;
            if (slackTrailingBlocks)
            {
                deallocateImpl(b[$ - slackTrailingBlocks * blockSize .. $]);
            }

            return result;
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
            void[] tmpResult;
            result = null;
            if (msbIdx + blocks <= 64)
            {
                // Allocation should fit this control word
                static if (isShared)
                {
                    ulong localControl = _control.rep[wordIdx];
                    bool didSetBit = setBitsIfZero(localControl,
                        cast(uint) (64 - msbIdx - blocks), 63 - msbIdx);
                    _control.rep[wordIdx] = localControl;
                }
                else
                {
                    bool didSetBit = setBitsIfZero(_control.rep[wordIdx],
                        cast(uint) (64 - msbIdx - blocks), 63 - msbIdx);
                }
                if (didSetBit)
                {
                    tmpResult = blocksFor(wordIdx, msbIdx, blocks);
                    if (!tmpResult)
                    {
                        static if (isShared)
                        {
                            localControl = _control.rep[wordIdx];
                            resetBits(localControl,
                                cast(uint) (64 - msbIdx - blocks), 63 - msbIdx);
                            _control.rep[wordIdx] = localControl;
                        }
                        else
                        {
                            resetBits(_control.rep[wordIdx],
                                cast(uint) (64 - msbIdx - blocks), 63 - msbIdx);
                        }
                        return tuple(size_t.max - 1, 0u);
                    }
                    result = tmpResult;
                    tmpResult = null;
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
                tmpResult = blocksFor(wordIdx, msbIdx, blocks);
                if (!tmpResult)
                {
                    return tuple(size_t.max - 1, 0u);
                }
                static if (isShared)
                {
                    // Dont want atomics, because this is protected by 'lock'
                    ulong localControl = _control.rep[wordIdx];
                    localControl |= mask;
                    _control.rep[wordIdx] = localControl;
                }
                else
                {
                    _control.rep[wordIdx] |= mask;
                }
                result = tmpResult;
                tmpResult = null;
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
            static if (isShared)
            {
                ulong localControl = _control.rep[wordIdx];
                localControl |= ulong.max >> available;
                _control.rep[wordIdx] = localControl;
            }
            else
            {
                _control.rep[wordIdx] |= ulong.max >> available;
            }
            return available;
        }

        pure nothrow @safe @nogc
        private void[] smallAlloc(uint blocks) return scope
        {
            assert(blocks >= 2 && blocks <= 64);
            void[] result;
            foreach (i; _startIdx .. _control.rep.length)
            {
                // Test within the current 64-bit word
                const v = _control.rep[i];
                if (v == ulong.max) continue;
                auto j = findContigOnes(~v, blocks);
                if (j < 64)
                {
                    // yay, found stuff
                    result = blocksFor(i, j, blocks);
                    if (result)
                    {
                        static if (isShared)
                        {
                            ulong localControl = _control.rep[i];
                            setBits(localControl, 64 - j - blocks, 63 - j);
                            _control.rep[i] = localControl;
                        }
                        else
                        {
                            setBits(_control.rep[i], 64 - j - blocks, 63 - j);
                        }
                    }
                    return result;
                }
                // Next, try allocations that cross a word
                auto available = trailingZeros(v);
                if (available == 0) continue;
                if (i + 1 >= _control.rep.length) break;
                assert(available < blocks); // otherwise we should have found it
                auto needed = blocks - available;
                assert(needed > 0 && needed < 64);
                result = blocksFor(i, 64 - available, blocks);
                if (result && allocateAtFront(i + 1, needed))
                {
                    static if (isShared)
                    {
                        ulong localControl = _control.rep[i];
                        localControl |= (1UL << available) - 1;
                        _control.rep[i] = localControl;
                    }
                    else
                    {
                        _control.rep[i] |= (1UL << available) - 1;
                    }
                    return result;
                }
            }
            return null;
        }

        pure nothrow @trusted @nogc
        private void[] hugeAlloc(size_t blocks) return scope
        {
            assert(blocks > 64);
            if (_startIdx == _control._rep.length)
            {
                assert((cast(BitVector) _control).allAre1);
                return null;
            }

            auto i = (cast(BitVector)_control).findZeros(blocks, _startIdx * 64);
            if (i == i.max || i + blocks > _blocks) return null;
            // Allocate those bits
            (cast(BitVector) _control)[i .. i + blocks] = 1;
            return cast(void[]) _payload[cast(size_t) (i * blockSize)
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
            static if (isShared)
            {
                ulong localControl = _control.rep[wordIdx];
                localControl |= ~mask;
                _control.rep[wordIdx] = localControl;
            }
            else
            {
                _control.rep[wordIdx] |= ~mask;
            }
            return true;
        }

        // Since the lock is not pure, only the single threaded 'expand' is pure
        static if (isShared)
        {
            nothrow @trusted @nogc
            bool expand(ref void[] b, immutable size_t delta)
            {
                lock.lock();
                scope(exit) lock.unlock();

                return expandImpl(b, delta);
            }
        }
        else
        {
            pure nothrow @trusted @nogc
            bool expand(ref void[] b, immutable size_t delta)
            {
                return expandImpl(b, delta);
            }
        }

        // If shared, this is protected by a lock inside 'expand'
        pure nothrow @trusted @nogc
        private bool expandImpl(ref void[] b, immutable size_t delta)
        {
            // Dispose with trivial corner cases
            if (b is null || delta == 0) return delta == 0;

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
            assert(p.ptr == b.ptr + blocksOld * blockSize);
            b = b.ptr[0 .. b.length + delta];
            adjustFreshBit(blockIdx + blocksNew);
            return true;
        }

        @system bool reallocate(ref void[] b, size_t newSize)
        {
            static if (isShared)
            {
                lock.lock();
                scope(exit) lock.unlock();
            }

            return reallocateImpl(b, newSize);
        }

        // If shared, this is protected by a lock inside 'reallocate'
        private @system bool reallocateImpl(ref void[] b, size_t newSize)
        {
            static bool slowReallocate(Allocator)(ref Allocator a, ref void[] b, size_t s)
            {
                if (b.length == s) return true;
                if (b.length <= s && a.expandImpl(b, s - b.length)) return true;
                auto newB = a.allocateImpl(s);
                if (newB.length != s) return false;
                if (newB.length <= b.length) newB[] = b[0 .. newB.length];
                else newB[0 .. b.length] = b[];
                a.deallocateImpl(b);
                b = newB;
                return true;
            }

            if (!b.ptr)
            {
                b = allocateImpl(newSize);
                return b.length == newSize;
            }
            if (newSize == 0)
            {
                deallocateImpl(b);
                b = null;
                return true;
            }
            if (newSize < b.length)
            {
                // Shrink. Will shrink in place by deallocating the trailing part.
                auto newCapacity = bytes2blocks(newSize) * blockSize;
                deallocateImpl(b[newCapacity .. $]);
                b = b[0 .. newSize];
                return true;
            }
            // Go the slow route
            return slowReallocate(this, b, newSize);
        }

        @system bool alignedReallocate(ref void[] b, size_t newSize, uint a)
        {
            static if (isShared)
            {
                lock.lock();
                scope(exit) lock.unlock();
            }

            return alignedReallocateImpl(b, newSize, a);
        }

        // If shared, this is protected by a lock inside 'alignedReallocate'
        private @system bool alignedReallocateImpl(ref void[] b, size_t newSize, uint a)
        {
            static bool slowAlignedReallocate(Allocator)(ref Allocator alloc,
                    ref void[] b, size_t s, uint a)
            {
                if (b.length <= s && b.ptr.alignedAt(a)
                    && alloc.expandImpl(b, s - b.length)) return true;

                auto newB = alloc.alignedAllocateImpl(s, a);
                if (newB.length != s) return false;
                if (newB.length <= b.length) newB[] = b[0 .. newB.length];
                else newB[0 .. b.length] = b[];
                alloc.deallocateImpl(b);
                b = newB;
                return true;
            }

            if (newSize == 0)
            {
                deallocateImpl(b);
                b = null;
                return true;
            }
            // Go the slow route
            return slowAlignedReallocate(this, b, newSize, a);
        }

        nothrow @nogc
        bool deallocate(void[] b)
        {
            static if (isShared)
            {
                lock.lock();
                scope(exit) lock.unlock();
            }

            return deallocateImpl(b);
        }

        // If shared, this is protected by a lock inside 'deallocate'
        nothrow @nogc
        private bool deallocateImpl(void[] b)
        {
            if (b is null) return true;

            // Locate position
            immutable pos = b.ptr - _payload.ptr;
            immutable blockIdx = pos / blockSize;

            // Adjust pointer, might be inside a block due to alignedAllocate
            void* begin = cast(void*) (_payload.ptr + blockIdx * blockSize),
                end = cast(void*) (b.ptr + b.length);
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
                    static if (isShared)
                    {
                        ulong localControl = _control.rep[wordIdx];
                        resetBits(localControl,
                            cast(uint) (64 - msbIdx - blocks),
                            63 - msbIdx);
                        _control.rep[wordIdx] = localControl;
                    }
                    else
                    {
                        resetBits(_control.rep[wordIdx],
                            cast(uint) (64 - msbIdx - blocks),
                            63 - msbIdx);
                    }
                    return true;
                }
                else
                {
                    static if (isShared)
                    {
                        ulong localControl = _control.rep[wordIdx];
                        localControl &= ulong.max << (64 - msbIdx);
                        _control.rep[wordIdx] = localControl;
                    }
                    else
                    {
                        _control.rep[wordIdx] &= ulong.max << (64 - msbIdx);
                    }
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
                static if (isShared)
                {
                    ulong localControl = _control.rep[wordIdx];
                    localControl &= ulong.max >> blocks;
                    _control.rep[wordIdx] = localControl;
                }
                else
                {
                    _control.rep[wordIdx] &= ulong.max >> blocks;
                }
            }
            return true;
        }

        // Since the lock is not pure, only the single threaded version is pure
        static if (isShared)
        {
            nothrow @nogc
            bool deallocateAll()
            {
                lock.lock();
                scope(exit) lock.unlock();

                (cast(BitVector) _control)[] = 0;
                _startIdx = 0;
                return true;
            }
        }
        else
        {
            pure nothrow @nogc
            bool deallocateAll()
            {
                _control[] = 0;
                _startIdx = 0;
                return true;
            }
        }

        // Since the lock is not pure, only the single threaded version is pure
        static if (isShared)
        {
            nothrow @safe @nogc
            Ternary empty()
            {
                lock.lock();
                scope(exit) lock.unlock();

                return emptyImpl();
            }
        }
        else
        {
            pure nothrow @safe @nogc
            Ternary empty()
            {
                return Ternary(_control.allAre0());
            }
        }

        pure nothrow @trusted @nogc
        private Ternary emptyImpl()
        {
            return Ternary((cast(BitVector) _control).allAre0());
        }

        // Debug helper
        debug(StdBitmapped)
        private void dump()
        {
            import std.stdio : writefln, writeln;

            ulong controlLen = (cast(BitVector) _control).length;
            writefln("%s @ %s {", typeid(this), cast(void*) (cast(BitVector) _control)._rep.ptr);
            scope(exit) writeln("}");
            assert(_payload.length >= blockSize * _blocks);
            assert(controlLen >= _blocks);
            writefln("  _startIdx=%s; blockSize=%s; blocks=%s",
                _startIdx, blockSize, _blocks);
            if (!controlLen) return;
            uint blockCount = 1;
            bool inAllocatedStore = (cast(BitVector) _control)[0];
            void* start = cast(void*) _payload.ptr;
            for (size_t i = 1;; ++i)
            {
                if (i >= _blocks || (cast(BitVector) _control)[i] != inAllocatedStore)
                {
                    writefln("  %s block at 0x%s, length: %s (%s*%s)",
                        inAllocatedStore ? "Busy" : "Free",
                        cast(void*) start,
                        blockCount * blockSize,
                        blockCount, blockSize);
                    if (i >= _blocks) break;
                    assert(i < controlLen);
                    inAllocatedStore = (cast(BitVector) _control)[i];
                    start = cast(void*) (_payload.ptr + blockCount * blockSize);
                    blockCount = 1;
                }
                else
                {
                    ++blockCount;
                }
            }
        }

        void[] allocateAll() return scope
        {
            static if (isShared)
            {
                lock.lock();
                scope(exit) lock.unlock();
            }

            if (emptyImpl != Ternary.yes) return null;
            (cast(BitVector) _control)[] = 1;
            return cast(void[]) _payload;
        }
    } // Finish Yes.multiblock implementation specifics
    else
    {
        static if (isShared)
        pure nothrow @trusted @nogc
        void[] allocate(const size_t s)
        {
            import core.atomic : cas, atomicLoad, atomicOp;
            import core.bitop : bsr;
            import std.range : iota;
            import std.algorithm.iteration : map;
            import std.array : array;

            if (s.divideRoundUp(blockSize) != 1)
                return null;

            // First zero bit position for all values in the 0 - 255 range
            // for fast lookup
            static immutable ubyte[255] firstZero = iota(255U).map!
                (x => (7 - (bsr((~x) & 0x000000ff)))).array;

            foreach (size_t i; 0 .. _control.length)
            {
                ulong controlVal, newControlVal, bitIndex;
                do
                {
                    bitIndex = 0;
                    newControlVal = 0;
                    controlVal = atomicLoad(_control[i]);

                    // skip all control words which have all bits set
                    if (controlVal == ulong.max)
                        break;

                    // fast lookup of first byte which has at least one zero bit
                    foreach (byteIndex; 0 .. 8)
                    {
                        ulong mask = (0xFFUL << (8 * (7 - byteIndex)));
                        if ((mask & controlVal) != mask)
                        {
                            ubyte byteVal = cast(ubyte) ((mask & controlVal) >> (8 * (7 - byteIndex)));
                            bitIndex += firstZero[byteVal];
                            newControlVal = controlVal | (1UL << (63 - bitIndex));
                            break;
                        }
                        bitIndex += 8;
                    }
                } while (!cas(&_control[i], controlVal, newControlVal));

                auto blockIndex = bitIndex + 64 * i;
                if (controlVal != ulong.max && blockIndex < _blocks)
                {
                    size_t payloadBlockStart = cast(size_t) blockIndex * blockSize;
                    return cast(void[]) _payload[payloadBlockStart .. payloadBlockStart + s];
                }
            }

            return null;
        }

        static if (!isShared)
        pure nothrow @trusted @nogc
        void[] allocate(const size_t s)
        {
            import core.bitop : bsr;
            import std.range : iota;
            import std.algorithm.iteration : map;
            import std.array : array;

            if (s.divideRoundUp(blockSize) != 1)
                return null;

            // First zero bit position for all values in the 0 - 255 range
            // for fast lookup
            static immutable ubyte[255] firstZero = iota(255U).map!
                (x => (7 - (bsr((~x) & 0x000000ff)))).array;

            _startIdx = (_startIdx + 1) % _control.length;
            foreach (size_t idx; 0 .. _control.length)
            {
                size_t i = (idx + _startIdx) % _control.length;
                size_t bitIndex = 0;
                // skip all control words which have all bits set
                if (_control[i] == ulong.max)
                    continue;

                // fast lookup of first byte which has at least one zero bit
                foreach (byteIndex; 0 .. 8)
                {
                    ulong mask = (0xFFUL << (8 * (7 - byteIndex)));
                    if ((mask & _control[i]) != mask)
                    {
                        ubyte byteVal = cast(ubyte) ((mask & _control[i]) >> (8 * (7 - byteIndex)));
                        bitIndex += firstZero[byteVal];
                        _control[i] |= (1UL << (63 - bitIndex));
                        break;
                    }
                    bitIndex += 8;
                }

                auto blockIndex = bitIndex + 64 * i;
                if (blockIndex < _blocks)
                {
                    size_t payloadBlockStart = cast(size_t) blockIndex * blockSize;
                    return cast(void[]) _payload[payloadBlockStart .. payloadBlockStart + s];
                }
            }

            return null;
        }

        nothrow @nogc
        bool deallocate(void[] b)
        {
            static if (isShared)
            import core.atomic : atomicOp;

            if (b is null)
                return true;

            auto blockIndex = (b.ptr - _payload.ptr) / blockSize;
            auto controlIndex = blockIndex / 64;
            auto bitIndex = blockIndex % 64;
            static if (isShared)
            {
                atomicOp!"&="(_control[controlIndex], ~(1UL << (63 - bitIndex)));
            }
            else
            {
                _control[controlIndex] &= ~(1UL << (63 - bitIndex));
            }

            return true;
        }

        pure nothrow @trusted @nogc
        bool expand(ref void[] b, immutable size_t delta)
        {
            if (delta == 0)
                return true;

            immutable newLength = delta + b.length;
            if (b is null || newLength > blockSize)
                return false;

            b = b.ptr[0 .. newLength];
            return true;
        }
    } // Finish No.multiblock implementation specifics

    pure nothrow @trusted @nogc
    Ternary owns(const void[] b) const
    {
        assert(b || b.length == 0, "Corrupt block.");
        return Ternary(b && _payload && (&b[0] >= &_payload[0])
               && (&b[0] + b.length) <= (&_payload[0] + _payload.length));
    }
}

/**
`BitmappedBlock` implements a simple heap consisting of one contiguous area
of memory organized in blocks, each of size `theBlockSize`. A block is a unit
of allocation. A bitmap serves as bookkeeping data, more precisely one bit per
block indicating whether that block is currently allocated or not.

Passing `NullAllocator` as `ParentAllocator` (the default) means user code
manages allocation of the memory block from the outside; in that case
`BitmappedBlock` must be constructed with a `ubyte[]` preallocated block and
has no responsibility regarding the lifetime of its support underlying storage.
If another allocator type is passed, `BitmappedBlock` defines a destructor that
uses the parent allocator to release the memory block. That makes the combination of `AllocatorList`,
`BitmappedBlock`, and a back-end allocator such as `MmapAllocator`
a simple and scalable solution for memory allocation.

There are advantages to storing bookkeeping data separated from the payload
(as opposed to e.g. using `AffixAllocator` to store metadata together with
each allocation). The layout is more compact (overhead is one bit per block),
searching for a free block during allocation enjoys better cache locality, and
deallocation does not touch memory around the payload being deallocated (which
is often cold).

Allocation requests are handled on a first-fit basis. Although linear in
complexity, allocation is in practice fast because of the compact bookkeeping
representation, use of simple and fast bitwise routines, and caching of the
first available block position. A known issue with this general approach is
fragmentation, partially mitigated by coalescing. Since `BitmappedBlock` does
not need to maintain the allocated size, freeing memory implicitly coalesces
free blocks together. Also, tuning `blockSize` has a considerable impact on
both internal and external fragmentation.

If the last template parameter is set to `No.multiblock`, the allocator will only serve
allocations which require at most `theBlockSize`. The `BitmappedBlock` has a specialized
implementation for single-block allocations which allows for greater performance,
at the cost of not being able to allocate more than one block at a time.

The size of each block can be selected either during compilation or at run
time. Statically-known block sizes are frequent in practice and yield slightly
better performance. To choose a block size statically, pass it as the `blockSize`
parameter as in `BitmappedBlock!(4096)`. To choose a block
size parameter, use `BitmappedBlock!(chooseAtRuntime)` and pass the
block size to the constructor.

Params:
    theBlockSize = the length of a block, which must be a multiple of `theAlignment`

    theAlignment = alignment of each block

    ParentAllocator = allocator from which the `BitmappedBlock` will draw memory.
        If set to `NullAllocator`, the storage must be passed via the constructor

    f = `Yes.multiblock` to support allocations spanning across multiple blocks and
        `No.multiblock` to support single block allocations.
        Although limited by single block allocations, `No.multiblock` will generally
        provide higher performance.
*/
struct BitmappedBlock(size_t theBlockSize, uint theAlignment = platformAlignment,
   ParentAllocator = NullAllocator, Flag!"multiblock" f = Yes.multiblock)
{
    version (StdDdoc)
    {
        /**
        Constructs a block allocator given a hunk of memory, or a desired capacity
        in bytes.
        $(UL
        $(LI If `ParentAllocator` is $(REF_ALTTEXT `NullAllocator`, NullAllocator, std,experimental,allocator,building_blocks,null_allocator),
        only the constructor taking `data` is defined and the user is responsible for freeing `data` if desired.)
        $(LI Otherwise, both constructors are defined. The `data`-based
        constructor assumes memory has been allocated with the parent allocator.
        The `capacity`-based constructor uses `ParentAllocator` to allocate
        an appropriate contiguous hunk of memory. Regardless of the constructor
        used, the destructor releases the memory by using `ParentAllocator.deallocate`.)
        )
        */
        this(ubyte[] data);

        /// Ditto
        this(ubyte[] data, uint blockSize);

        /// Ditto
        this(size_t capacity);

        /// Ditto
        this(ParentAllocator parent, size_t capacity);

        /// Ditto
        this(size_t capacity, uint blockSize);

        /// Ditto
        this(ParentAllocator parent, size_t capacity, uint blockSize);

        /**
        If `blockSize == chooseAtRuntime`, `BitmappedBlock` offers a read/write
        property `blockSize`. It must be set before any use of the allocator.
        Otherwise (i.e. `theBlockSize` is a legit constant), `blockSize` is
        an alias for `theBlockSize`. Whether constant or variable, must also be
        a multiple of `alignment`. This constraint is `assert`ed statically
        and dynamically.
        */
        alias blockSize = theBlockSize;

        /**
        The _alignment offered is user-configurable statically through parameter
        `theAlignment`, defaulted to `platformAlignment`.
        */
        alias alignment = theAlignment;

        /**
        The _parent allocator. Depending on whether `ParentAllocator` holds state
        or not, this is a member variable or an alias for
        `ParentAllocator.instance`.
        */
        ParentAllocator parent;

        /**
        Returns the actual bytes allocated when `n` bytes are requested, i.e.
        `n.roundUpToMultipleOf(blockSize)`.
        */
        pure nothrow @safe @nogc
        size_t goodAllocSize(size_t n);

        /**
        Returns `Ternary.yes` if `b` belongs to the `BitmappedBlock` object,
        `Ternary.no` otherwise. Never returns `Ternary.unkown`. (This
        method is somewhat tolerant in that accepts an interior slice.)
        */
        pure nothrow @trusted @nogc
        Ternary owns(const void[] b) const;

        /**
        Expands in place a buffer previously allocated by `BitmappedBlock`.
        If instantiated with `No.multiblock`, the expansion fails if the new length
        exceeds `theBlockSize`.
        */
        pure nothrow @trusted @nogc
        bool expand(ref void[] b, immutable size_t delta);

        /**
        Deallocates a block previously allocated with this allocator.
        */
        nothrow @nogc
        bool deallocate(void[] b);

        /**
        Allocates `s` bytes of memory and returns it, or `null` if memory
        could not be allocated.

        The following information might be of help with choosing the appropriate
        block size. Actual allocation occurs in sizes multiple of the block size.
        Allocating one block is the fastest because only one 0 bit needs to be
        found in the metadata. Allocating 2 through 64 blocks is the next cheapest
        because it affects a maximum of two `ulong` in the metadata.
        Allocations greater than 64 blocks require a multiword search through the
        metadata.

        If instantiated with `No.multiblock`, it performs a search for the first zero
        bit in the bitmap and sets it.
        */
        pure nothrow @trusted @nogc
        void[] allocate(const size_t s);

        /**
        Allocates s bytes of memory and returns it, or `null` if memory could not be allocated.
        `allocateFresh` behaves just like allocate, the only difference being that this always
        returns unused(fresh) memory. Although there may still be available space in the `BitmappedBlock`,
        `allocateFresh` could still return null, because all the available blocks have been previously deallocated.
        */
        @trusted void[] allocateFresh(const size_t s);

        /**
        If the `BitmappedBlock` object is empty (has no active allocation), allocates
        all memory within and returns a slice to it. Otherwise, returns `null`
        (i.e. no attempt is made to allocate the largest available block).
        */
        void[] allocateAll();

        /**
        Returns `Ternary.yes` if no memory is currently allocated with this
        allocator, otherwise `Ternary.no`. This method never returns
        `Ternary.unknown`.
        */
        pure nothrow @safe @nogc
        Ternary empty();

        /**
        Forcibly deallocates all memory allocated by this allocator, making it
        available for further allocations. Does not return memory to `ParentAllocator`.
        */
        pure nothrow @nogc
        bool deallocateAll();

        /**
        Reallocates a block previously allocated with `alignedAllocate`. Contractions do not occur in place.
        */
        @system bool alignedReallocate(ref void[] b, size_t newSize, uint a);

        /**
        Reallocates a previously-allocated block. Contractions occur in place.
        */
        @system bool reallocate(ref void[] b, size_t newSize);

        /**
        Allocates a block with specified alignment `a`. The alignment must be a
        power of 2. If `a <= alignment`, function forwards to `allocate`.
        Otherwise, it attempts to overallocate and then adjust the result for
        proper alignment. In the worst case the slack memory is around two blocks.
        */
        void[] alignedAllocate(size_t n, uint a);

        /**
        If `ParentAllocator` is not `NullAllocator` and defines `deallocate`,
        the destructor is defined to deallocate the block held.
        */
        ~this();
    }
    else
    {
        version (StdUnittest)
        @system unittest
        {
            import std.algorithm.comparison : max;
            import std.experimental.allocator.mallocator : AlignedMallocator;
            auto m = cast(ubyte[])(AlignedMallocator.instance.alignedAllocate(1024 * 64,
                                    max(theAlignment, cast(uint) size_t.sizeof)));
            scope(exit) () nothrow @nogc { AlignedMallocator.instance.deallocate(m); }();
            static if (theBlockSize == chooseAtRuntime)
            {
                testAllocator!(() => BitmappedBlock!(theBlockSize, theAlignment, NullAllocator)(m, 64));
            }
            else
            {
                testAllocator!(() => BitmappedBlock!(theBlockSize, theAlignment, NullAllocator)(m));
            }
        }
        mixin BitmappedBlockImpl!(false, f == Yes.multiblock);
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

///
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Flag, Yes;

    enum blockSize = 64;
    enum numBlocks = 10;

    // The 'BitmappedBlock' is implicitly instantiated with Yes.multiblock
    auto a = BitmappedBlock!(blockSize, 8, Mallocator, Yes.multiblock)(numBlocks * blockSize);

    // Instantiated with Yes.multiblock, can allocate more than one block at a time
    void[] buf = a.allocate(2 * blockSize);
    assert(buf.length == 2 * blockSize);
    assert(a.deallocate(buf));

    // Can also allocate less than one block
    buf = a.allocate(blockSize / 2);
    assert(buf.length == blockSize / 2);

    // Expands inside the same block
    assert(a.expand(buf, blockSize / 2));
    assert(buf.length == blockSize);

    // If Yes.multiblock, can expand past the size of a single block
    assert(a.expand(buf, 3 * blockSize));
    assert(buf.length == 4 * blockSize);
    assert(a.deallocate(buf));
}

///
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.typecons : Flag, No;

    enum blockSize = 64;
    auto a = BitmappedBlock!(blockSize, 8, Mallocator, No.multiblock)(1024 * blockSize);

    // Since instantiated with No.multiblock, can only allocate at most the block size
    void[] buf = a.allocate(blockSize + 1);
    assert(buf is null);

    buf = a.allocate(blockSize);
    assert(buf.length == blockSize);
    assert(a.deallocate(buf));

    // This is also fine, because it's less than the block size
    buf = a.allocate(blockSize / 2);
    assert(buf.length == blockSize / 2);

    // Can expand the buffer until its length is at most 64
    assert(a.expand(buf, blockSize / 2));
    assert(buf.length == blockSize);

    // Cannot expand anymore
    assert(!a.expand(buf, 1));
    assert(a.deallocate(buf));
}

// Test instantiation with stateful allocators
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.building_blocks.region : Region;
    auto r = Region!Mallocator(1024 * 96);
    auto a = BitmappedBlock!(chooseAtRuntime, 8, Region!Mallocator*, No.multiblock)(&r, 1024 * 64, 1024);
}

/**
The threadsafe version of the $(LREF BitmappedBlock).
The semantics of the `SharedBitmappedBlock` are identical to the regular $(LREF BitmappedBlock).

Params:
    theBlockSize = the length of a block, which must be a multiple of `theAlignment`

    theAlignment = alignment of each block

    ParentAllocator = allocator from which the `BitmappedBlock` will draw memory.
        If set to `NullAllocator`, the storage must be passed via the constructor

    f = `Yes.multiblock` to support allocations spanning across multiple blocks and
        `No.multiblock` to support single block allocations.
        Although limited by single block allocations, `No.multiblock` will generally
        provide higher performance.
*/
shared struct SharedBitmappedBlock(size_t theBlockSize, uint theAlignment = platformAlignment,
   ParentAllocator = NullAllocator, Flag!"multiblock" f = Yes.multiblock)
{
    version (StdDdoc)
    {
        /**
        Constructs a block allocator given a hunk of memory, or a desired capacity
        in bytes.
        $(UL
        $(LI If `ParentAllocator` is $(REF_ALTTEXT `NullAllocator`, NullAllocator, std,experimental,allocator,building_blocks,null_allocator),
        only the constructor taking `data` is defined and the user is responsible for freeing `data` if desired.)
        $(LI Otherwise, both constructors are defined. The `data`-based
        constructor assumes memory has been allocated with the parent allocator.
        The `capacity`-based constructor uses `ParentAllocator` to allocate
        an appropriate contiguous hunk of memory. Regardless of the constructor
        used, the destructor releases the memory by using `ParentAllocator.deallocate`.)
        )
        */
        this(ubyte[] data);

        /// Ditto
        this(ubyte[] data, uint blockSize);

        /// Ditto
        this(size_t capacity);

        /// Ditto
        this(ParentAllocator parent, size_t capacity);

        /// Ditto
        this(size_t capacity, uint blockSize);

        /// Ditto
        this(ParentAllocator parent, size_t capacity, uint blockSize);

        /**
        If `blockSize == chooseAtRuntime`, `SharedBitmappedBlock` offers a read/write
        property `blockSize`. It must be set before any use of the allocator.
        Otherwise (i.e. `theBlockSize` is a legit constant), `blockSize` is
        an alias for `theBlockSize`. Whether constant or variable, must also be
        a multiple of `alignment`. This constraint is `assert`ed statically
        and dynamically.
        */
        alias blockSize = theBlockSize;

        /**
        The _alignment offered is user-configurable statically through parameter
        `theAlignment`, defaulted to `platformAlignment`.
        */
        alias alignment = theAlignment;

        /**
        The _parent allocator. Depending on whether `ParentAllocator` holds state
        or not, this is a member variable or an alias for
        `ParentAllocator.instance`.
        */
        ParentAllocator parent;

        /**
        Returns the actual bytes allocated when `n` bytes are requested, i.e.
        `n.roundUpToMultipleOf(blockSize)`.
        */
        pure nothrow @safe @nogc
        size_t goodAllocSize(size_t n);

        /**
        Returns `Ternary.yes` if `b` belongs to the `SharedBitmappedBlock` object,
        `Ternary.no` otherwise. Never returns `Ternary.unkown`. (This
        method is somewhat tolerant in that accepts an interior slice.)
        */
        pure nothrow @trusted @nogc
        Ternary owns(const void[] b) const;

        /**
        Expands in place a buffer previously allocated by `SharedBitmappedBlock`.
        Expansion fails if the new length exceeds the block size.
        */
        bool expand(ref void[] b, immutable size_t delta);

        /**
        Deallocates the given buffer `b`, by atomically setting the corresponding
        bit to `0`. `b` must be valid, and cannot contain multiple adjacent `blocks`.
        */
        nothrow @nogc
        bool deallocate(void[] b);

        /**
        Allocates `s` bytes of memory and returns it, or `null` if memory
        could not be allocated.

        The `SharedBitmappedBlock` cannot allocate more than the given block size.
        Allocations are satisfied by searching the first unset bit in the bitmap,
        and atomically setting it.
        In rare memory pressure scenarios, the allocation could fail.
        */
        nothrow @trusted @nogc
        void[] allocate(const size_t s);

        /**
        Allocates s bytes of memory and returns it, or `null` if memory could not be allocated.
        `allocateFresh` behaves just like allocate, the only difference being that this always
        returns unused(fresh) memory. Although there may still be available space in the `SharedBitmappedBlock`,
        `allocateFresh` could still return null, because all the available blocks have been previously deallocated.
        */
        @trusted void[] allocateFresh(const size_t s);

        /**
        If the `SharedBitmappedBlock` object is empty (has no active allocation), allocates
        all memory within and returns a slice to it. Otherwise, returns `null`
        (i.e. no attempt is made to allocate the largest available block).
        */
        void[] allocateAll();

        /**
        Returns `Ternary.yes` if no memory is currently allocated with this
        allocator, otherwise `Ternary.no`. This method never returns
        `Ternary.unknown`.
        */
        nothrow @safe @nogc
        Ternary empty();

        /**
        Forcibly deallocates all memory allocated by this allocator, making it
        available for further allocations. Does not return memory to `ParentAllocator`.
        */
        nothrow @nogc
        bool deallocateAll();

        /**
        Reallocates a block previously allocated with `alignedAllocate`. Contractions do not occur in place.
        */
        @system bool alignedReallocate(ref void[] b, size_t newSize, uint a);

        /**
        Reallocates a previously-allocated block. Contractions occur in place.
        */
        @system bool reallocate(ref void[] b, size_t newSize);

        /**
        Allocates a block with specified alignment `a`. The alignment must be a
        power of 2. If `a <= alignment`, function forwards to `allocate`.
        Otherwise, it attempts to overallocate and then adjust the result for
        proper alignment. In the worst case the slack memory is around two blocks.
        */
        void[] alignedAllocate(size_t n, uint a);

        /**
        If `ParentAllocator` is not `NullAllocator` and defines `deallocate`,
        the destructor is defined to deallocate the block held.
        */
        ~this();
    }
    else
    {
        version (StdUnittest)
        @system unittest
        {
            import std.algorithm.comparison : max;
            import std.experimental.allocator.mallocator : AlignedMallocator;
            auto m = cast(ubyte[])(AlignedMallocator.instance.alignedAllocate(1024 * 64,
                                    max(theAlignment, cast(uint) size_t.sizeof)));
            scope(exit) () nothrow @nogc { AlignedMallocator.instance.deallocate(m); }();
            static if (theBlockSize == chooseAtRuntime)
            {
                testAllocator!(() => SharedBitmappedBlock!(theBlockSize, theAlignment, NullAllocator)(m, 64));
            }
            else
            {
                testAllocator!(() => SharedBitmappedBlock!(theBlockSize, theAlignment, NullAllocator)(m));
            }
        }
        mixin BitmappedBlockImpl!(true, f == Yes.multiblock);
    }
}

///
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.common : platformAlignment;
    import std.typecons : Flag, Yes, No;

    // Create 'numThreads' threads, each allocating in parallel a chunk of memory
    static void testAlloc(Allocator)(ref Allocator a, size_t allocSize)
    {
        import core.thread : ThreadGroup;
        import std.algorithm.sorting : sort;
        import core.internal.spinlock : SpinLock;

        SpinLock lock = SpinLock(SpinLock.Contention.brief);
        enum numThreads = 10;
        void[][numThreads] buf;
        size_t count = 0;

        // Each threads allocates 'allocSize'
        void fun()
        {
            void[] b = a.allocate(allocSize);
            assert(b.length == allocSize);

            lock.lock();
            scope(exit) lock.unlock();

            buf[count] = b;
            count++;
        }

        auto tg = new ThreadGroup;
        foreach (i; 0 .. numThreads)
        {
            tg.create(&fun);
        }
        tg.joinAll();

        // Sorting the allocations made by each thread, we expect the buffers to be
        // adjacent inside the SharedBitmappedBlock
        sort!((a, b) => a.ptr < b.ptr)(buf[0 .. numThreads]);
        foreach (i; 0 .. numThreads - 1)
        {
            assert(buf[i].ptr + a.goodAllocSize(buf[i].length) <= buf[i + 1].ptr);
        }

        // Deallocate everything
        foreach (i; 0 .. numThreads)
        {
            assert(a.deallocate(buf[i]));
        }
    }

    enum blockSize = 64;
    auto alloc1 = SharedBitmappedBlock!(blockSize, platformAlignment, Mallocator, Yes.multiblock)(1024 * 1024);
    auto alloc2 = SharedBitmappedBlock!(blockSize, platformAlignment, Mallocator, No.multiblock)(1024 * 1024);
    testAlloc(alloc1, 2 * blockSize);
    testAlloc(alloc2, blockSize);
}

@system unittest
{
    // Test chooseAtRuntime
    // Create a block allocator on top of a 10KB stack region.
    import std.experimental.allocator.building_blocks.region : InSituRegion;
    import std.traits : hasMember;
    InSituRegion!(10_240, 64) r;
    uint blockSize = 64;
    auto a = BitmappedBlock!(chooseAtRuntime, 64)(cast(ubyte[])(r.allocateAll()), blockSize);
    static assert(hasMember!(InSituRegion!(10_240, 64), "allocateAll"));
    const b = (() pure nothrow @safe @nogc => a.allocate(100))();
    assert(b.length == 100);
}

pure @safe unittest
{
    import std.typecons : Ternary;

    auto a = (() @trusted => BitmappedBlock!(64, 64, NullAllocator, Yes.multiblock)(new ubyte[10_240]))();
    () nothrow @nogc {
        assert(a.empty == Ternary.yes);
        const b = a.allocate(100);
        assert(b.length == 100);
        assert(a.empty == Ternary.no);
    }();
}

@safe unittest
{
    import std.typecons : Ternary;

    auto a = (() @trusted => SharedBitmappedBlock!(64, 64, NullAllocator, Yes.multiblock)(new ubyte[10_240]))();
    assert((() nothrow @safe @nogc => a.empty)() == Ternary.yes);
    const b = a.allocate(100);
    assert(b.length == 100);
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    testAllocator!(() => BitmappedBlock!(64, 8, GCAllocator)(1024 * 64));
}

version (StdUnittest)
@system unittest
{
    // Test chooseAtRuntime
    import std.experimental.allocator.gc_allocator : GCAllocator;
    uint blockSize = 64;
    testAllocator!(() => BitmappedBlock!(chooseAtRuntime, 8, GCAllocator, Yes.multiblock)(1024 * 64, blockSize));
    testAllocator!(() => BitmappedBlock!(chooseAtRuntime, 8, GCAllocator, No.multiblock)(1024 * 64, blockSize));
}

version (StdUnittest)
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    testAllocator!(() => SharedBitmappedBlock!(64, 8, Mallocator, Yes.multiblock)(1024 * 64));
    testAllocator!(() => SharedBitmappedBlock!(64, 8, Mallocator, No.multiblock)(1024 * 64));
}

version (StdUnittest)
@system unittest
{
    // Test chooseAtRuntime
    import std.experimental.allocator.mallocator : Mallocator;
    uint blockSize = 64;
    testAllocator!(() => SharedBitmappedBlock!(chooseAtRuntime, 8, Mallocator, Yes.multiblock)(1024 * 64, blockSize));
    testAllocator!(() => SharedBitmappedBlock!(chooseAtRuntime, 8, Mallocator, No.multiblock)(1024 * 64, blockSize));
}

@system unittest
{
    static void testAllocateAll(size_t bs, bool isShared = true)(size_t blocks, uint blocksAtATime)
    {
        template attribAllocate(string size)
        {
            static if (isShared)
            {
                const char[] attribAllocate = "(() nothrow @safe @nogc => a.allocate(" ~ size ~ "))()";
            }
            else
            {
                const char[] attribAllocate = "(() pure nothrow @safe @nogc => a.allocate(" ~ size ~ "))()";
            }
        }

        assert(bs);
        import std.typecons : Ternary;
        import std.algorithm.comparison : min;
        import std.experimental.allocator.gc_allocator : GCAllocator;

        static if (isShared)
        {
            auto a = SharedBitmappedBlock!(bs, min(bs, platformAlignment), NullAllocator)(
                cast(ubyte[])(GCAllocator.instance.allocate((blocks * bs * 8 + blocks) / 8)));
        }
        else
        {
            auto a = BitmappedBlock!(bs, min(bs, platformAlignment), NullAllocator)(
                cast(ubyte[])(GCAllocator.instance.allocate((blocks * bs * 8 + blocks) / 8)));
        }

        import std.conv : text;
        assert(blocks >= a._blocks, text(blocks, " < ", a._blocks));
        blocks = a._blocks;

        // test allocation of 0 bytes
        auto x = mixin(attribAllocate!("0"));
        assert(x is null);
        // test allocation of 1 byte
        x = mixin(attribAllocate!("1"));
        assert(x.length == 1 || blocks == 0);
        assert((() nothrow @nogc => a.deallocateAll())());
        assert(a.empty() == Ternary.yes);
        bool twice = true;

    begin:
        foreach (i; 0 .. blocks / blocksAtATime)
        {
            auto b = mixin(attribAllocate!("bs * blocksAtATime"));
            assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
        }

        assert(mixin(attribAllocate!("bs * blocksAtATime")) is null);
        if (a._blocks % blocksAtATime == 0)
        {
            assert(mixin(attribAllocate!("1")) is null);
        }

        // Now deallocate all and do it again!
        assert((() nothrow @nogc => a.deallocateAll())());

        // Test deallocation

        auto v = new void[][blocks / blocksAtATime];
        foreach (i; 0 .. blocks / blocksAtATime)
        {
            auto b = mixin(attribAllocate!("bs * blocksAtATime"));
            assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
            v[i] = b;
        }
        assert(mixin(attribAllocate!("bs * blocksAtATime")) is null);
        if (a._blocks % blocksAtATime == 0)
        {
            assert(mixin(attribAllocate!("1")) is null);
        }

        foreach (i; 0 .. blocks / blocksAtATime)
        {
            () nothrow @nogc { a.deallocate(v[i]); }();
        }

        foreach (i; 0 .. blocks / blocksAtATime)
        {
            auto b = mixin(attribAllocate!("bs * blocksAtATime"));
            assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
            v[i] = b;
        }

        foreach (i; 0 .. v.length)
        {
            () nothrow @nogc { a.deallocate(v[i]); }();
        }

        if (twice)
        {
            twice = false;
            goto begin;
        }

        assert((() nothrow @nogc => a.deallocateAll())());

        // test expansion
        if (blocks >= blocksAtATime)
        {
            foreach (i; 0 .. blocks / blocksAtATime - 1)
            {
                auto b = mixin(attribAllocate!("bs * blocksAtATime"));
                assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
                (cast(ubyte[]) b)[] = 0xff;
                static if (isShared)
                {
                    assert((() nothrow @safe @nogc => a.expand(b, blocksAtATime * bs))()
                            , text(i));
                }
                else
                {
                    assert((() pure nothrow @safe @nogc => a.expand(b, blocksAtATime * bs))()
                            , text(i));
                }
                (cast(ubyte[]) b)[] = 0xfe;
                assert(b.length == bs * blocksAtATime * 2, text(i, ": ", b.length));
                a.reallocate(b, blocksAtATime * bs) || assert(0);
                assert(b.length == bs * blocksAtATime, text(i, ": ", b.length));
            }
        }
    }

    testAllocateAll!(1)(0, 1);
    testAllocateAll!(1, false)(0, 1);
    testAllocateAll!(1)(8, 1);
    testAllocateAll!(1, false)(8, 1);

    testAllocateAll!(4096)(128, 1);
    testAllocateAll!(4096, false)(128, 1);

    testAllocateAll!(1)(0, 2);
    testAllocateAll!(1)(128, 2);
    testAllocateAll!(4096)(128, 2);

    testAllocateAll!(1, false)(0, 2);
    testAllocateAll!(1, false)(128, 2);
    testAllocateAll!(4096, false)(128, 2);

    testAllocateAll!(1)(0, 4);
    testAllocateAll!(1)(128, 4);
    testAllocateAll!(4096)(128, 4);

    testAllocateAll!(1, false)(0, 4);
    testAllocateAll!(1, false)(128, 4);
    testAllocateAll!(4096, false)(128, 4);

    testAllocateAll!(1)(0, 3);
    testAllocateAll!(1)(24, 3);
    testAllocateAll!(3008)(100, 1);
    testAllocateAll!(3008)(100, 3);

    testAllocateAll!(1, false)(0, 3);
    testAllocateAll!(1, false)(24, 3);
    testAllocateAll!(3008, false)(100, 1);
    testAllocateAll!(3008, false)(100, 3);

    testAllocateAll!(1)(0, 128);
    testAllocateAll!(1)(128 * 1, 128);
    testAllocateAll!(128 * 20)(13 * 128, 128);

    testAllocateAll!(1, false)(0, 128);
    testAllocateAll!(1, false)(128 * 1, 128);
    testAllocateAll!(128 * 20, false)(13 * 128, 128);
}

@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;

    enum blocks = 10000;
    int count = 0;

    ubyte[] payload = cast(ubyte[]) Mallocator.instance.allocate(blocks * 16);
    auto a = BitmappedBlock!(16, 16)(payload);
    void[][] buf = cast(void[][]) Mallocator.instance.allocate((void[]).sizeof * blocks);

    assert(!a.allocateFresh(0));
    assert(!a._control[0]);

    void[] b = a.allocate(256 * 16);
    assert(b.length == 256 * 16);
    count += 256;

    assert(!a._control[count]);
    b = a.allocateFresh(16);
    assert(b.length == 16);
    count++;
    assert(a._control[count - 1]);

    b = a.allocateFresh(16 * 300);
    assert(b.length == 16 * 300);
    count += 300;

    for (int i = 0; i < count; i++)
        assert(a._control[i]);
    assert(!a._control[count]);

    assert(a.expand(b, 313 * 16));
    count += 313;

    for (int i = 0; i < count; i++)
        assert(a._control[i]);
    assert(!a._control[count]);

    b = a.allocate(64 * 16);
    assert(b.length == 64 * 16);
    count += 64;

    b = a.allocateFresh(16);
    assert(b.length == 16);
    count++;

    for (int i = 0; i < count; i++)
        assert(a._control[i]);
    assert(!a._control[count]);

    assert(a.deallocateAll());
    for (int i = 0; i < a._blocks; i++)
        assert(!a._control[i]);

    b = a.allocateFresh(257 * 16);
    assert(b.length == 257 * 16);
    for (int i = 0; i < count; i++)
        assert(!a._control[i]);
    for (int i = count; i < count + 257; i++)
        assert(a._control[i]);
    count += 257;
    assert(!a._control[count]);

    while (true)
    {
        b = a.allocate(16);
        if (!b)
            break;
        assert(b.length == 16);
    }

    assert(!a.allocateFresh(16));
    assert(a.deallocateAll());

    assert(a.allocate(16).length == 16);
    assert(!a.allocateFresh(16));
}


@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.random;

    static void testAlloc(Allocator)()
    {
        auto numBlocks = [1, 64, 256];
        enum blocks = 10000;
        int iter = 0;

        ubyte[] payload = cast(ubyte[]) Mallocator.instance.allocate(blocks * 16);
        auto a = Allocator(payload);
        void[][] buf = cast(void[][]) Mallocator.instance.allocate((void[]).sizeof * blocks);

        auto rnd = Random();
        while (iter < blocks)
        {
            int event = uniform(0, 2, rnd);
            int doExpand = uniform(0, 2, rnd);
            int allocSize = numBlocks[uniform(0, 3, rnd)] * 16;
            int expandSize = numBlocks[uniform(0, 3, rnd)] * 16;
            int doDeallocate = uniform(0, 2, rnd);

            if (event) buf[iter] = a.allocate(allocSize);
            else buf[iter] = a.allocateFresh(allocSize);

            if (!buf[iter])
                break;
            assert(buf[iter].length == allocSize);

            auto oldSize = buf[iter].length;
            if (doExpand && a.expand(buf[iter], expandSize))
                assert(buf[iter].length == expandSize + oldSize);

            if (doDeallocate)
            {
                assert(a.deallocate(buf[iter]));
                buf[iter] = null;
            }

            iter++;
        }

        while (iter < blocks)
        {
            buf[iter++] = a.allocate(16);
            if (!buf[iter - 1])
                break;
            assert(buf[iter - 1].length == 16);
        }

        for (size_t i = 0; i < a._blocks; i++)
            assert((cast(BitVector) a._control)[i]);

        assert(!a.allocate(16));
        for (size_t i = 0; i < iter; i++)
        {
            if (buf[i])
                assert(a.deallocate(buf[i]));
        }

        for (size_t i = 0; i < a._blocks; i++)
            assert(!(cast(BitVector) a._control)[i]);
    }

    testAlloc!(BitmappedBlock!(16, 16))();
    testAlloc!(SharedBitmappedBlock!(16, 16))();
}

// Test totalAllocation and goodAllocSize
nothrow @safe @nogc unittest
{
    BitmappedBlock!(8, 8, NullAllocator) h1;
    assert(h1.goodAllocSize(1) == 8);
    assert(h1.totalAllocation(1) >= 8);
    assert(h1.totalAllocation(64) >= 64);
    assert(h1.totalAllocation(8 * 64) >= 8 * 64);
    assert(h1.totalAllocation(8 * 63) >= 8 * 63);
    assert(h1.totalAllocation(8 * 64 + 1) >= 8 * 65);

    BitmappedBlock!(64, 8, NullAllocator) h2;
    assert(h2.goodAllocSize(1) == 64);
    assert(h2.totalAllocation(1) >= 64);
    assert(h2.totalAllocation(64 * 64) >= 64 * 64);

    BitmappedBlock!(4096, 4096, NullAllocator) h3;
    assert(h3.goodAllocSize(1) == 4096);
    assert(h3.totalAllocation(1) >= 4096);
    assert(h3.totalAllocation(64 * 4096) >= 64 * 4096);
    assert(h3.totalAllocation(64 * 4096 + 1) >= 65 * 4096);
}

// Test owns
@system unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    import std.typecons : Ternary;

    auto a = BitmappedBlock!(64, 8, GCAllocator)(1024 * 64);
    const void[] buff = (() pure nothrow @safe @nogc => a.allocate(42))();

    assert((() nothrow @safe @nogc => a.owns(buff))() == Ternary.yes);
    assert((() nothrow @safe @nogc => a.owns(null))() == Ternary.no);
}

// BitmappedBlockWithInternalPointers
/**

A `BitmappedBlock` with additional structure for supporting `resolveInternalPointer`.
To that end, `BitmappedBlockWithInternalPointers` adds a
bitmap (one bit per block) that marks object starts. The bitmap itself has
variable size and is allocated together with regular allocations.

The time complexity of `resolveInternalPointer` is $(BIGOH k), where `k`
is the size of the object within which the internal pointer is looked up.

*/
struct BitmappedBlockWithInternalPointers(
    size_t theBlockSize, uint theAlignment = platformAlignment,
    ParentAllocator = NullAllocator)
{
    import std.conv : text;
    import std.typecons : Ternary;

    static if (!stateSize!ParentAllocator)
    version (StdUnittest)
    @system unittest
    {
        import std.experimental.allocator.mallocator : AlignedMallocator;
        auto m = cast(ubyte[])(AlignedMallocator.instance.alignedAllocate(1024 * 64,
            theAlignment));
        scope(exit) () nothrow @nogc { AlignedMallocator.instance.deallocate(m); }();
        testAllocator!(() => BitmappedBlockWithInternalPointers(m));
    }

    // state {
    private BitmappedBlock!(theBlockSize, theAlignment, ParentAllocator) _heap;
    private BitVector _allocStart;
    // }

    /**
    Constructors accepting desired capacity or a preallocated buffer, similar
    in semantics to those of `BitmappedBlock`.
    */
    static if (!stateSize!ParentAllocator)
    this(ubyte[] data)
    {
        _heap = BitmappedBlock!(theBlockSize, theAlignment, ParentAllocator)(data);
    }

    static if (stateSize!ParentAllocator)
    this(ParentAllocator parent, ubyte[] data)
    {
        _heap = BitmappedBlock!(theBlockSize, theAlignment, ParentAllocator)(data);
        _heap.parent = parent;
    }

    /// Ditto
    static if (!is(ParentAllocator == NullAllocator) && !stateSize!ParentAllocator)
    this(size_t capacity)
    {
        // Add room for the _allocStart vector
        _heap = BitmappedBlock!(theBlockSize, theAlignment, ParentAllocator)
            (capacity + capacity.divideRoundUp(64));
    }

    /// Ditto
    static if (!is(ParentAllocator == NullAllocator) && stateSize!ParentAllocator)
    this(ParentAllocator parent, size_t capacity)
    {
        // Add room for the _allocStart vector
        _heap = BitmappedBlock!(theBlockSize, theAlignment, ParentAllocator)
            (parent, capacity + capacity.divideRoundUp(64));
    }

    // Makes sure there's enough room for _allocStart
    @safe
    private bool ensureRoomForAllocStart(size_t len)
    {
        if (_allocStart.length >= len) return true;
        // Must ensure there's room
        immutable oldLength = _allocStart.rep.length;
        immutable bits = len.roundUpToMultipleOf(64);
        void[] b = _allocStart.rep;
        if ((() @trusted => !_heap.reallocate(b, bits / 8))()) return false;
        assert(b.length * 8 == bits);
        _allocStart = BitVector((() @trusted => cast(ulong[]) b)());
        assert(_allocStart.rep.length * 64 == bits);
        _allocStart.rep[oldLength .. $] = ulong.max;
        return true;
    }

    /**
    Allocator primitives.
    */
    alias alignment = theAlignment;

    /// Ditto
    pure nothrow @safe @nogc
    size_t goodAllocSize(size_t n)
    {
        return n.roundUpToMultipleOf(_heap.blockSize);
    }

    /// Ditto
    void[] allocate(size_t bytes)
    {
        auto r = _heap.allocate(bytes);
        if (!r.ptr) return r;
        immutable block = (() @trusted => (r.ptr - _heap._payload.ptr) / _heap.blockSize)();
        immutable blocks =
            (r.length + _heap.blockSize - 1) / _heap.blockSize;
        if (!ensureRoomForAllocStart(block + blocks))
        {
            // Failed, free r and bailout
            () @trusted { _heap.deallocate(r); r = null; }();
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
        immutable block = (() @trusted => (b.ptr - _heap._payload.ptr) / _heap.blockSize)();
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
    nothrow @safe @nogc
    Ternary resolveInternalPointer(const void* p, ref void[] result)
    {
        if ((() @trusted => _heap._payload
                    && (p < &_heap._payload[0]
                        || p >= &_heap._payload[0] + _heap._payload.length))())
        {
            return Ternary.no;
        }
        // Find block start
        auto block = (() @trusted => (p - &_heap._payload[0]) / _heap.blockSize)();
        if (block >= _allocStart.length) return Ternary.no;
        // Within an allocation, must find the 1 just to the left of it
        auto i = _allocStart.find1Backward(block);
        if (i == i.max) return Ternary.no;
        auto j = _allocStart.find1(i + 1);
        result = (() @trusted => _heap._payload.ptr[cast(size_t) (_heap.blockSize * i)
                                                    .. cast(size_t) (_heap.blockSize * j)])();
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
    assert((() nothrow @safe @nogc => h.empty)() == Ternary.yes);
    auto b = (() pure nothrow @safe @nogc => h.allocate(123))();
    assert(b.length == 123);
    assert((() nothrow @safe @nogc => h.empty)() == Ternary.no);

    void[] p;
    void* offset = &b[0] + 17;
    assert((() nothrow @safe @nogc => h.resolveInternalPointer(offset, p))() == Ternary.yes);
    assert(p.ptr is b.ptr);
    assert(p.length >= b.length);
    b = (() pure nothrow @safe @nogc => h.allocate(4096))();

    offset = &b[0];
    assert((() nothrow @safe @nogc => h.resolveInternalPointer(offset, p))() == Ternary.yes);
    assert(p is b);

    offset = &b[0] + 11;
    assert((() nothrow @safe @nogc => h.resolveInternalPointer(offset, p))() == Ternary.yes);
    assert(p is b);

    void[] unchanged = p;
    offset = &b[0] - 40_970;
    assert((() nothrow @safe @nogc => h.resolveInternalPointer(offset, p))() == Ternary.no);
    assert(p is unchanged);

    assert((() @safe => h.expand(b, 1))());
    assert(b.length == 4097);
    offset = &b[0] + 4096;
    assert((() nothrow @safe @nogc => h.resolveInternalPointer(offset, p))() == Ternary.yes);
    assert(p.ptr is b.ptr);

    // Ensure deallocate inherits from parent
    () nothrow @nogc { h.deallocate(b); }();
}

@system unittest
{
    auto h = BitmappedBlockWithInternalPointers!(4096)(new ubyte[4096 * 1024]);
    assert((() pure nothrow @safe @nogc => h.goodAllocSize(1))() == 4096);
}

// Test instantiation with stateful allocators
@system unittest
{
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.building_blocks.region : Region;
    auto r = Region!Mallocator(1024 * 1024);
    auto h = BitmappedBlockWithInternalPointers!(4096, 8, Region!Mallocator*)(&r, 4096 * 1024);
}

/**
Returns the number of most significant ones before a zero can be found in `x`.
If `x` contains no zeros (i.e. is equal to `ulong.max`), returns 64.
*/
pure nothrow @safe @nogc
private uint leadingOnes(ulong x)
{
    import core.bitop : bsr;
    const x_ = ~x;
    return x_ == 0 ? 64 : (63 - bsr(x_));
}

@safe unittest
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
Finds a run of contiguous ones in `x` of length at least `n`.
*/
pure nothrow @safe @nogc
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

@safe unittest
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
pure nothrow @safe @nogc
private void setBits(ref ulong w, uint lsb, uint msb)
{
    assert(lsb <= msb && msb < 64);
    const mask = (ulong.max << lsb) & (ulong.max >> (63 - msb));
    w |= mask;
}

@safe unittest
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
pure nothrow @safe @nogc
private bool setBitsIfZero(ref ulong w, uint lsb, uint msb)
{
    assert(lsb <= msb && msb < 64);
    const mask = (ulong.max << lsb) & (ulong.max >> (63 - msb));
    if (w & mask) return false;
    w |= mask;
    return true;
}

// Assigns bits in w from lsb through msb to zero.
pure nothrow @safe @nogc
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

    auto rep(this _)() { return _rep; }

    pure nothrow @safe @nogc
    this(ulong[] data) { _rep = data; }

    pure nothrow @safe @nogc
    void opSliceAssign(bool b) { _rep[] = b ? ulong.max : 0; }

    pure nothrow @safe @nogc
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
            _rep[i1 + 1 .. i2] = (b ? ulong.max : 0);
            if (b) setBits(_rep[i2], b2, 63);
            else resetBits(_rep[i2], b2, 63);
        }
    }

    pure nothrow @safe @nogc
    bool opIndex(ulong x)
    {
        assert(x < length);
        return (_rep[cast(size_t) (x / 64)]
            & (0x8000_0000_0000_0000UL >> (x % 64))) != 0;
    }

    pure nothrow @safe @nogc
    void opIndexAssign(bool b, ulong x)
    {
        assert(x / 64 <= size_t.max);
        immutable i = cast(size_t) (x / 64);
        immutable j = 0x8000_0000_0000_0000UL >> (x % 64);
        if (b) _rep[i] |= j;
        else _rep[i] &= ~j;
    }

    pure nothrow @safe @nogc
    ulong length() const
    {
        return _rep.length * 64;
    }

    /* Returns the index of the first 1 to the right of i (including i itself),
    or length if not found.
    */
    pure nothrow @safe @nogc
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
    pure nothrow @safe @nogc
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
    pure nothrow @safe @nogc
    bool allAre0() const
    {
        foreach (w; _rep) if (w) return false;
        return true;
    }

    /// Are all bits one?
    pure nothrow @safe @nogc
    bool allAre1() const
    {
        foreach (w; _rep) if (w != ulong.max) return false;
        return true;
    }

    pure nothrow @safe @nogc
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

@safe unittest
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

/**
 Functions to manipulate metadata in-block.

 functionality was moved from rt.lifetime
 */
module core.internal.gc.blockmeta;

import core.memory;

alias BlkInfo = GC.BlkInfo;
alias BlkAttr = GC.BlkAttr;

enum : size_t
{
    PAGESIZE = 4096,
    BIGLENGTHMASK = ~(PAGESIZE - 1),
    SMALLPAD = 1,
    MEDPAD = ushort.sizeof,
    LARGEPREFIX = 16, // 16 bytes padding at the front of the array
    LARGEPAD = LARGEPREFIX + 1,
    MAXSMALLSIZE = 256-SMALLPAD,
    MAXMEDSIZE = (PAGESIZE / 2) - MEDPAD
}

// size used to store the TypeInfo at the end of an allocation for structs that have a destructor
size_t structTypeInfoSize(const TypeInfo ti) pure nothrow @nogc
{
    if (ti && typeid(ti) is typeid(TypeInfo_Struct)) // avoid a complete dynamic type cast
    {
        auto sti = cast(TypeInfo_Struct)cast(void*)ti;
        if (sti.xdtor)
            return size_t.sizeof;
    }
    return 0;
}

/**
  Set the allocated length of the array block.  This is called
  any time an array is appended to or its length is set.

  The allocated block looks like this for blocks < PAGESIZE:

  |elem0|elem1|elem2|...|elemN-1|emptyspace|N*elemsize|


  The size of the allocated length at the end depends on the block size:

  a block of 16 to 256 bytes has an 8-bit length.

  a block with 512 to pagesize/2 bytes has a 16-bit length.

  For blocks >= pagesize, the length is a size_t and is at the beginning of the
  block.  The reason we have to do this is because the block can extend into
  more pages, so we cannot trust the block length if it sits at the end of the
  block, because it might have just been extended.  If we can prove in the
  future that the block is unshared, we may be able to change this, but I'm not
  sure it's important.

  In order to do put the length at the front, we have to provide 16 bytes
  buffer space in case the block has to be aligned properly.  In x86, certain
  SSE instructions will only work if the data is 16-byte aligned.  In addition,
  we need the sentinel byte to prevent accidental pointers to the next block.
  Because of the extra overhead, we only do this for page size and above, where
  the overhead is minimal compared to the block size.

  So for those blocks, it looks like:

  |N*elemsize|padding|elem0|elem1|...|elemN-1|emptyspace|sentinelbyte|

  where elem0 starts 16 bytes after the first byte.
  */
bool __setArrayAllocLength(ref BlkInfo info, size_t newlength, bool isshared, const TypeInfo tinext, size_t oldlength = size_t.max) pure nothrow
{
    __setBlockFinalizerInfo(info, tinext);

    size_t typeInfoSize = (info.attr & BlkAttr.STRUCTFINAL) ? size_t.sizeof : 0;
    return __setArrayAllocLengthImpl(info, newlength, isshared, oldlength, typeInfoSize);
}

// the impl function, used both above and in core.internal.array.utils
bool __setArrayAllocLengthImpl(ref BlkInfo info, size_t newlength, bool isshared, size_t oldlength, size_t typeInfoSize) pure nothrow
{
    import core.atomic;

    if (info.size <= 256)
    {
        import core.checkedint;

        bool overflow;
        auto newlength_padded = addu(newlength,
                                     addu(SMALLPAD, typeInfoSize, overflow),
                                     overflow);

        if (newlength_padded > info.size || overflow)
            // new size does not fit inside block
            return false;

        auto length = cast(ubyte *)(info.base + info.size - typeInfoSize - SMALLPAD);
        if (oldlength != size_t.max)
        {
            if (isshared)
            {
                return cas(cast(shared)length, cast(ubyte)oldlength, cast(ubyte)newlength);
            }
            else
            {
                if (*length == cast(ubyte)oldlength)
                    *length = cast(ubyte)newlength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = cast(ubyte)newlength;
        }
    }
    else if (info.size < PAGESIZE)
    {
        if (newlength + MEDPAD + typeInfoSize > info.size)
            // new size does not fit inside block
            return false;
        auto length = cast(ushort *)(info.base + info.size - typeInfoSize - MEDPAD);
        if (oldlength != size_t.max)
        {
            if (isshared)
            {
                return cas(cast(shared)length, cast(ushort)oldlength, cast(ushort)newlength);
            }
            else
            {
                if (*length == oldlength)
                    *length = cast(ushort)newlength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = cast(ushort)newlength;
        }
    }
    else
    {
        if (newlength + LARGEPAD > info.size)
            // new size does not fit inside block
            return false;
        auto length = cast(size_t *)(info.base);
        if (oldlength != size_t.max)
        {
            if (isshared)
            {
                return cas(cast(shared)length, cast(size_t)oldlength, cast(size_t)newlength);
            }
            else
            {
                if (*length == oldlength)
                    *length = newlength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = newlength;
        }
    }
    return true; // resize succeeded
}

/**
  The block finalizer info is set separately from the array length, as that is
  only needed on the initial setup of the block. No shared is needed, since
  this should only happen when the block is new.
  If the STRUCTFINAL bit is not set, no finalizer is stored (but if needed the
  slot is zeroed)
  */
void __setBlockFinalizerInfo(ref BlkInfo info, const TypeInfo ti) pure nothrow
{
    if ((info.attr & BlkAttr.APPENDABLE) && info.size >= PAGESIZE)
    {
        // if the structfinal bit is not set, we don't have a finalizer. But we
        // should still zero out the finalizer slot.
        auto context = (info.attr & BlkAttr.STRUCTFINAL) ? cast(void*)ti : null;

        // array used size goes at the beginning. We can stuff the typeinfo
        // right after it, as we need to use 16 bytes anyway.
        //
        auto typeInfo = cast(void**)info.base + 1;
        *typeInfo = context;
        version (D_LP64) {} else
        {
            // zero out the extra padding
            (cast(size_t*)info.base)[2 .. 4] = 0;
        }
    }
    else if(info.attr & BlkAttr.STRUCTFINAL)
    {
        // all other cases the typeinfo gets put at the end of the block
        auto typeInfo = cast(void**)(info.base + info.size) - 1;
        *typeInfo = cast(void*) ti;
    }
}

/**
  Get the finalizer info from the block (typeinfo).
  Must be called on a block with STRUCTFINAL set.
  */
const(TypeInfo) __getBlockFinalizerInfo(ref BlkInfo info) pure nothrow
{
    bool isLargeArray = (info.attr & BlkAttr.APPENDABLE) && info.size >= PAGESIZE;
    auto typeInfo = isLargeArray ?
        info.base + size_t.sizeof :
        info.base + info.size - size_t.sizeof;
    return *cast(TypeInfo*)typeInfo;
}

/**
  get the used size of the array for the given block
  */
size_t __arrayAllocLength(ref BlkInfo info) pure nothrow
    in(info.attr & BlkAttr.APPENDABLE)
{
    auto typeInfoSize = (info.attr & BlkAttr.STRUCTFINAL) ? size_t.sizeof : 0;
    if (info.size <= 256)
        return *cast(ubyte *)(info.base + info.size - typeInfoSize - SMALLPAD);

    if (info.size < PAGESIZE)
        return *cast(ushort *)(info.base + info.size - typeInfoSize - MEDPAD);

    return *cast(size_t *)(info.base);
}

/**
  Atomically get the used size of the array for the given block
  */
size_t __arrayAllocLengthAtomic(ref BlkInfo info) pure nothrow
    in(info.attr & BlkAttr.APPENDABLE)
{
    import core.atomic;
    auto typeInfoSize = (info.attr & BlkAttr.STRUCTFINAL) ? size_t.sizeof : 0;
    if (info.size <= 256)
        return atomicLoad(*cast(shared(ubyte)*)(info.base + info.size - typeInfoSize - SMALLPAD));

    if (info.size < PAGESIZE)
        return atomicLoad(*cast(shared(ushort)*)(info.base + info.size - typeInfoSize - MEDPAD));

    return atomicLoad(*cast(shared(size_t)*)(info.base));
}

/**
  Get the maximum bytes that can be stored in the given block.
  */
size_t __arrayAllocCapacity(ref BlkInfo info) pure nothrow
    in(info.attr & BlkAttr.APPENDABLE)
{
    // Capacity is a calculation based solely on the block info.
    if (info.size >= PAGESIZE)
        return info.size - LARGEPAD;

    auto typeInfoSize = (info.attr & BlkAttr.STRUCTFINAL) ? size_t.sizeof : 0;
    auto padsize = info.size <= 256 ? SMALLPAD : MEDPAD;
    return info.size - typeInfoSize - padsize;
}

/**
  get the padding required to allocate size bytes.  Note that the padding is
  NOT included in the passed in size.  Therefore, do NOT call this function
  with the size of an allocated block.
  */
size_t __arrayPad(size_t size, const TypeInfo tinext) nothrow pure @trusted
{
    return size > MAXMEDSIZE ? LARGEPAD : ((size > MAXSMALLSIZE ? MEDPAD : SMALLPAD) + structTypeInfoSize(tinext));
}

/**
  get the padding required to allocate size bytes, use the bits to determine
  which metadata must be stored.
  */
size_t __allocPad(size_t size, uint bits) nothrow pure @trusted
{
    auto finalizerSize = (bits & BlkAttr.STRUCTFINAL) ? (void*).sizeof : 0;
    if (bits & BlkAttr.APPENDABLE)
    {
        if (size > MAXMEDSIZE - finalizerSize)
            return LARGEPAD;
        auto pad = (size > MAXSMALLSIZE - finalizerSize) ? MEDPAD : SMALLPAD;
        return pad + finalizerSize;
    }

    return finalizerSize;
}

/**
 * Get the start of the array for the given block.
 *
 * Params:
 *  info = array metadata
 * Returns:
 *  pointer to the start of the array
 */
void *__arrayStart()(return scope BlkInfo info) nothrow pure
{
    return info.base + ((info.size & BIGLENGTHMASK) ? LARGEPREFIX : 0);
}

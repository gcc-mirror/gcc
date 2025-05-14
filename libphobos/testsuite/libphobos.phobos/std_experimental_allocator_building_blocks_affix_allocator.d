@system unittest
{
    import std.experimental.allocator.building_blocks.affix_allocator;

    import std.experimental.allocator.mallocator : Mallocator;
    // One word before and after each allocation.
    alias A = AffixAllocator!(Mallocator, size_t, size_t);
    auto b = A.instance.allocate(11);
    A.instance.prefix(b) = 0xCAFE_BABE;
    A.instance.suffix(b) = 0xDEAD_BEEF;
    assert(A.instance.prefix(b) == 0xCAFE_BABE
        && A.instance.suffix(b) == 0xDEAD_BEEF);
}


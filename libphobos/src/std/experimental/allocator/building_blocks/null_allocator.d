// Written in the D programming language.
/**
Source: $(PHOBOSSRC std/experimental/allocator/building_blocks/null_allocator.d)
*/
module std.experimental.allocator.building_blocks.null_allocator;

/**
`NullAllocator` is an emphatically empty implementation of the allocator
interface. Although it has no direct use, it is useful as a "terminator" in
composite allocators.
*/
struct NullAllocator
{
    import std.typecons : Ternary;

    nothrow @nogc pure @safe:
    /**
    `NullAllocator` advertises a relatively large _alignment equal to 64 KB.
    This is because `NullAllocator` never actually needs to honor this
    alignment and because composite allocators using `NullAllocator`
    shouldn't be unnecessarily constrained.
    */
    enum uint alignment = 64 * 1024;
    // /// Returns `n`.
    //size_t goodAllocSize(size_t n) shared const
    //{ return .goodAllocSize(this, n); }
    /// Always returns `null`.
    void[] allocate(size_t) shared { return null; }
    /// Always returns `null`.
    void[] alignedAllocate(size_t, uint) shared { return null; }
    /// Always returns `null`.
    void[] allocateAll() shared { return null; }
    /**
    These methods return `false`.
    Precondition: $(D b is null). This is because there is no other possible
    legitimate input.
    */
    bool expand(ref void[] b, size_t s) shared
    { assert(b is null); return s == 0; }
    /// Ditto
    bool reallocate(ref void[] b, size_t) shared
    { assert(b is null); return false; }
    /// Ditto
    bool alignedReallocate(ref void[] b, size_t, uint) shared
    { assert(b is null); return false; }
    /// Returns `Ternary.no`.
    Ternary owns(const void[]) shared const { return Ternary.no; }
    /**
    Returns `Ternary.no`.
    */
    Ternary resolveInternalPointer(const void*, ref void[]) shared const
    { return Ternary.no; }
    /**
    No-op.
    Precondition: $(D b is null)
    */
    bool deallocate(void[] b) shared { assert(b is null); return true; }
    /**
    No-op.
    */
    bool deallocateAll() shared { return true; }
    /**
    Returns `Ternary.yes`.
    */
    Ternary empty() shared const { return Ternary.yes; }
    /**
    Returns the `shared` global instance of the `NullAllocator`.
    */
    static shared NullAllocator instance;
}

nothrow @nogc pure @safe unittest
{
    alias a = NullAllocator.instance;

    assert(a.alignedAllocate(100, 0) is null);
    assert(a.allocateAll() is null);
    auto b = a.allocate(100);
    assert(b is null);
    assert(a.expand(b, 0));
    assert(!a.expand(b, 42));
    assert(!a.reallocate(b, 42));
    assert(!a.alignedReallocate(b, 42, 0));
    assert(a.deallocate(b));
    assert(a.deallocateAll());

    import std.typecons : Ternary;
    assert(a.empty == Ternary.yes);
    assert(a.owns(null) == Ternary.no);

    void[] p;
    assert(a.resolveInternalPointer(null, p) == Ternary.no);
}

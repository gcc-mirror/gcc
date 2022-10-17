import core.memory;

// TODO: The following should work, but L10 (second assert) fails.
version(none) void dotest(T) (T* ptr)
{
    GC.clrAttr(ptr, uint.max);
    assert(GC.getAttr(ptr) == 0);

    GC.setAttr(ptr, GC.BlkAttr.NO_MOVE);
    assert(GC.getAttr(ptr) == GC.BlkAttr.NO_MOVE);

    GC.clrAttr(ptr, GC.BlkAttr.NO_MOVE);
    assert(GC.getAttr(ptr) == 0);
    GC.clrAttr(ptr, GC.BlkAttr.NO_MOVE);
    assert(GC.getAttr(ptr) == 0);
}
else void dotest(T) (T* ptr)
{
    // https://issues.dlang.org/show_bug.cgi?id=21484
    GC.clrAttr(ptr, uint.max);
    GC.setAttr(ptr, GC.BlkAttr.NO_MOVE);
    GC.getAttr(ptr);
}

void main ()
{
    auto ptr = new int;
    dotest!(const(int))(ptr);
    dotest!(int)(ptr);
}

import core.memory;
void main()
{
    auto collections = GC.profileStats().numCollections;
    // loop until we trigger a collection
    for (;;)
    {
        cast(void)GC.malloc(100_000, GC.BlkAttr.NO_SCAN);
        if (GC.profileStats().numCollections == collections+1)
            break;
    }
}

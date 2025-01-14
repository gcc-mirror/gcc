import core.gc.registry;
import core.gc.gcinterface;
import core.stdc.stdlib;

static import core.memory;

extern (C) __gshared string[] rt_options = ["gcopt=gc:malloc"];

extern (C) pragma(crt_constructor) void register_mygc()
{
    registerGCFactory("malloc", &MallocGC.initialize);
}

extern (C) void register_default_gcs()
{
    // remove default GCs
}

/** Simple GC that requires any pointers passed to it's API
    to point to start of the allocation.
 */
class MallocGC : GC
{
nothrow @nogc:
    // To make sure all allocations are multiples of 8 bytes for alignment
    private size_t alignUp(size_t size)
    {
        return (size + 7) & ~7LU;
    }

    static GC initialize()
    {
        import core.stdc.string : memcpy;

        __gshared align(__traits(classInstanceAlignment, MallocGC))
            ubyte[__traits(classInstanceSize, MallocGC)] buf;

        auto init = typeid(MallocGC).initializer();
        assert(init.length == buf.length);
        auto instance = cast(MallocGC) memcpy(buf.ptr, init.ptr, init.length);
        instance.__ctor();
        return instance;
    }

    this()
    {
    }

    void Dtor()
    {
    }

    void enable()
    {
    }

    void disable()
    {
    }

    void collect() nothrow
    {
    }

    void collectNoStack() nothrow
    {
    }

    void minimize() nothrow
    {
    }

    uint getAttr(void* p) nothrow
    {
        return 0;
    }

    uint setAttr(void* p, uint mask) nothrow
    {
        return mask;
    }

    uint clrAttr(void* p, uint mask) nothrow
    {
        return mask;
    }

    void* malloc(size_t size, uint bits, const TypeInfo ti) nothrow
    {
        size = alignUp(size);
        return sentinelAdd(.malloc(size + sentinelSize), size);
    }

    BlkInfo qalloc(size_t size, uint bits, const scope TypeInfo ti) nothrow
    {
        size = alignUp(size);
        return BlkInfo(malloc(size, bits, ti), size);
    }

    void* calloc(size_t size, uint bits, const TypeInfo ti) nothrow
    {
        size = alignUp(size);
        return sentinelAdd(.calloc(1, size + sentinelSize), size);
    }

    void* realloc(void* p, size_t size, uint bits, const TypeInfo ti) nothrow
    {
        size = alignUp(size);
        return sentinelAdd(.realloc(p - sentinelSize, size + sentinelSize), size);
    }

    size_t extend(void* p, size_t minsize, size_t maxsize, const TypeInfo ti) nothrow
    {
        return 0;
    }

    size_t reserve(size_t size) nothrow
    {
        return 0;
    }

    void free(void* p) nothrow
    {
        free(p - sentinelSize);
    }

    void* addrOf(void* p) nothrow
    {
        return p;
    }

    size_t sizeOf(void* p) nothrow
    {
        return query(p).size;
    }

    BlkInfo query(void* p) nothrow
    {
        return p ? BlkInfo(p, sentinelGet(p)) : BlkInfo.init;
    }

    core.memory.GC.Stats stats() nothrow
    {
        return core.memory.GC.Stats.init;
    }

    core.memory.GC.ProfileStats profileStats() nothrow
    {
        return typeof(return).init;
    }

    void addRoot(void* p) nothrow @nogc
    {
    }

    void removeRoot(void* p) nothrow @nogc
    {
    }

    @property RootIterator rootIter() @nogc
    {
        return null;
    }

    void addRange(void* p, size_t sz, const TypeInfo ti) nothrow @nogc
    {
    }

    void removeRange(void* p) nothrow @nogc
    {
    }

    @property RangeIterator rangeIter() @nogc
    {
        return null;
    }

    void runFinalizers(const scope void[] segment) nothrow
    {
    }

    bool inFinalizer() nothrow
    {
        return false;
    }

    ulong allocatedInCurrentThread() nothrow
    {
        return stats().allocatedInCurrentThread;
    }

    void[] getArrayUsed(void *ptr, bool atomic = false) nothrow
    {
        return null;
    }

    bool expandArrayUsed(void[] slice, size_t newUsed, bool atomic = false) nothrow @safe
    {
        return false;
    }

    size_t reserveArrayCapacity(void[] slice, size_t request, bool atomic = false) nothrow @safe
    {
        return 0;
    }

    bool shrinkArrayUsed(void[] slice, size_t existingUsed, bool atomic = false) nothrow
    {
        return false;
    }

private:
    // doesn't care for alignment
    static void* sentinelAdd(void* p, size_t value)
    {
        *cast(size_t*) p = value;
        return p + sentinelSize;
    }

    static size_t sentinelGet(void* p)
    {
        return *cast(size_t*)(p - sentinelSize);
    }

    enum sentinelSize = size_t.sizeof;
}

void main()
{
    // test array append cache
    char[] s;
    foreach (char c; char.min .. char.max + 1)
        s ~= c;
}

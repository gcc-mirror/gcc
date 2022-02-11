// https://issues.dlang.org/show_bug.cgi?id=20438
import core.stdc.stdio;
import core.memory;

void main()
{
    auto used0 = GC.stats.usedSize;
    void* z = GC.malloc(100);
    GC.free(z);
    GC.collect();
    auto used1 = GC.stats.usedSize;
    used1 <= used0 || assert(false);
}

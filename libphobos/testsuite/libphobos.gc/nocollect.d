// https://issues.dlang.org/show_bug.cgi?id=20567

import core.memory;

void main()
{
    auto stats = GC.profileStats();
    assert(stats.numCollections == 0);

    char[] sbuf = new char[256];  // small pool
    char[] lbuf = new char[2049]; // large pool

    stats = GC.profileStats();
    assert(stats.numCollections == 0);
}

// Issue https://issues.dlang.org/show_bug.cgi?id=18996
// Array!string calls removeRange without first adding the range, but never
// initializes the GC. The behavior of the default GC is to ignore removing
// ranges when the range wasn't added. The ProtoGC originally would crash when
// this happened.

import core.memory;

void main()
{
    GC.removeRange(null);
    GC.removeRoot(null);
}

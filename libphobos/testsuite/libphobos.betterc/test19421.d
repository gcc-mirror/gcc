/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=19421

import core.memory;

extern(C) void main() @nogc nothrow pure
{
    auto p = pureMalloc(1);
    p = pureRealloc(p, 2);
    if (p) pureFree(p);
    p = pureCalloc(1, 1);
    if (p) pureFree(p);
}

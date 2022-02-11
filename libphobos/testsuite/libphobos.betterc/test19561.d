/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=19561

import core.memory;

extern(C) void main() @nogc nothrow pure
{
    int[3] a, b;
    a[] = 0;
    a[] = b[];
    //FIXME: Next line requires compiler change.
    //a[] = 1; // error: undefined reference to '_memset32'
    a[] += 1;
    a[] += b[];
    int[3] c = a[] + b[];
}

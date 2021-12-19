// https://issues.dlang.org/show_bug.cgi?id=22210

import core.internal.traits : allSatisfy;

enum isHashable(T) = __traits(compiles,
    () { T.init; }
);

class A
{
    static if (isHashable!B) {}
}

class B
{
    static if (isHashable!C) {}
}

class C
{
    static if (allSatisfy!(isHashable, int, B)) {}
}

void main() {}

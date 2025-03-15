// precise GC related:
// https://issues.dlang.org/show_bug.cgi?id=3463
// https://issues.dlang.org/show_bug.cgi?id=4358
// https://issues.dlang.org/show_bug.cgi?id=9094
// https://issues.dlang.org/show_bug.cgi?id=13801
// https://issues.dlang.org/show_bug.cgi?id=18900
module testgc;

import core.memory;
import core.stdc.stdio : printf;

class C
{
    __gshared int dtors;
    ~this() { dtors++; }

    C next;
    size_t val;
}

struct S
{
    __gshared int dtors;
    ~this() { dtors++; }

    size_t val;
    S* next;
}

struct L
{
    __gshared int dtors;
    ~this() { dtors++; }

    size_t[1000] data;
    S* node;
}

struct Roots
{
    C c;
    S *s;
    L *l;
};

Roots* roots;
size_t iroots;

void init()
{
    roots = new Roots;
    roots.c = new C;
    roots.c.next = new C;

    roots.s = new S;
    roots.s.next = new S;

    roots.l = new L;
    roots.l.node = new S;
}

void verifyPointers()
{
    assert(C.dtors == 0);
    assert(S.dtors == 0);
    assert(L.dtors == 0);
}

// compiling with -gx should help eliminating false pointers on the stack
Roots makeFalsePointers()
{
    roots.c.val = cast(size_t) cast(void*) roots.c.next;
    roots.c.next = null;
    roots.s.val = cast(size_t) cast(void*) roots.s.next;
    roots.s.next = null;
    roots.l.data[7] = cast(size_t) cast(void*) roots.l.node;
    roots.l.node = null;

    return Roots(null, null, null); // try to spill register contents
}

Roots moveRoot()
{
    iroots = cast(size_t)roots;
    roots = null;

    return Roots(null, null, null); // try to spill register contents
}

// compiling with -gx should help eliminating false pointers on the stack
void verifyFalsePointers()
{
    assert(C.dtors <= 1);
    if (C.dtors < 1) printf ("False pointers? C.dtors = %d, 1 expected\n", C.dtors);
    assert(S.dtors <= 2);
    if (S.dtors < 2) printf ("False pointers? S.dtors = %d, 2 expected\n", S.dtors);
    assert(L.dtors == 0);
}

extern(C) __gshared string[] rt_options = [ "gcopt=gc:precise", "scanDataSeg=precise" ];

void main()
{
    GC.collect(); // cleanup from unittests

    init();
    GC.collect(); // should collect nothing
    verifyPointers();

    makeFalsePointers();
    GC.collect(); // should collect roots.c.next, roots.s.next and roots.l.node
    verifyFalsePointers();

    moveRoot();
    GC.collect(); // should collect all

    version(Windows) // precise DATA scanning only implemented on Windows
    {
        assert(C.dtors <= 2);
        if (C.dtors < 2) printf ("False DATA pointers? C.dtors = %d, 2 expected\n", C.dtors);
        assert(S.dtors <= 3);
        if (S.dtors < 3) printf ("False DATA pointers? S.dtors = %d, 2 expected\n", S.dtors);
        assert(L.dtors <= 1);
        if (L.dtors < 1) printf ("False DATA pointers? L.dtors = %d, 1 expected\n", L.dtors);
    }
}

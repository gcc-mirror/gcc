// https://issues.dlang.org/show_bug.cgi?id=20178

interface I {}
interface J : I {}
interface K(T) {}
class C1 : I {}
class C2 : C1 {}
class C3 : J {}
class C4(T) : C3, K!T {}
class C5(T) : C4!T {}

void main() @nogc nothrow pure @safe
{
    assert(typeid(C1).isBaseOf(typeid(C1)));
    assert(typeid(C1).isBaseOf(typeid(C2)));

    assert(!typeid(C2).isBaseOf(typeid(C1)));
    assert(typeid(C2).isBaseOf(typeid(C2)));

    assert(!typeid(C1).isBaseOf(typeid(Object)));
    assert(!typeid(C2).isBaseOf(typeid(Object)));
    assert(typeid(Object).isBaseOf(typeid(C1)));
    assert(typeid(Object).isBaseOf(typeid(C2)));

    assert(typeid(I).isBaseOf(typeid(I)));
    assert(typeid(I).isBaseOf(typeid(J)));
    assert(typeid(I).isBaseOf(typeid(C1)));
    assert(typeid(I).isBaseOf(typeid(C2)));
    assert(typeid(I).isBaseOf(typeid(C3)));
    assert(!typeid(I).isBaseOf(typeid(Object)));

    assert(!typeid(J).isBaseOf(typeid(I)));
    assert(typeid(J).isBaseOf(typeid(J)));
    assert(!typeid(J).isBaseOf(typeid(C1)));
    assert(!typeid(J).isBaseOf(typeid(C2)));
    assert(typeid(J).isBaseOf(typeid(C3)));
    assert(!typeid(J).isBaseOf(typeid(Object)));

    assert(typeid(C4!int).isBaseOf(typeid(C5!int)));
    assert(typeid(K!int).isBaseOf(typeid(C5!int)));
    assert(!typeid(C4!Object).isBaseOf(typeid(C5!int)));
    assert(!typeid(K!Object).isBaseOf(typeid(C5!int)));

    static assert(!__traits(compiles, TypeInfo.init.isBaseOf(typeid(C1))));
    static assert(!__traits(compiles, typeid(C1).isBaseOf(TypeInfo.init)));
}

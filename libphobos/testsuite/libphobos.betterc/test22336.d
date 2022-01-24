/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=22336

import core.lifetime;

struct Foo {
    int f = -1;
    @disable this(this);
    this(int x) { f = x; }
    @disable this();
}

extern(C) int main() {
    Foo a = Foo(42);
    Foo b = move(a);
    assert(a.f == -1);
    assert(b.f == 42);
    return 0;
}

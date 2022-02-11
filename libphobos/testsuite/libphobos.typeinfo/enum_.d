// https://issues.dlang.org/show_bug.cgi?id=21441

int dtorCount;
int postblitCount;

struct S
{
    this(this) { ++postblitCount; }
    ~this() { ++dtorCount; }
}

enum E : S { _ = S.init }

void main()
{
    E e;
    typeid(e).destroy(&e);
    assert(dtorCount == 1);
    typeid(e).postblit(&e);
    assert(postblitCount == 1);
}

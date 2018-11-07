// https://github.com/dlang/druntime/pull/1781

struct S
{
    int i;
    static int comparisons;
    int opCmp(const S s) const { comparisons++; return i - s.i; }
}

void testStructs()
{
    auto s1 = S(1);
    auto s2 = S(2);
    auto s3 = S(3);
    auto s4 = S(4);

    // Test lexicographical order

    assert(s1 < s2 && s2 < s3);
    assert([s1, s2, s3] < [s1, s3]);
    assert([s1, s2] < [s1, s2, s3]);

    // Test number of comparisons for nested types

    S.comparisons = 0;
    assert(s1 < s2);
    assert(S.comparisons == 1);

    S.comparisons = 0;
    assert([s1, s2] < [s3, s4]);
    assert(S.comparisons == 1);

    S.comparisons = 0;
    assert([[s1, s2]] < [[s3, s4]]);
    assert(S.comparisons == 1);
}

class C
{
    this(int i) { this.i = i; }
    int i;
    static int comparisons;
    override int opCmp(Object c) const { comparisons++; return i - (cast(C)c).i; }
}

void testClasses()
{
    auto c1 = new C(1);
    auto c2 = new C(2);
    auto c3 = new C(3);
    auto c4 = new C(4);

    // Test lexicographical order

    assert(c1 < c2 && c2 < c3);
    assert([c1, c2, c3] < [c1, c3]);
    assert([c1, c2] < [c1, c2, c3]);

    // Test number of comparisons for nested types

    C.comparisons = 0;
    assert(c1 < c2);
    assert(C.comparisons == 1);

    C.comparisons = 0;
    assert([c1, c2] < [c3, c4]);
    assert(C.comparisons == 1);

    C.comparisons = 0;
    assert([[c1, c2]] < [[c3, c4]]);
    assert(C.comparisons == 1);
}

void main()
{
    testStructs();
    testClasses();
}

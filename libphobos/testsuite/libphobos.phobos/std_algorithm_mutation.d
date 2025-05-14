@safe unittest
{
    import std.algorithm.mutation;

    auto arr = [4, 5, 6, 7, 1, 2, 3];
    auto p = bringToFront(arr[0 .. 4], arr[4 .. $]);
    assert(p == arr.length - 4);
    assert(arr == [ 1, 2, 3, 4, 5, 6, 7 ]);
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.algorithm.comparison : equal;
    import std.container : SList;
    import std.range.primitives : popFrontN;

    auto list = SList!(int)(4, 5, 6, 7, 1, 2, 3);
    auto r1 = list[];
    auto r2 = list[]; popFrontN(r2, 4);
    assert(equal(r2, [ 1, 2, 3 ]));
    bringToFront(r1, r2);
    assert(equal(list[], [ 1, 2, 3, 4, 5, 6, 7 ]));
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.algorithm.comparison : equal;
    import std.container : SList;

    auto list = SList!(int)(4, 5, 6, 7);
    auto vec = [ 1, 2, 3 ];
    bringToFront(list[], vec);
    assert(equal(list[], [ 1, 2, 3, 4 ]));
    assert(equal(vec, [ 5, 6, 7 ]));
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.string : representation;
    auto ar = representation("a".dup);
    auto br = representation("ç".dup);

    bringToFront(ar, br);

    auto a = cast(char[]) ar;
    auto b = cast(char[]) br;

    // Illegal UTF-8
    assert(a == "\303");
    // Illegal UTF-8
    assert(b == "\247a");
}

@safe unittest
{
    import std.algorithm.mutation;

    int[] a = [ 1, 5 ];
    int[] b = [ 9, 8 ];
    int[] buf = new int[](a.length + b.length + 10);
    auto rem = a.copy(buf);    // copy a into buf
    rem = b.copy(rem);         // copy b into remainder of buf
    assert(buf[0 .. a.length + b.length] == [1, 5, 9, 8]);
    assert(rem.length == 10);   // unused slots in buf
}

@safe unittest
{
    import std.algorithm.mutation;

    float[] src = [ 1.0f, 5 ];
    double[] dest = new double[src.length];
    src.copy(dest);
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.range;
    int[] src = [ 1, 5, 8, 9, 10 ];
    auto dest = new int[](3);
    src.take(dest.length).copy(dest);
    assert(dest == [ 1, 5, 8 ]);
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.algorithm.iteration : filter;
    int[] src = [ 1, 5, 8, 9, 10, 1, 2, 0 ];
    auto dest = new int[src.length];
    auto rem = src
        .filter!(a => (a & 1) == 1)
        .copy(dest);
    assert(dest[0 .. $ - rem.length] == [ 1, 5, 9, 1 ]);
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.algorithm, std.range;
    int[] src = [1, 2, 4];
    int[] dest = [0, 0, 0, 0, 0];
    src.retro.copy(dest.retro);
    assert(dest == [0, 0, 1, 2, 4]);
}

@safe unittest
{
    import std.algorithm.mutation;

    int[] a = [ 1, 2, 3, 4 ];
    fill(a, 5);
    assert(a == [ 5, 5, 5, 5 ]);
}

@safe unittest
{
    import std.algorithm.mutation;

    int[] a = [ 1, 2, 3, 4, 5 ];
    int[] b = [ 8, 9 ];
    fill(a, b);
    assert(a == [ 8, 9, 8, 9, 8 ]);
}

@system unittest
{
    import std.algorithm.mutation;

    import core.stdc.stdlib : malloc, free;

    struct S
    {
        int a = 10;
    }

    auto s = (cast(S*) malloc(5 * S.sizeof))[0 .. 5];
    initializeAll(s);
    assert(s == [S(10), S(10), S(10), S(10), S(10)]);

    scope(exit) free(s.ptr);
}

@safe unittest
{
    import std.algorithm.mutation;

    Object obj1 = new Object;
    Object obj2 = obj1;
    Object obj3;

    move(obj2, obj3);
    assert(obj3 is obj1);
    // obj2 unchanged
    assert(obj2 is obj1);
}

pure nothrow @safe @nogc unittest
{
    import std.algorithm.mutation;

    // Structs without destructors are simply copied
    struct S1
    {
        int a = 1;
        int b = 2;
    }
    S1 s11 = { 10, 11 };
    S1 s12;

    move(s11, s12);

    assert(s12 == S1(10, 11));
    assert(s11 == s12);

    // But structs with destructors or postblits are reset to their .init value
    // after copying to the target.
    struct S2
    {
        int a = 1;
        int b = 2;

        ~this() pure nothrow @safe @nogc { }
    }
    S2 s21 = { 3, 4 };
    S2 s22;

    move(s21, s22);

    assert(s21 == S2(1, 2));
    assert(s22 == S2(3, 4));
}

pure nothrow @safe @nogc unittest
{
    import std.algorithm.mutation;

    struct S
    {
        int a = 1;
        @disable this(this);
        ~this() pure nothrow @safe @nogc {}
    }
    S s1;
    s1.a = 2;
    S s2 = move(s1);
    assert(s1.a == 1);
    assert(s2.a == 2);
}

pure nothrow @safe @nogc unittest
{
    import std.algorithm.mutation;

    struct S
    {
        int a;
        void opPostMove(const ref S old)
        {
            assert(a == old.a);
            a++;
        }
    }
    S s1;
    s1.a = 41;
    S s2 = move(s1);
    assert(s2.a == 42);
}

pure nothrow @nogc @system unittest
{
    import std.algorithm.mutation;

    static struct Foo
    {
    pure nothrow @nogc:
        this(int* ptr) { _ptr = ptr; }
        ~this() { if (_ptr) ++*_ptr; }
        int* _ptr;
    }

    int val;
    Foo foo1 = void; // uninitialized
    auto foo2 = Foo(&val); // initialized
    assert(foo2._ptr is &val);

    // Using `move(foo2, foo1)` would have an undefined effect because it would destroy
    // the uninitialized foo1.
    // moveEmplace directly overwrites foo1 without destroying or initializing it first.
    moveEmplace(foo2, foo1);
    assert(foo1._ptr is &val);
    assert(foo2._ptr is null);
    assert(val == 0);
}

pure nothrow @safe @nogc unittest
{
    import std.algorithm.mutation;

    int[3] a = [ 1, 2, 3 ];
    int[5] b;
    assert(moveAll(a[], b[]) is b[3 .. $]);
    assert(a[] == b[0 .. 3]);
    int[3] cmp = [ 1, 2, 3 ];
    assert(a[] == cmp[]);
}

pure nothrow @nogc @system unittest
{
    import std.algorithm.mutation;

    static struct Foo
    {
        ~this() pure nothrow @nogc { if (_ptr) ++*_ptr; }
        int* _ptr;
    }
    int[3] refs = [0, 1, 2];
    Foo[3] src = [Foo(&refs[0]), Foo(&refs[1]), Foo(&refs[2])];
    Foo[5] dst = void;

    auto tail = moveEmplaceAll(src[], dst[]); // move 3 value from src over dst
    assert(tail.length == 2); // returns remaining uninitialized values
    initializeAll(tail);

    import std.algorithm.searching : all;
    assert(src[].all!(e => e._ptr is null));
    assert(dst[0 .. 3].all!(e => e._ptr !is null));
}

pure nothrow @safe @nogc unittest
{
    import std.algorithm.mutation;

    int[5] a = [ 1, 2, 3, 4, 5 ];
    int[3] b;
    assert(moveSome(a[], b[])[0] is a[3 .. $]);
    assert(a[0 .. 3] == b);
    assert(a == [ 1, 2, 3, 4, 5 ]);
}

pure nothrow @nogc @system unittest
{
    import std.algorithm.mutation;

    static struct Foo
    {
        ~this() pure nothrow @nogc { if (_ptr) ++*_ptr; }
        int* _ptr;
    }
    int[4] refs = [0, 1, 2, 3];
    Foo[4] src = [Foo(&refs[0]), Foo(&refs[1]), Foo(&refs[2]), Foo(&refs[3])];
    Foo[3] dst = void;

    auto res = moveEmplaceSome(src[], dst[]);
    assert(res.length == 2);

    import std.algorithm.searching : all;
    assert(src[0 .. 3].all!(e => e._ptr is null));
    assert(src[3]._ptr !is null);
    assert(dst[].all!(e => e._ptr !is null));
}

@safe unittest
{
    import std.algorithm.mutation;

    int[] a = [0, 1, 2, 3];
    assert(remove!(SwapStrategy.stable)(a, 1) == [0, 2, 3]);
    a = [0, 1, 2, 3];
    assert(remove!(SwapStrategy.unstable)(a, 1) == [0, 3, 2]);
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.algorithm.sorting : partition;

    // Put stuff greater than 3 on the left
    auto arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    assert(partition!(a => a > 3, SwapStrategy.stable)(arr) == [1, 2, 3]);
    assert(arr == [4, 5, 6, 7, 8, 9, 10, 1, 2, 3]);

    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    assert(partition!(a => a > 3, SwapStrategy.semistable)(arr) == [2, 3, 1]);
    assert(arr == [4, 5, 6, 7, 8, 9, 10, 2, 3, 1]);

    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    assert(partition!(a => a > 3, SwapStrategy.unstable)(arr) == [3, 2, 1]);
    assert(arr == [10, 9, 8, 4, 5, 6, 7, 3, 2, 1]);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    import std.typecons : tuple;

    auto a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.stable)(a, 1) == [ 0, 2, 3, 4, 5 ]);
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.stable)(a, 1, 3) == [ 0, 2, 4, 5] );
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.stable)(a, 1, tuple(3, 6)) == [ 0, 2 ]);

    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.unstable)(a, 1) == [0, 5, 2, 3, 4]);
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.unstable)(a, tuple(1, 4)) == [0, 5, 4]);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    import std.typecons : tuple;

    // Delete an index
    assert([4, 5, 6].remove(1) == [4, 6]);

    // Delete multiple indices
    assert([4, 5, 6, 7, 8].remove(1, 3) == [4, 6, 8]);

    // Use an indices range
    assert([4, 5, 6, 7, 8].remove(tuple(1, 3)) == [4, 7, 8]);

    // Use an indices range and individual indices
    assert([4, 5, 6, 7, 8].remove(0, tuple(1, 3), 4) == [7]);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    assert([5, 6, 7, 8].remove!(SwapStrategy.stable)(1) == [5, 7, 8]);
    assert([5, 6, 7, 8].remove!(SwapStrategy.unstable)(1) == [5, 8, 7]);
}

@safe unittest
{
    import std.algorithm.mutation;

    static immutable base = [1, 2, 3, 2, 4, 2, 5, 2];

    int[] arr = base[].dup;

    // using a string-based predicate
    assert(remove!("a == 2")(arr) == [ 1, 3, 4, 5 ]);

    // The original array contents have been modified,
    // so we need to reset it to its original state.
    // The length is unmodified however.
    arr[] = base[];

    // using a lambda predicate
    assert(remove!(a => a == 2)(arr) == [ 1, 3, 4, 5 ]);
}

@safe unittest
{
    import std.algorithm.mutation;

    int[] arr = [ 1, 2, 3 ];
    assert(arr.reverse == [ 3, 2, 1 ]);
}

@safe unittest
{
    import std.algorithm.mutation;

    char[] arr = "hello\U00010143\u0100\U00010143".dup;
    assert(arr.reverse == "\U00010143\u0100\U00010143olleh");
}

@safe pure unittest
{
    import std.algorithm.mutation;

    assert("  foobar  ".strip(' ') == "foobar");
    assert("00223.444500".strip('0') == "223.4445");
    assert("ëëêéüŗōpéêëë".strip('ë') == "êéüŗōpéê");
    assert([1, 1, 0, 1, 1].strip(1) == [0]);
    assert([0.0, 0.01, 0.01, 0.0].strip(0).length == 2);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    assert("  foobar  ".strip!(a => a == ' ')() == "foobar");
    assert("00223.444500".strip!(a => a == '0')() == "223.4445");
    assert("ëëêéüŗōpéêëë".strip!(a => a == 'ë')() == "êéüŗōpéê");
    assert([1, 1, 0, 1, 1].strip!(a => a == 1)() == [0]);
    assert([0.0, 0.01, 0.5, 0.6, 0.01, 0.0].strip!(a => a < 0.4)().length == 2);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    assert("  foobar  ".stripLeft(' ') == "foobar  ");
    assert("00223.444500".stripLeft('0') == "223.444500");
    assert("ůůűniçodêéé".stripLeft('ů') == "űniçodêéé");
    assert([1, 1, 0, 1, 1].stripLeft(1) == [0, 1, 1]);
    assert([0.0, 0.01, 0.01, 0.0].stripLeft(0).length == 3);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    assert("  foobar  ".stripLeft!(a => a == ' ')() == "foobar  ");
    assert("00223.444500".stripLeft!(a => a == '0')() == "223.444500");
    assert("ůůűniçodêéé".stripLeft!(a => a == 'ů')() == "űniçodêéé");
    assert([1, 1, 0, 1, 1].stripLeft!(a => a == 1)() == [0, 1, 1]);
    assert([0.0, 0.01, 0.10, 0.5, 0.6].stripLeft!(a => a < 0.4)().length == 2);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    assert("  foobar  ".stripRight(' ') == "  foobar");
    assert("00223.444500".stripRight('0') == "00223.4445");
    assert("ùniçodêéé".stripRight('é') == "ùniçodê");
    assert([1, 1, 0, 1, 1].stripRight(1) == [1, 1, 0]);
    assert([0.0, 0.01, 0.01, 0.0].stripRight(0).length == 3);
}

@safe pure unittest
{
    import std.algorithm.mutation;

    assert("  foobar  ".stripRight!(a => a == ' ')() == "  foobar");
    assert("00223.444500".stripRight!(a => a == '0')() == "00223.4445");
    assert("ùniçodêéé".stripRight!(a => a == 'é')() == "ùniçodê");
    assert([1, 1, 0, 1, 1].stripRight!(a => a == 1)() == [1, 1, 0]);
    assert([0.0, 0.01, 0.10, 0.5, 0.6].stripRight!(a => a > 0.4)().length == 3);
}

@safe unittest
{
    import std.algorithm.mutation;

    // Swapping POD (plain old data) types:
    int a = 42, b = 34;
    swap(a, b);
    assert(a == 34 && b == 42);

    // Swapping structs with indirection:
    static struct S { int x; char c; int[] y; }
    S s1 = { 0, 'z', [ 1, 2 ] };
    S s2 = { 42, 'a', [ 4, 6 ] };
    swap(s1, s2);
    assert(s1.x == 42);
    assert(s1.c == 'a');
    assert(s1.y == [ 4, 6 ]);

    assert(s2.x == 0);
    assert(s2.c == 'z');
    assert(s2.y == [ 1, 2 ]);

    // Immutables cannot be swapped:
    immutable int imm1 = 1, imm2 = 2;
    static assert(!__traits(compiles, swap(imm1, imm2)));

    int c = imm1 + 0;
    int d = imm2 + 0;
    swap(c, d);
    assert(c == 2);
    assert(d == 1);
}

@safe unittest
{
    import std.algorithm.mutation;

    // Non-copyable types can still be swapped.
    static struct NoCopy
    {
        this(this) { assert(0); }
        int n;
        string s;
    }
    NoCopy nc1, nc2;
    nc1.n = 127; nc1.s = "abc";
    nc2.n = 513; nc2.s = "uvwxyz";

    swap(nc1, nc2);
    assert(nc1.n == 513 && nc1.s == "uvwxyz");
    assert(nc2.n == 127 && nc2.s == "abc");

    swap(nc1, nc1);
    swap(nc2, nc2);
    assert(nc1.n == 513 && nc1.s == "uvwxyz");
    assert(nc2.n == 127 && nc2.s == "abc");

    // Types containing non-copyable fields can also be swapped.
    static struct NoCopyHolder
    {
        NoCopy noCopy;
    }
    NoCopyHolder h1, h2;
    h1.noCopy.n = 31; h1.noCopy.s = "abc";
    h2.noCopy.n = 65; h2.noCopy.s = null;

    swap(h1, h2);
    assert(h1.noCopy.n == 65 && h1.noCopy.s == null);
    assert(h2.noCopy.n == 31 && h2.noCopy.s == "abc");

    swap(h1, h1);
    swap(h2, h2);
    assert(h1.noCopy.n == 65 && h1.noCopy.s == null);
    assert(h2.noCopy.n == 31 && h2.noCopy.s == "abc");

    // Const types cannot be swapped.
    const NoCopy const1, const2;
    assert(const1.n == 0 && const2.n == 0);
    static assert(!__traits(compiles, swap(const1, const2)));
}

pure @safe nothrow unittest
{
    import std.algorithm.mutation;

    import std.algorithm.comparison : equal;
    auto a = [1, 2, 3];
    a.swapAt(1, 2);
    assert(a.equal([1, 3, 2]));
}

@safe unittest
{
    import std.algorithm.mutation;

    import std.range : empty;
    int[] a = [ 100, 101, 102, 103 ];
    int[] b = [ 0, 1, 2, 3 ];
    auto c = swapRanges(a[1 .. 3], b[2 .. 4]);
    assert(c[0].empty && c[1].empty);
    assert(a == [ 100, 2, 3, 103 ]);
    assert(b == [ 0, 1, 101, 102 ]);
}

nothrow @system unittest
{
    import std.algorithm.mutation;

    import core.stdc.stdlib : malloc, free;

    auto s = (cast(int*) malloc(5 * int.sizeof))[0 .. 5];
    uninitializedFill(s, 42);
    assert(s == [ 42, 42, 42, 42, 42 ]);

    scope(exit) free(s.ptr);
}


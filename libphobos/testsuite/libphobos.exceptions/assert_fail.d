import core.stdc.stdio : fprintf, stderr;
import core.internal.dassert : _d_assert_fail;

void test(string comp = "==", A, B)(A a, B b, string msg, size_t line = __LINE__)
{
    test(_d_assert_fail!(A)(comp, a, b), msg, line);
}

void test(const string actual, const string expected, size_t line = __LINE__)
{
    import core.exception : AssertError;

    if (actual != expected)
    {
        const msg = "Mismatch!\nExpected: <" ~ expected ~ ">\nActual:   <" ~ actual ~ '>';
        throw new AssertError(msg, __FILE__, line);
    }
}

void testIntegers()
{
    test(1, 2, "1 != 2");
    test(-10, 8, "-10 != 8");
    test(byte.min, byte.max, "-128 != 127");
    test(ubyte.min, ubyte.max, "0 != 255");
    test(short.min, short.max, "-32768 != 32767");
    test(ushort.min, ushort.max, "0 != 65535");
    test(int.min, int.max, "-2147483648 != 2147483647");
    test(uint.min, uint.max, "0 != 4294967295");
    test(long.min, long.max, "-9223372036854775808 != 9223372036854775807");
    test(ulong.min, ulong.max, "0 != 18446744073709551615");
    test(shared(ulong).min, shared(ulong).max, "0 != 18446744073709551615");

    int testFun() { return 1; }
    test(testFun(), 2, "1 != 2");
}

void testIntegerComparisons()
{
    test!"!="(2, 2, "2 == 2");
    test!"<"(2, 1, "2 >= 1");
    test!"<="(2, 1, "2 > 1");
    test!">"(1, 2, "1 <= 2");
    test!">="(1, 2, "1 < 2");
}

void testFloatingPoint()
{
    if (__ctfe)
    {
        test(float.max, -float.max, "<float not supported> != <float not supported>");
        test(double.max, -double.max, "<double not supported> != <double not supported>");
        test(real(1), real(-1), "<real not supported> != <real not supported>");
    }
    else
    {
        test(1.5, 2.5, "1.5 != 2.5");
        test(float.max, -float.max, "3.40282e+38 != -3.40282e+38");
        test(double.max, -double.max, "1.79769e+308 != -1.79769e+308");
        test(real(1), real(-1), "1 != -1");
    }
}

void testPointers()
{
    static struct S
    {
        string toString() const { return "S(...)"; }
    }

    static if ((void*).sizeof == 4)
        enum ptr = "0x12345670";
    else
        enum ptr = "0x123456789abcdef0";

    int* p = cast(int*) mixin(ptr);
    test(cast(S*) p, p, ptr ~ " != " ~ ptr);
}

void testStrings()
{
    test("foo", "bar", `"foo" != "bar"`);
    test("", "bar", `"" != "bar"`);

    char[] dlang = "dlang".dup;
    const(char)[] rust = "rust";
    test(dlang, rust, `"dlang" != "rust"`);

    // https://issues.dlang.org/show_bug.cgi?id=20322
    test("left"w, "right"w, `"left" != "right"`);
    test("left"d, "right"d, `"left" != "right"`);

    test('A', 'B', "'A' != 'B'");
    test(wchar('❤'), wchar('∑'), "'❤' != '∑'");
    test(dchar('❤'), dchar('∑'), "'❤' != '∑'");

    // Detect invalid code points
    test(char(255), 'B', "cast(char) 255 != 'B'");
    test(wchar(0xD888), wchar('∑'), "cast(wchar) 55432 != '∑'");
    test(dchar(0xDDDD), dchar('∑'), "cast(dchar) 56797 != '∑'");
}

void testToString()
{
    class Foo
    {
        this(string payload) {
            this.payload = payload;
        }

        string payload;
        override string toString() {
            return "Foo(" ~ payload ~ ")";
        }
    }
    test(new Foo("a"), new Foo("b"), "Foo(a) != Foo(b)");

    scope f = cast(shared) new Foo("a");
    if (!__ctfe) // Ref somehow get's lost in CTFE
    test!"!="(f, f, "Foo(a) == Foo(a)");

    // Verifiy that the const toString is selected if present
    static struct Overloaded
    {
        string toString()
        {
            return "Mutable";
        }

        string toString() const
        {
            return "Const";
        }
    }

    test!"!="(Overloaded(), Overloaded(), "Const == Const");

    Foo fnull = null;
    test!"!is"(fnull, fnull, "`null` is `null`");
}


void testArray()
{
    test([1], [0], "[1] != [0]");
    test([1, 2, 3], [0], "[1, 2, 3] != [0]");

    // test with long arrays
    int[] arr;
    foreach (i; 0 .. 100)
        arr ~= i;
    test(arr, [0], "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, ...] != [0]");

    // Ignore fake arrays
    static struct S
    {
        int[2] arr;
        int[] get() return { return arr[]; }
        alias get this;
    }

    const a = S([1, 2]);
    test(a, S([3, 4]), "S([1, 2]) != S([3, 4])");
}

void testStruct()
{
    struct S { int s; }
    struct T { T[] t; }
    test(S(0), S(1), "S(0) != S(1)");
    test(T([T(null)]), T(null), "T([T([])]) != T([])");

    // https://issues.dlang.org/show_bug.cgi?id=20323
    static struct NoCopy
    {
        @disable this(this);
    }

    NoCopy n;
    test(_d_assert_fail!(typeof(n))("!=", n, n), "NoCopy() == NoCopy()");

    shared NoCopy sn;
    test(_d_assert_fail!(typeof(sn))("!=", sn, sn), "NoCopy() == NoCopy()");
}

void testAA()
{
    test([1:"one"], [2: "two"], `[1: "one"] != [2: "two"]`);
    test!"in"(1, [2: 3], "1 !in [2: 3]");
    test!"in"("foo", ["bar": true], `"foo" !in ["bar": true]`);
}

void testAttributes() @safe pure @nogc nothrow
{
    int a;
    string s = _d_assert_fail!(int, char)("==", a, 'c', 1, 'd');
    assert(s == `(0, 'c') != (1, 'd')`);

    string s2 = _d_assert_fail!int("", a);
    assert(s2 == `0 != true`);
}

// https://issues.dlang.org/show_bug.cgi?id=20066
void testVoidArray()
{
    test!"!is"([], null, (__ctfe ? "<void[] not supported>" : "[]") ~ " is `null`");
    test!"!is"(null, null, "`null` is `null`");
    test([1], null, "[1] != `null`");
    test("s", null, "\"s\" != `null`");
    test(['c'], null, "\"c\" != `null`");
    test!"!="(null, null, "`null` == `null`");

    const void[] chunk = [byte(1), byte(2), byte(3)];
    test(chunk, null, (__ctfe ? "<void[] not supported>" : "[1, 2, 3]") ~ " != `null`");
}

void testTemporary()
{
    static struct Bad
    {
        ~this() @system {}
    }

    test!"!="(Bad(), Bad(), "Bad() == Bad()");
}

void testEnum()
{
    static struct UUID {
        union
        {
            ubyte[] data = [1];
        }
    }

    ubyte[] data;
    enum ctfe = UUID();
    test(_d_assert_fail!(ubyte[])("==", ctfe.data, data), "[1] != []");
}

void testUnary()
{
    test(_d_assert_fail!int("", 9), "9 != true");
    test(_d_assert_fail!(int[])("!", [1, 2, 3]), "[1, 2, 3] == true");
}

void testTuple()
{
    test(_d_assert_fail("=="), "() != ()");
    test(_d_assert_fail("!="), "() == ()");
    test(_d_assert_fail(">="), "() < ()");
}

void testStructEquals()
{
    struct T {
        bool b;
        int i;
        float f1 = 2.5;
        float f2 = 0;
        string s1 = "bar";
        string s2;
    }

    T t1;
    test!"!="(t1, t1, `T(false, 0, 2.5, 0, "bar", "") == T(false, 0, 2.5, 0, "bar", "")`);
    T t2 = {s1: "bari"};
    test(t1, t2, `T(false, 0, 2.5, 0, "bar", "") != T(false, 0, 2.5, 0, "bari", "")`);
}

void testStructEquals2()
{
    struct T {
        bool b;
        int i;
        float f1 = 2.5;
        float f2 = 0;
    }

    T t1;
    test!"!="(t1, t1, `T(false, 0, 2.5, 0) == T(false, 0, 2.5, 0)`);
    T t2 = {i: 2};
    test(t1, t2, `T(false, 0, 2.5, 0) != T(false, 2, 2.5, 0)`);
}

void testStructEquals3()
{
    struct T {
        bool b;
        int i;
        string s1 = "bar";
        string s2;
    }

    T t1;
    test!"!="(t1, t1, `T(false, 0, "bar", "") == T(false, 0, "bar", "")`);
    T t2 = {s1: "bari"};
    test(t1, t2, `T(false, 0, "bar", "") != T(false, 0, "bari", "")`);
}

void testStructEquals4()
{
    struct T {
        float f1 = 2.5;
        float f2 = 0;
        string s1 = "bar";
        string s2;
    }

    T t1;
    test!"!="(t1, t1, `T(2.5, 0, "bar", "") == T(2.5, 0, "bar", "")`);
    T t2 = {s1: "bari"};
    test(t1, t2, `T(2.5, 0, "bar", "") != T(2.5, 0, "bari", "")`);
}

void testStructEquals5()
{
    struct T {
        bool b;
        int i;
        float f2 = 0;
        string s2;
    }

    T t1;
    test!"!="(t1, t1, `T(false, 0, 0, "") == T(false, 0, 0, "")`);
    T t2 = {b: true};
    test(t1, t2, `T(false, 0, 0, "") != T(true, 0, 0, "")`);
}

void testStructEquals6()
{
    class C { override string toString() { return "C()"; }}
    struct T {
        bool b;
        int i;
        float f2 = 0;
        string s2;
        int[] arr;
        C c;
    }

    T t1;
    test!"!="(t1, t1, "T(false, 0, 0, \"\", [], `null`) == T(false, 0, 0, \"\", [], `null`)");
    T t2 = {arr: [1]};
    test(t1, t2, "T(false, 0, 0, \"\", [], `null`) != T(false, 0, 0, \"\", [1], `null`)");
    T t3 = {c: new C()};
    test(t1, t3, "T(false, 0, 0, \"\", [], `null`) != T(false, 0, 0, \"\", [], C())");
}

void testContextPointer()
{
    int i;
    struct T
    {
        int j;
        int get()
        {
            return i * j;
        }
    }
    T t = T(1);
    t.tupleof[$-1] = cast(void*) 0xABCD; // Deterministic context pointer
    test(t, t, `T(1, <context>: 0xabcd) != T(1, <context>: 0xabcd)`);
}

void testExternClasses()
{
    {
        extern(C++) static class Cpp
        {
            int a;
            this(int a) { this.a = a; }
        }
        scope a = new Cpp(1);
        scope b = new Cpp(2);
        test(a, b, "Cpp(1) != Cpp(2)");
        test(a, Cpp.init, "Cpp(1) != null");
    }
    {
        extern(C++) static class CppToString
        {
            int a;
            this(int a) { this.a = a; }
            extern(D) string toString() const { return a == 0 ? "hello" : "world"; }
        }
        scope a = new CppToString(0);
        scope b = new CppToString(1);
        test(a, b, "hello != world");
    }
    if (!__ctfe)
    {
        extern(C++) static class Opaque;
        Opaque null_ = null;
        Opaque notNull = cast(Opaque) &null_;
        test(null_, notNull, "null != <Opaque>");
    }
    {
        extern(C++) static interface Stuff {}
        scope Stuff stuff = new class Stuff {};
        test(stuff, Stuff.init, "Stuff() != null");
    }
}

void testShared()
{
    static struct Small
    {
        int i;
    }

    auto s1 = shared Small(1);
    const s2 = shared Small(2);
    test(s1, s2, "Small(1) != Small(2)");

    static struct Big
    {
        long[10] l;
    }

    auto b1 = shared Big([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    const b2 = shared Big();
    test(b1, b2, "Big([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) != Big([0, 0, 0, 0, 0, 0, 0, 0, 0, 0])");

    // Sanity check: Big shouldn't be supported by atomicLoad
    import core.atomic : atomicLoad;
    static assert( __traits(compiles, atomicLoad(s1)));
    static assert(!__traits(compiles, atomicLoad(b1)));

    static struct Fail
    {
        int value;

        @safe pure nothrow @nogc:
        bool opCast () shared const scope { return true; }
    }

    shared Fail fail = { value: 1 };
    assert(_d_assert_fail!(shared Fail)("==", fail) == "Fail(1) != true");
    assert(_d_assert_fail!(shared Fail)("==", fail, fail) == "Fail(1) != Fail(1)");
}

void testException()
{
    static struct MayThrow
    {
        int i;
        string toString()
        {
            if (i == 1)
                throw new Exception("Error");
            return "Some message";
        }
    }

    test(MayThrow(0), MayThrow(1), `Some message != <toString() failed: "Error", called on MayThrow(1)>`);
}

void testOverlappingFields()
{
    static struct S
    {
        union
        {
            double num;
            immutable(char)[] name;
        }
    }

    test(S(1.0), S(2.0), "S(<overlapped field>, <overlapped field>) != S(<overlapped field>, <overlapped field>)");

    static struct S2
    {
        int valid;
        union
        {
            double num;
            immutable(char)[] name;
        }
    }

    test(S2(4, 1.0), S2(5, 2.0), "S2(4, <overlapped field>, <overlapped field>) != S2(5, <overlapped field>, <overlapped field>)");

    static struct S3
    {
        union
        {
            double num;
            immutable(char)[] name;
        }
        int valid;
    }
    S3 a = {
        num: 1.0,
        valid: 8
    };

    S3 b = {
        num: 1.0,
        valid: 8
    };
    test(a, b, "S3(<overlapped field>, <overlapped field>, 8) != S3(<overlapped field>, <overlapped field>, 8)");
}

void testDestruction()
{
    static class Test
    {
        __gshared string unary, binary;
        __gshared bool run;

        ~this()
        {
            run = true;
            unary = _d_assert_fail!int("", 1);
            binary = _d_assert_fail!int("==", 1, 2);
        }
    }

    static void createGarbage()
    {
        new Test();
        new long[100];
    }

    import core.memory : GC;
    createGarbage();
    GC.collect();

    assert(Test.run);
    assert(Test.unary == "Assertion failed (rich formatting is disabled in finalizers)");
    assert(Test.binary == "Assertion failed (rich formatting is disabled in finalizers)");
}

int main()
{
    testIntegers();
    testIntegerComparisons();
    testFloatingPoint();
    testPointers();
    testStrings();
    testToString();
    testArray();
    testStruct();
    testAA();
    testAttributes();
    testVoidArray();
    testTemporary();
    testEnum();
    testUnary();
    testTuple();
    if (!__ctfe)
        testStructEquals();
    if (!__ctfe)
        testStructEquals2();
    testStructEquals3();
    if (!__ctfe)
        testStructEquals4();
    if (!__ctfe)
        testStructEquals5();
    if (!__ctfe)
        testStructEquals6();
    testContextPointer();
    testExternClasses();
    testShared();
    testException();
    testOverlappingFields();
    if (!__ctfe)
        testDestruction();

    if (!__ctfe)
        fprintf(stderr, "success.\n");
    return 0;
}

enum forceCTFE = main();

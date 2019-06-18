void main()
{
    testKeysValues1();
    testKeysValues2();
    testGet1();
    testGet2();
    testRequire1();
    testRequire2();
    testRequire3();
    testUpdate1();
    testUpdate2();
    testByKey1();
    testByKey2();
    testByKey3();
    testByKey4();
    issue5842();
    issue5842Expanded();
    issue5925();
    issue8583();
    issue9052();
    issue9119();
    issue9852();
    issue10381();
    issue10720();
    issue11761();
    issue13078();
    issue14104();
    issue14626();
    issue15290();
    issue15367();
    issue16974();
    issue18071();
    issue20440();
    issue21442();
    testIterationWithConst();
    testStructArrayKey();
    miscTests1();
    miscTests2();
    testRemove();
    testZeroSizedValue();
    testTombstonePurging();
    testClear();
}

void testKeysValues1()
{
    static struct T
    {
        byte b;
        static size_t count;
        this(this) { ++count; }
    }
    T[int] aa;
    T t;
    aa[0] = t;
    aa[1] = t;
    assert(T.count == 2);
    auto vals = aa.values;
    assert(vals.length == 2);
    assert(T.count == 4);

    T.count = 0;
    int[T] aa2;
    aa2[t] = 0;
    assert(T.count == 1);
    aa2[t] = 1;
    assert(T.count == 1);
    auto keys = aa2.keys;
    assert(keys.length == 1);
    assert(T.count == 2);
}

void testKeysValues2() nothrow pure
{
    int[string] aa;

    assert(aa.keys.length == 0);
    assert(aa.values.length == 0);

    aa["hello"] = 3;
    assert(aa["hello"] == 3);
    aa["hello"]++;
    assert(aa["hello"] == 4);

    assert(aa.length == 1);

    string[] keys = aa.keys;
    assert(keys.length == 1);
    assert(keys[0] == "hello");

    int[] values = aa.values;
    assert(values.length == 1);
    assert(values[0] == 4);

    aa.rehash;
    assert(aa.length == 1);
    assert(aa["hello"] == 4);

    aa["foo"] = 1;
    aa["bar"] = 2;
    aa["batz"] = 3;

    assert(aa.keys.length == 4);
    assert(aa.values.length == 4);

    foreach (a; aa.keys)
    {
        assert(a.length != 0);
        assert(a.ptr != null);
    }

    foreach (v; aa.values)
    {
        assert(v != 0);
    }
}

void testGet1() @safe
{
    int[string] aa;
    int a;
    foreach (val; aa.byKeyValue)
    {
        ++aa[val.key];
        a = val.value;
    }
}

void testGet2()
{
    static class T
    {
        static size_t count;
        this() { ++count; }
    }

    T[string] aa;

    auto a = new T;
    aa["foo"] = a;
    assert(T.count == 1);
    auto b = aa.get("foo", new T);
    assert(T.count == 1);
    assert(b is a);
    auto c = aa.get("bar", new T);
    assert(T.count == 2);
    assert(c !is a);

    //Obviously get doesn't add.
    assert("bar" !in aa);
}

void testRequire1()
{
    static class T
    {
        static size_t count;
        this() { ++count; }
    }

    T[string] aa;

    auto a = new T;
    aa["foo"] = a;
    assert(T.count == 1);
    auto b = aa.require("foo", new T);
    assert(T.count == 1);
    assert(b is a);
    auto c = aa.require("bar", null);
    assert(T.count == 1);
    assert(c is null);
    assert("bar" in aa);
    auto d = aa.require("bar", new T);
    assert(d is null);
    auto e = aa.require("baz", new T);
    assert(T.count == 2);
    assert(e !is a);

    assert("baz" in aa);

    bool created = false;
    auto f = aa.require("qux", { created = true; return new T; }());
    assert(created == true);

    T g;
    auto h = aa.require("qux", { g = new T; return g; }());
    assert(g !is h);
}

void testRequire2()
{
    static struct S
    {
        int value;
    }

    S[string] aa;

    aa.require("foo").value = 1;
    assert(aa == ["foo" : S(1)]);

    aa["bar"] = S(2);
    auto a = aa.require("bar", S(3));
    assert(a == S(2));

    auto b = aa["bar"];
    assert(b == S(2));

    S* c = &aa.require("baz", S(4));
    assert(c is &aa["baz"]);
    assert(*c == S(4));

    assert("baz" in aa);

    auto d = aa["baz"];
    assert(d == S(4));
}

void testRequire3() pure
{
    string[string] aa;

    auto a = aa.require("foo", "bar");
    assert("foo" in aa);
}


void testUpdate1()
{
    static class C {}
    C[string] aa;

    C orig = new C;
    aa["foo"] = orig;

    C newer;
    C older;

    void test(string key)
    {
        aa.update(key, {
            newer = new C;
            return newer;
        }, (ref C c) {
            older = c;
            newer = new C;
            return newer;
        });
    }

    test("foo");
    assert(older is orig);
    assert(newer is aa["foo"]);

    test("bar");
    assert(newer is aa["bar"]);
}

void testUpdate2()
{
    static class C {}
    C[string] aa;

    auto created = false;
    auto updated = false;

    class Creator
    {
        C opCall()
        {
            created = true;
            return new C();
        }
    }

    class Updater
    {
        C opCall(ref C)
        {
            updated = true;
            return new C();
        }
    }

    aa.update("foo", new Creator, new Updater);
    assert(created);
    aa.update("foo", new Creator, new Updater);
    assert(updated);
}

void testByKey1() @safe
{
    static struct BadValue
    {
        int x;
        this(this) @system { *(cast(ubyte*)(null) + 100000) = 5; } // not @safe
        alias x this;
    }

    BadValue[int] aa;

    // FIXME: Should be @system because of the postblit
    if (false)
        auto x = aa.byKey.front;
}

void testByKey2() nothrow pure
{
    int[int] a;
    foreach (i; a.byKey)
    {
        assert(false);
    }
    foreach (i; a.byValue)
    {
        assert(false);
    }
}

void testByKey3() /*nothrow*/ pure
{
    auto a = [ 1:"one", 2:"two", 3:"three" ];
    auto b = a.dup;
    assert(b == [ 1:"one", 2:"two", 3:"three" ]);

    int[] c;
    foreach (k; a.byKey)
    {
        c ~= k;
    }

    assert(c.length == 3);
    assert(c[0] == 1 || c[1] == 1 || c[2] == 1);
    assert(c[0] == 2 || c[1] == 2 || c[2] == 2);
    assert(c[0] == 3 || c[1] == 3 || c[2] == 3);
}

void testByKey4() nothrow pure
{
    string[] keys = ["a", "b", "c", "d", "e", "f"];

    // Test forward range capabilities of byKey
    {
        int[string] aa;
        foreach (key; keys)
            aa[key] = 0;

        auto keyRange = aa.byKey();
        auto savedKeyRange = keyRange.save;

        // Consume key range once
        size_t keyCount = 0;
        while (!keyRange.empty)
        {
            aa[keyRange.front]++;
            keyCount++;
            keyRange.popFront();
        }

        foreach (key; keys)
        {
            assert(aa[key] == 1);
        }
        assert(keyCount == keys.length);

        // Verify it's possible to iterate the range the second time
        keyCount = 0;
        while (!savedKeyRange.empty)
        {
            aa[savedKeyRange.front]++;
            keyCount++;
            savedKeyRange.popFront();
        }

        foreach (key; keys)
        {
            assert(aa[key] == 2);
        }
        assert(keyCount == keys.length);
    }

    // Test forward range capabilities of byValue
    {
        size_t[string] aa;
        foreach (i; 0 .. keys.length)
        {
            aa[keys[i]] = i;
        }

        auto valRange = aa.byValue();
        auto savedValRange = valRange.save;

        // Consume value range once
        int[] hasSeen;
        hasSeen.length = keys.length;
        while (!valRange.empty)
        {
            assert(hasSeen[valRange.front] == 0);
            hasSeen[valRange.front]++;
            valRange.popFront();
        }

        foreach (sawValue; hasSeen) { assert(sawValue == 1); }

        // Verify it's possible to iterate the range the second time
        hasSeen = null;
        hasSeen.length = keys.length;
        while (!savedValRange.empty)
        {
            assert(!hasSeen[savedValRange.front]);
            hasSeen[savedValRange.front] = true;
            savedValRange.popFront();
        }

        foreach (sawValue; hasSeen) { assert(sawValue); }
    }
}

void issue5842() pure nothrow
{
    string[string] test = null;
    test["test1"] = "test1";
    test.remove("test1");
    test.rehash;
    test["test3"] = "test3"; // causes divide by zero if rehash broke the AA
}

/// expanded test for 5842: increase AA size past the point where the AA
/// stops using binit, in order to test another code path in rehash.
void issue5842Expanded() pure nothrow
{
    int[int] aa;
    foreach (int i; 0 .. 32)
        aa[i] = i;
    foreach (int i; 0 .. 32)
        aa.remove(i);
    aa.rehash;
    aa[1] = 1;
}

void issue5925() nothrow pure
{
    const a = [4:0];
    const b = [4:0];
    assert(a == b);
}

/// test for bug 8583: ensure Slot and aaA are on the same page wrt value alignment
void issue8583() nothrow pure
{
    string[byte]    aa0 = [0: "zero"];
    string[uint[3]] aa1 = [[1,2,3]: "onetwothree"];
    ushort[uint[3]] aa2 = [[9,8,7]: 987];
    ushort[uint[4]] aa3 = [[1,2,3,4]: 1234];
    string[uint[5]] aa4 = [[1,2,3,4,5]: "onetwothreefourfive"];

    assert(aa0.byValue.front == "zero");
    assert(aa1.byValue.front == "onetwothree");
    assert(aa2.byValue.front == 987);
    assert(aa3.byValue.front == 1234);
    assert(aa4.byValue.front == "onetwothreefourfive");
}

void issue9052() nothrow pure
{
    static struct Json {
        Json[string] aa;
        void opAssign(Json) {}
        size_t length() const { return aa.length; }
        // This length() instantiates AssociativeArray!(string, const(Json)) to call AA.length(), and
        // inside ref Slot opAssign(Slot p); (which is automatically generated by compiler in Slot),
        // this.value = p.value would actually fail, because both side types of the assignment
        // are const(Json).
    }
}

void issue9119()
{
    int[string] aa;
    assert(aa.byKeyValue.empty);

    aa["a"] = 1;
    aa["b"] = 2;
    aa["c"] = 3;

    auto pairs = aa.byKeyValue;

    auto savedPairs = pairs.save;
    size_t count = 0;
    while (!pairs.empty)
    {
        assert(pairs.front.key in aa);
        assert(pairs.front.value == aa[pairs.front.key]);
        count++;
        pairs.popFront();
    }
    assert(count == aa.length);

    // Verify that saved range can iterate over the AA again
    count = 0;
    while (!savedPairs.empty)
    {
        assert(savedPairs.front.key in aa);
        assert(savedPairs.front.value == aa[savedPairs.front.key]);
        count++;
        savedPairs.popFront();
    }
    assert(count == aa.length);
}

void issue9852() nothrow pure
{
    // Original test case (revised, original assert was wrong)
    int[string] a;
    a["foo"] = 0;
    a.remove("foo");
    assert(a == null); // should not crash

    int[string] b;
    assert(b is null);
    assert(a == b); // should not deref null
    assert(b == a); // ditto

    int[string] c;
    c["a"] = 1;
    assert(a != c); // comparison with empty non-null AA
    assert(c != a);
    assert(b != c); // comparison with null AA
    assert(c != b);
}

void issue10381()
{
    alias II = int[int];
    II aa1 = [0 : 1];
    II aa2 = [0 : 1];
    II aa3 = [0 : 2];
    assert(aa1 == aa2); // Passes
    assert(typeid(II).equals(&aa1, &aa2));
    assert(!typeid(II).equals(&aa1, &aa3));
}

void issue10720() nothrow pure
{
    static struct NC
    {
        @disable this(this) { }
    }

    NC[string] aa;
    static assert(!is(aa.nonExistingField));
}

/// bug 11761: test forward range functionality
void issue11761() pure nothrow
{
    auto aa = ["a": 1];

    void testFwdRange(R, T)(R fwdRange, T testValue)
    {
        assert(!fwdRange.empty);
        assert(fwdRange.front == testValue);
        static assert(is(typeof(fwdRange.save) == typeof(fwdRange)));

        auto saved = fwdRange.save;
        fwdRange.popFront();
        assert(fwdRange.empty);

        assert(!saved.empty);
        assert(saved.front == testValue);
        saved.popFront();
        assert(saved.empty);
    }

    testFwdRange(aa.byKey, "a");
    testFwdRange(aa.byValue, 1);
    //testFwdRange(aa.byPair, tuple("a", 1));
}

void issue13078() nothrow pure
{
    shared string[][string] map;
    map.rehash;
}

void issue14104()
{
    import core.stdc.stdio;

    alias K = const(ubyte)*;
    size_t[K] aa;
    immutable key = cast(K)(cast(size_t) uint.max + 1);
    aa[key] = 12;
    assert(key in aa);
}

void issue14626()
{
    static struct S
    {
        string[string] aa;
        inout(string) key() inout { return aa.byKey().front; }
        inout(string) val() inout { return aa.byValue().front; }
        auto keyval() inout { return aa.byKeyValue().front; }
    }

    S s = S(["a":"b"]);
    assert(s.key() == "a");
    assert(s.val() == "b");
    assert(s.keyval().key == "a");
    assert(s.keyval().value == "b");

    void testInoutKeyVal(inout(string) key)
    {
        inout(string)[typeof(key)] aa;

        foreach (i; aa.byKey()) {}
        foreach (i; aa.byValue()) {}
        foreach (i; aa.byKeyValue()) {}
    }

    const int[int] caa;
    static assert(is(typeof(caa.byValue().front) == const int));
}

/// test duplicated keys in AA literal
/// https://issues.dlang.org/show_bug.cgi?id=15290
void issue15290()
{
    string[int] aa = [ 0: "a", 0: "b" ];
    assert(aa.length == 1);
    assert(aa.keys == [ 0 ]);
}

void issue15367()
{
    void f1() {}
    void f2() {}

    // TypeInfo_Delegate.getHash
    int[void delegate()] aa;
    assert(aa.length == 0);
    aa[&f1] = 1;
    assert(aa.length == 1);
    aa[&f1] = 1;
    assert(aa.length == 1);

    auto a1 = [&f2, &f1];
    auto a2 = [&f2, &f1];

    // TypeInfo_Delegate.equals
    for (auto i = 0; i < 2; i++)
        assert(a1[i] == a2[i]);
    assert(a1 == a2);

    // TypeInfo_Delegate.compare
    for (auto i = 0; i < 2; i++)
        assert(a1[i] <= a2[i]);
    assert(a1 <= a2);
}

/// test AA as key
/// https://issues.dlang.org/show_bug.cgi?id=16974
void issue16974()
{
    int[int] a = [1 : 2], a2 = [1 : 2];

    assert([a : 3] == [a : 3]);
    assert([a : 3] == [a2 : 3]);

    assert(typeid(a).getHash(&a) == typeid(a).getHash(&a));
    assert(typeid(a).getHash(&a) == typeid(a).getHash(&a2));
}

/// test safety for alias-this'd AA that have unsafe opCast
/// https://issues.dlang.org/show_bug.cgi?id=18071
void issue18071()
{
    static struct Foo
    {
        int[int] aa;
        auto opCast() pure nothrow @nogc
        {
            *cast(uint*)0xdeadbeef = 0xcafebabe;// unsafe
            return null;
        }
        alias aa this;
    }

    Foo f;
    () @safe { assert(f.byKey.empty); }();
}

/// Test that `require` works even with types whose opAssign
/// doesn't return a reference to the receiver.
/// https://issues.dlang.org/show_bug.cgi?id=20440
void issue20440() @safe
{
    static struct S
    {
        int value;
        auto opAssign(S s) {
            this.value = s.value;
            return this;
        }
    }
    S[S] aa;
    assert(aa.require(S(1), S(2)) == S(2));
    assert(aa[S(1)] == S(2));
}

///
void issue21442()
{
    import core.memory;

    size_t[size_t] glob;

    class Foo
    {
        size_t count;

        this (size_t entries) @safe
        {
            this.count = entries;
            foreach (idx; 0 .. entries)
                glob[idx] = idx;
        }

        ~this () @safe
        {
            foreach (idx; 0 .. this.count)
                glob.remove(idx);
        }
    }

    void bar () @safe
    {
        Foo f = new Foo(16);
    }

    bar();
    GC.collect(); // Needs to happen from a GC collection
}

/// Verify iteration with const.
void testIterationWithConst()
{
    auto aa = [1:2, 3:4];
    foreach (const t; aa.byKeyValue)
    {
        auto k = t.key;
        auto v = t.value;
    }
}

void testStructArrayKey() @safe
{
    struct S
    {
        int i;
    const @safe nothrow:
        hash_t toHash() { return 0; }
        bool opEquals(const S) { return true; }
        int opCmp(const S) { return 0; }
    }

    int[S[]] aa = [[S(11)] : 13];
    assert(aa[[S(12)]] == 13);
}

void miscTests1() pure nothrow
{
    string[int] key1 = [1 : "true", 2 : "false"];
    string[int] key2 = [1 : "false", 2 : "true"];
    string[int] key3;

    // AA lits create a larger hashtable
    int[string[int]] aa1 = [key1 : 100, key2 : 200, key3 : 300];

    // Ensure consistent hash values are computed for key1
    assert((key1 in aa1) !is null);

    // Manually assigning to an empty AA creates a smaller hashtable
    int[string[int]] aa2;
    aa2[key1] = 100;
    aa2[key2] = 200;
    aa2[key3] = 300;

    assert(aa1 == aa2);

    // Ensure binary-independence of equal hash keys
    string[int] key2a;
    key2a[1] = "false";
    key2a[2] = "true";

    assert(aa1[key2a] == 200);
}

void miscTests2()
{
    int[int] aa;
    foreach (k, v; aa)
        assert(false);
    foreach (v; aa)
        assert(false);
    assert(aa.byKey.empty);
    assert(aa.byValue.empty);
    assert(aa.byKeyValue.empty);

    size_t n;
    aa = [0 : 3, 1 : 4, 2 : 5];
    foreach (k, v; aa)
    {
        n += k;
        assert(k >= 0 && k < 3);
        assert(v >= 3 && v < 6);
    }
    assert(n == 3);
    n = 0;

    foreach (v; aa)
    {
        n += v;
        assert(v >= 3 && v < 6);
    }
    assert(n == 12);

    n = 0;
    foreach (k, v; aa)
    {
        ++n;
        break;
    }
    assert(n == 1);

    n = 0;
    foreach (v; aa)
    {
        ++n;
        break;
    }
    assert(n == 1);
}

void testRemove()
{
    int[int] aa;
    assert(!aa.remove(0));
    aa = [0 : 1];
    assert(aa.remove(0));
    assert(!aa.remove(0));
    aa[1] = 2;
    assert(!aa.remove(0));
    assert(aa.remove(1));

    assert(aa.length == 0);
    assert(aa.byKey.empty);
}

/// test zero sized value (hashset)
void testZeroSizedValue()
{
    alias V = void[0];
    auto aa = [0 : V.init];
    assert(aa.length == 1);
    assert(aa.byKey.front == 0);
    assert(aa.byValue.front == V.init);
    aa[1] = V.init;
    assert(aa.length == 2);
    aa[0] = V.init;
    assert(aa.length == 2);
    assert(aa.remove(0));
    aa[0] = V.init;
    assert(aa.length == 2);
    assert(aa == [0 : V.init, 1 : V.init]);
}

void testTombstonePurging()
{
    int[int] aa;
    foreach (i; 0 .. 6)
        aa[i] = i;
    foreach (i; 0 .. 6)
        assert(aa.remove(i));
    foreach (i; 6 .. 10)
        aa[i] = i;
    assert(aa.length == 4);
    foreach (i; 6 .. 10)
        assert(i in aa);
}

void testClear()
{
    int[int] aa;
    assert(aa.length == 0);
    foreach (i; 0 .. 100)
        aa[i] = i * 2;
    assert(aa.length == 100);
    auto aa2 = aa;
    assert(aa2.length == 100);
    aa.clear();
    assert(aa.length == 0);
    assert(aa2.length == 0);

    aa2[5] = 6;
    assert(aa.length == 1);
    assert(aa[5] == 6);
}

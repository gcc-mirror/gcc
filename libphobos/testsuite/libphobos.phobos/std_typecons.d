@safe unittest
{
    import std.typecons;

    alias Coord = Tuple!(int, "x", int, "y", int, "z");
    Coord c;
    c[1] = 1;       // access by index
    c.z = 1;        // access by given name
    assert(c == Coord(0, 1, 1));

    // names can be omitted, types can be mixed
    alias DictEntry = Tuple!(string, int);
    auto dict = DictEntry("seven", 7);

    // element types can be inferred
    assert(tuple(2, 3, 4)[1] == 3);
    // type inference works with names too
    auto tup = tuple!("x", "y", "z")(2, 3, 4);
    assert(tup.y == 3);
}

@safe unittest
{
    import std.typecons;

    class Widget
    {
        void foo() const @safe {}
    }
    const w1 = new Widget, w2 = new Widget;
    w1.foo();
    // w1 = w2 would not work; can't rebind const object

    auto r = Rebindable!(const Widget)(w1);
    // invoke method as if r were a Widget object
    r.foo();
    // rebind r to refer to another object
    r = w2;
}

@safe unittest
{
    import std.typecons;

    struct S
    {
        int i;
        this(int i){this.i = i;}
    }
    Unique!S produce()
    {
        // Construct a unique instance of S on the heap
        Unique!S ut = new S(5);
        // Implicit transfer of ownership
        return ut;
    }
    // Borrow a unique resource by ref
    void increment(ref Unique!S ur)
    {
        ur.i++;
    }
    void consume(Unique!S u2)
    {
        assert(u2.i == 6);
        // Resource automatically deleted here
    }
    Unique!S u1;
    assert(u1.isEmpty);
    u1 = produce();
    assert(u1.i == 5);
    increment(u1);
    assert(u1.i == 6);
    //consume(u1); // Error: u1 is not copyable
    // Transfer ownership of the resource
    consume(u1.release);
    assert(u1.isEmpty);
}

@safe unittest
{
    import std.typecons;

            import std.meta : AliasSeq;
            alias Fields = Tuple!(int, "id", string, float);
            static assert(is(Fields.Types == AliasSeq!(int, string, float)));
        
}

@safe unittest
{
    import std.typecons;

            import std.meta : AliasSeq;
            alias Fields = Tuple!(int, "id", string, float);
            static assert(Fields.fieldNames == AliasSeq!("id", "", ""));
        
}

@safe unittest
{
    import std.typecons;

            auto t1 = tuple(1, " hello ", 'a');
            assert(t1.toString() == `Tuple!(int, string, char)(1, " hello ", 'a')`);

            void takeSeveralTypes(int n, string s, bool b)
            {
                assert(n == 4 && s == "test" && b == false);
            }

            auto t2 = tuple(4, "test", false);
            //t.expand acting as a list of values
            takeSeveralTypes(t2.expand);
        
}

@safe unittest
{
    import std.typecons;

            alias ISD = Tuple!(int, string, double);
            auto tup = ISD(1, "test", 3.2);
            assert(tup.toString() == `Tuple!(int, string, double)(1, "test", 3.2)`);
        
}

@safe unittest
{
    import std.typecons;

            int[2] ints;
            Tuple!(int, int) t = ints;
        
}

@safe unittest
{
    import std.typecons;

            alias IntVec = Tuple!(int, int, int);
            alias DubVec = Tuple!(double, double, double);

            IntVec iv = tuple(1, 1, 1);

            //Ok, int can implicitly convert to double
            DubVec dv = iv;
            //Error: double cannot implicitly convert to int
            //IntVec iv2 = dv;
        
}

@safe unittest
{
    import std.typecons;

            Tuple!(int, string) t1 = tuple(1, "test");
            Tuple!(double, string) t2 =  tuple(1.0, "test");
            //Ok, int can be compared with double and
            //both have a value of 1
            assert(t1 == t2);
        
}

@safe unittest
{
    import std.typecons;

            auto tup1 = tuple(1, 1, 1);
            auto tup2 = tuple(1, 100, 100);
            assert(tup1 < tup2);

            //Only the first result matters for comparison
            tup1[0] = 2;
            assert(tup1 > tup2);
        
}

@safe unittest
{
    import std.typecons;

            auto t0 = tuple(4, "hello");

            auto t0Named = t0.rename!("val", "tag");
            assert(t0Named.val == 4);
            assert(t0Named.tag == "hello");

            Tuple!(float, "dat", size_t[2], "pos") t1;
            t1.pos = [2, 1];
            auto t1Named = t1.rename!"height";
            t1Named.height = 3.4f;
            assert(t1Named.height == 3.4f);
            assert(t1Named.pos == [2, 1]);
            t1Named.rename!"altitude".altitude = 5;
            assert(t1Named.height == 5);

            Tuple!(int, "a", int, int, "c") t2;
            t2 = tuple(3,4,5);
            auto t2Named = t2.rename!("", "b");
            // "a" no longer has a name
            static assert(!__traits(hasMember, typeof(t2Named), "a"));
            assert(t2Named[0] == 3);
            assert(t2Named.b == 4);
            assert(t2Named.c == 5);

            // not allowed to specify more names than the tuple has members
            static assert(!__traits(compiles, t2.rename!("a","b","c","d")));

            // use it in a range pipeline
            import std.range : iota, zip;
            import std.algorithm.iteration : map, sum;
            auto res = zip(iota(1, 4), iota(10, 13))
                .map!(t => t.rename!("a", "b"))
                .map!(t => t.a * t.b)
                .sum;
            assert(res == 68);

            const tup = Tuple!(int, "a", int, "b")(2, 3);
            const renamed = tup.rename!("c", "d");
            assert(renamed.c + renamed.d == 5);
        
}

@safe unittest
{
    import std.typecons;

            //replacing names by their current name

            Tuple!(float, "dat", size_t[2], "pos") t1;
            t1.pos = [2, 1];
            auto t1Named = t1.rename!(["dat": "height"]);
            t1Named.height = 3.4;
            assert(t1Named.pos == [2, 1]);
            t1Named.rename!(["height": "altitude"]).altitude = 5;
            assert(t1Named.height == 5);

            Tuple!(int, "a", int, "b") t2;
            t2 = tuple(3, 4);
            auto t2Named = t2.rename!(["a": "b", "b": "c"]);
            assert(t2Named.b == 3);
            assert(t2Named.c == 4);

            const t3 = Tuple!(int, "a", int, "b")(3, 4);
            const t3Named = t3.rename!(["a": "b", "b": "c"]);
            assert(t3Named.b == 3);
            assert(t3Named.c == 4);
        
}

@system unittest
{
    import std.typecons;

            //replace names by their position

            Tuple!(float, "dat", size_t[2], "pos") t1;
            t1.pos = [2, 1];
            auto t1Named = t1.rename!([0: "height"]);
            t1Named.height = 3.4;
            assert(t1Named.pos == [2, 1]);
            t1Named.rename!([0: "altitude"]).altitude = 5;
            assert(t1Named.height == 5);

            Tuple!(int, "a", int, "b", int, "c") t2;
            t2 = tuple(3, 4, 5);
            auto t2Named = t2.rename!([0: "c", 2: "a"]);
            assert(t2Named.a == 5);
            assert(t2Named.b == 4);
            assert(t2Named.c == 3);
        
}

@safe unittest
{
    import std.typecons;

            Tuple!(int, string, float, double) a;
            a[1] = "abc";
            a[2] = 4.5;
            auto s = a.slice!(1, 3);
            static assert(is(typeof(s) == Tuple!(string, float)));
            assert(s[0] == "abc" && s[1] == 4.5);

            // https://issues.dlang.org/show_bug.cgi?id=15645
            Tuple!(int, short, bool, double) b;
            static assert(!__traits(compiles, b.slice!(2, 4)));
        
}

@safe unittest
{
    import std.typecons;

            import std.format : format;

            Tuple!(int, double)[3] tupList = [ tuple(1, 1.0), tuple(2, 4.0), tuple(3, 9.0) ];

            // Default format
            assert(format("%s", tuple("a", 1)) == `Tuple!(string, int)("a", 1)`);

            // One Format for each individual component
            assert(format("%(%#x v %.4f w %#x%)", tuple(1, 1.0, 10))         == `0x1 v 1.0000 w 0xa`);
            assert(format(  "%#x v %.4f w %#x"  , tuple(1, 1.0, 10).expand)  == `0x1 v 1.0000 w 0xa`);

            // One Format for all components
            assert(format("%(>%s<%| & %)", tuple("abc", 1, 2.3, [4, 5])) == `>abc< & >1< & >2.3< & >[4, 5]<`);

            // Array of Tuples
            assert(format("%(%(f(%d) = %.1f%);  %)", tupList) == `f(1) = 1.0;  f(2) = 4.0;  f(3) = 9.0`);
        
}

@safe unittest
{
    import std.typecons;

            import std.exception : assertThrown;
            import std.format : format, FormatException;

            // Error: %( %) missing.
            assertThrown!FormatException(
                format("%d, %f", tuple(1, 2.0)) == `1, 2.0`
            );

            // Error: %( %| %) missing.
            assertThrown!FormatException(
                format("%d", tuple(1, 2)) == `1, 2`
            );

            // Error: %d inadequate for double
            assertThrown!FormatException(
                format("%(%d%|, %)", tuple(1, 2.0)) == `1, 2.0`
            );
        
}

@safe unittest
{
    import std.typecons;

    Tuple!(int, int) point;
    // assign coordinates
    point[0] = 5;
    point[1] = 6;
    // read coordinates
    auto x = point[0];
    auto y = point[1];
}

@safe unittest
{
    import std.typecons;

    alias Entry = Tuple!(int, "index", string, "value");
    Entry e;
    e.index = 4;
    e.value = "Hello";
    assert(e[1] == "Hello");
    assert(e[0] == 4);
}

@safe unittest
{
    import std.typecons;

    Tuple!(int, "x", int, "y") point1;
    Tuple!(int, int) point2;
    assert(!is(typeof(point1) == typeof(point2)));
}

@safe unittest
{
    import std.typecons;

    import std.algorithm.iteration : sum;
    import std.range : only;
    auto t = tuple(1, 2);
    assert(t.expand.only.sum == 3);
}

@safe unittest
{
    import std.typecons;

    import std.meta : AliasSeq;
    auto t = tuple(1, "2") ~ tuple(ushort(42), true);
    static assert(is(t.Types == AliasSeq!(int, string, ushort, bool)));
    assert(t[1] == "2");
    assert(t[2] == 42);
    assert(t[3] == true);
}

@safe unittest
{
    import std.typecons;

    auto tup = tuple(1, "2");
    assert(tup.reverse == tuple("2", 1));
}

@safe unittest
{
    import std.typecons;

    auto value = tuple(5, 6.7, "hello");
    assert(value[0] == 5);
    assert(value[1] == 6.7);
    assert(value[2] == "hello");

    // Field names can be provided.
    auto entry = tuple!("index", "value")(4, "Hello");
    assert(entry.index == 4);
    assert(entry.value == "Hello");
}

@safe unittest
{
    import std.typecons;

    static assert(isTuple!(Tuple!()));
    static assert(isTuple!(Tuple!(int)));
    static assert(isTuple!(Tuple!(int, real, string)));
    static assert(isTuple!(Tuple!(int, "x", real, "y")));
    static assert(isTuple!(Tuple!(int, Tuple!(real), string)));
}

@safe unittest
{
    import std.typecons;

    class Widget { int x; int y() @safe const { return x; } }
    const a = new Widget;
    // Fine
    a.y();
    // error! can't modify const a
    // a.x = 5;
    // error! can't modify const a
    // a = new Widget;
}

@safe unittest
{
    import std.typecons;

    class Widget { int x; int y() const @safe { return x; } }
    auto a = Rebindable!(const Widget)(new Widget);
    // Fine
    a.y();
    // error! can't modify const a
    // a.x = 5;
    // Fine
    a = new Widget;
}

@safe unittest
{
    import std.typecons;

    import std.range.primitives : front, popFront;

    // simple version of std.algorithm.searching.maxElement
    typeof(R.init.front) maxElement(R)(R r)
    {
        auto max = rebindable(r.front);
        r.popFront;
        foreach (e; r)
            if (e > max)
                max = e; // Rebindable allows const-correct reassignment
        return max;
    }
    struct S
    {
        char[] arr;
        alias arr this; // for comparison
    }
    // can't convert to mutable
    const S cs;
    static assert(!__traits(compiles, { S s = cs; }));

    alias CS = const S;
    CS[] arr = [CS("harp"), CS("apple"), CS("pot")];
    CS ms = maxElement(arr);
    assert(ms.arr == "pot");
}

@system unittest
{
    import std.typecons;

    static struct S
    {
        int* ptr;
    }
    S s = S(new int);

    const cs = s;
    // Can't assign s.ptr to cs.ptr
    static assert(!__traits(compiles, {s = cs;}));

    Rebindable!(const S) rs = s;
    assert(rs.ptr is s.ptr);
    // rs.ptr is const
    static assert(!__traits(compiles, {rs.ptr = null;}));

    // Can't assign s.ptr to rs.ptr
    static assert(!__traits(compiles, {s = rs;}));

    const S cs2 = rs;
    // Rebind rs
    rs = cs2;
    rs = S();
    assert(rs.ptr is null);
}

@system unittest
{
    import std.typecons;

    class C
    {
        int payload;
        this(int p) { payload = p; }
    }
    const c = new C(1);

    auto c2 = c.rebindable;
    assert(c2.payload == 1);
    // passing Rebindable to rebindable
    c2 = c2.rebindable;

    c2 = new C(2);
    assert(c2.payload == 2);

    const c3 = c2.get;
    assert(c3.payload == 2);
}

@safe unittest
{
    import std.typecons;

    immutable struct S
    {
        int[] array;
    }
    auto s1 = [3].idup.rebindable;
    s1 = [4].idup.rebindable;
    assert(s1 == [4]);
}

@system unittest
{
    import std.typecons;

    class C
    {
        int payload;
        this(int p) { payload = p; }
    }
    const c = new C(1);

    auto c2 = c.rebindable;
    assert(c2.payload == 1);
    // passing Rebindable to rebindable
    c2 = c2.rebindable;
    assert(c2.payload == 1);
}

@system unittest
{
    import std.typecons;

    class Data {}

    static shared(Data) a;
    static UnqualRef!(shared Data) b;

    import core.thread;

    auto thread = new core.thread.Thread({
        a = new shared Data();
        b = new shared Data();
    });

    thread.start();
    thread.join();

    assert(a !is null);
    assert(b is null);
}

@safe unittest
{
    import std.typecons;

    struct Banner {
        mixin(alignForSize!(byte[6], double)(["name", "height"]));
    }
}

@safe unittest
{
    import std.typecons;

        Nullable!int empty;
        Nullable!int a = 42;
        Nullable!int b = 42;
        Nullable!int c = 27;

        assert(empty == empty);
        assert(empty == Nullable!int.init);
        assert(empty != a);
        assert(empty != b);
        assert(empty != c);

        assert(a == b);
        assert(a != c);

        assert(empty != 42);
        assert(a == 42);
        assert(c != 42);
    
}

@safe unittest
{
    import std.typecons;

        Nullable!int ni;
        assert(ni.isNull);

        ni = 0;
        assert(!ni.isNull);
    
}

@safe unittest
{
    import std.typecons;

        Nullable!int ni = 0;
        assert(!ni.isNull);

        ni.nullify();
        assert(ni.isNull);
    
}

@safe unittest
{
    import std.typecons;

        //Passes
        Nullable!(int*) npi;
        assert(npi.isNull);

        //Passes?!
        npi = null;
        assert(!npi.isNull);
    
}

@safe unittest
{
    import std.typecons;

    struct CustomerRecord
    {
        string name;
        string address;
        int customerNum;
    }

    Nullable!CustomerRecord getByName(string name)
    {
        //A bunch of hairy stuff

        return Nullable!CustomerRecord.init;
    }

    auto queryResult = getByName("Doe, John");
    if (!queryResult.isNull)
    {
        //Process Mr. Doe's customer record
        auto address = queryResult.get.address;
        auto customerNum = queryResult.get.customerNum;

        //Do some things with this customer's info
    }
    else
    {
        //Add the customer to the database
    }
}

@system unittest
{
    import std.typecons;

    import std.exception : assertThrown;

    auto a = 42.nullable;
    assert(!a.isNull);
    assert(a.get == 42);

    a.nullify();
    assert(a.isNull);
    assertThrown!Throwable(a.get);
}

@safe unittest
{
    import std.typecons;

    import std.algorithm.iteration : each, joiner;
    Nullable!int a = 42;
    Nullable!int b;
    // Add each value to an array
    int[] arr;
    a.each!((n) => arr ~= n);
    assert(arr == [42]);
    b.each!((n) => arr ~= n);
    assert(arr == [42]);
    // Take first value from an array of Nullables
    Nullable!int[] c = new Nullable!int[](10);
    c[7] = Nullable!int(42);
    assert(c.joiner.front == 42);
}

@safe unittest
{
    import std.typecons;

    Nullable!(int, -1) ni;
    //Initialized to "null" state
    assert(ni.isNull);

    ni = 0;
    assert(!ni.isNull);
}

@safe unittest
{
    import std.typecons;

    Nullable!(int, -1) ni = 0;
    assert(!ni.isNull);

    ni = -1;
    assert(ni.isNull);
}

@system unittest
{
    import std.typecons;

    //Passes
    enum nullVal = cast(int*) 0xCAFEBABE;
    Nullable!(int*, nullVal) npi;
    assert(npi.isNull);

    //Passes?!
    npi = null;
    assert(!npi.isNull);
}

@system unittest
{
    import std.typecons;

    import std.exception : assertThrown, assertNotThrown;

    Nullable!(int, -1) ni;
    //`get` is implicitly called. Will throw
    //an error in non-release mode
    assertThrown!Throwable(ni == 0);

    ni = 0;
    assertNotThrown!Throwable(ni == 0);
}

@safe unittest
{
    import std.typecons;

    Nullable!(size_t, size_t.max) indexOf(string[] haystack, string needle)
    {
        //Find the needle, returning -1 if not found

        return Nullable!(size_t, size_t.max).init;
    }

    void sendLunchInvite(string name)
    {
    }

    //It's safer than C...
    auto coworkers = ["Jane", "Jim", "Marry", "Fred"];
    auto pos = indexOf(coworkers, "Bob");
    if (!pos.isNull)
    {
        //Send Bob an invitation to lunch
        sendLunchInvite(coworkers[pos]);
    }
    else
    {
        //Bob not found; report the error
    }

    //And there's no overhead
    static assert(Nullable!(size_t, size_t.max).sizeof == size_t.sizeof);
}

@system unittest
{
    import std.typecons;

    import std.exception : assertThrown;

    Nullable!(int, int.min) a;
    assert(a.isNull);
    assertThrown!Throwable(a.get);
    a = 5;
    assert(!a.isNull);
    assert(a == 5);
    static assert(a.sizeof == int.sizeof);
}

@safe unittest
{
    import std.typecons;

    auto a = nullable!(int.min)(8);
    assert(a == 8);
    a.nullify();
    assert(a.isNull);
}

nothrow pure @nogc @safe unittest
{
    import std.typecons;

    alias toFloat = i => cast(float) i;

    Nullable!int sample;

    // apply(null) results in a null `Nullable` of the function's return type.
    Nullable!float f = sample.apply!toFloat;
    assert(sample.isNull && f.isNull);

    sample = 3;

    // apply(non-null) calls the function and wraps the result in a `Nullable`.
    f = sample.apply!toFloat;
    assert(!sample.isNull && !f.isNull);
    assert(f.get == 3.0f);
}

nothrow pure @nogc @safe unittest
{
    import std.typecons;

    alias greaterThree = i => (i > 3) ? i.nullable : Nullable!(typeof(i)).init;

    Nullable!int sample;

    // when the function already returns a `Nullable`, that `Nullable` is not wrapped.
    auto result = sample.apply!greaterThree;
    assert(sample.isNull && result.isNull);

    // The function may decide to return a null `Nullable`.
    sample = 3;
    result = sample.apply!greaterThree;
    assert(!sample.isNull && result.isNull);

    // Or it may return a value already wrapped in a `Nullable`.
    sample = 4;
    result = sample.apply!greaterThree;
    assert(!sample.isNull && !result.isNull);
    assert(result.get == 4);
}

@safe unittest
{
    import std.typecons;

        NullableRef!int nr = new int(42);
        assert(nr == 42);

        int* n = new int(1);
        nr.bind(n);
        assert(nr == 1);
    
}

@safe unittest
{
    import std.typecons;

        NullableRef!int nr;
        assert(nr.isNull);

        int* n = new int(42);
        nr.bind(n);
        assert(!nr.isNull && nr == 42);
    
}

@safe unittest
{
    import std.typecons;

        NullableRef!int nr = new int(42);
        assert(!nr.isNull);

        nr.nullify();
        assert(nr.isNull);
    
}

@system unittest
{
    import std.typecons;

        import std.exception : assertThrown, assertNotThrown;

        NullableRef!int nr;
        assert(nr.isNull);
        assertThrown!Throwable(nr = 42);

        nr.bind(new int(0));
        assert(!nr.isNull);
        assertNotThrown!Throwable(nr = 42);
        assert(nr == 42);
    
}

@system unittest
{
    import std.typecons;

        import std.exception : assertThrown, assertNotThrown;

        NullableRef!int nr;
        //`get` is implicitly called. Will throw
        //an error in non-release mode
        assertThrown!Throwable(nr == 0);

        nr.bind(new int(0));
        assertNotThrown!Throwable(nr == 0);
    
}

@system unittest
{
    import std.typecons;

    import std.exception : assertThrown;

    int x = 5, y = 7;
    auto a = nullableRef(&x);
    assert(!a.isNull);
    assert(a == 5);
    assert(x == 5);
    a = 42;
    assert(x == 42);
    assert(!a.isNull);
    assert(a == 42);
    a.nullify();
    assert(x == 42);
    assert(a.isNull);
    assertThrown!Throwable(a.get);
    assertThrown!Throwable(a = 71);
    a.bind(&y);
    assert(a == 7);
    y = 135;
    assert(a == 135);
}

@system unittest
{
    import std.typecons;

    import std.math.traits : isNaN;

    static abstract class C
    {
        int m_value;
        this(int v) { m_value = v; }
        int value() @property { return m_value; }

        abstract real realValue() @property;
        abstract void doSomething();
    }

    auto c = new BlackHole!C(42);
    assert(c.value == 42);

    // Returns real.init which is NaN
    assert(c.realValue.isNaN);
    // Abstract functions are implemented as do-nothing
    c.doSomething();
}

@system unittest
{
    import std.typecons;

    import std.exception : assertThrown;

    static class C
    {
        abstract void notYetImplemented();
    }

    auto c = new WhiteHole!C;
    assertThrown!NotImplementedError(c.notYetImplemented()); // throws an Error
}

@system unittest
{
    import std.typecons;

    import std.exception : assertThrown;
    // nothrow
    {
        interface I_1
        {
            void foo();
            void bar() nothrow;
        }
        auto o = new WhiteHole!I_1;
        assertThrown!NotImplementedError(o.foo());
        assertThrown!NotImplementedError(o.bar());
    }
    // doc example
    {
        static class C
        {
            abstract void notYetImplemented();
        }

        auto c = new WhiteHole!C;
        try
        {
            c.notYetImplemented();
            assert(0);
        }
        catch (Error e) {}
    }
}

@system unittest
{
    import std.typecons;

    interface PackageSupplier
    {
        int foo();
        int bar();
    }

    static abstract class AbstractFallbackPackageSupplier : PackageSupplier
    {
        protected PackageSupplier default_, fallback;

        this(PackageSupplier default_, PackageSupplier fallback)
        {
            this.default_ = default_;
            this.fallback = fallback;
        }

        abstract int foo();
        abstract int bar();
    }

    template fallback(T, alias func)
    {
        import std.format : format;
        // for all implemented methods:
        // - try default first
        // - only on a failure run & return fallback
        enum fallback = q{
            try
            {
                return default_.%1$s(args);
            }
            catch (Exception)
            {
                return fallback.%1$s(args);
            }
        }.format(__traits(identifier, func));
    }

    // combines two classes and use the second one as fallback
    alias FallbackPackageSupplier = AutoImplement!(AbstractFallbackPackageSupplier, fallback);

    class FailingPackageSupplier : PackageSupplier
    {
        int foo(){ throw new Exception("failure"); }
        int bar(){ return 2;}
    }

    class BackupPackageSupplier : PackageSupplier
    {
        int foo(){ return -1; }
        int bar(){ return -1;}
    }

    auto registry = new FallbackPackageSupplier(new FailingPackageSupplier(), new BackupPackageSupplier());

    assert(registry.foo() == -1);
    assert(registry.bar() == 2);
}

@system unittest
{
    import std.typecons;

    alias BlackHole(Base) = AutoImplement!(Base, generateEmptyFunction);

    interface I
    {
        int foo();
        string bar();
    }

    auto i = new BlackHole!I();
    // generateEmptyFunction returns the default value of the return type without doing anything
    assert(i.foo == 0);
    assert(i.bar is null);
}

@system unittest
{
    import std.typecons;

    import std.exception : assertThrown;

    alias WhiteHole(Base) = AutoImplement!(Base, generateAssertTrap);

    interface I
    {
        int foo();
        string bar();
    }

    auto i = new WhiteHole!I();
    // generateAssertTrap throws an exception for every unimplemented function of the interface
    assertThrown!NotImplementedError(i.foo);
    assertThrown!NotImplementedError(i.bar);
}

@system unittest
{
    import std.typecons;

    interface Quack
    {
        int quack();
        @property int height();
    }
    interface Flyer
    {
        @property int height();
    }
    class Duck : Quack
    {
        int quack() { return 1; }
        @property int height() { return 10; }
    }
    class Human
    {
        int quack() { return 2; }
        @property int height() { return 20; }
    }

    Duck d1 = new Duck();
    Human h1 = new Human();

    interface Refleshable
    {
        int reflesh();
    }

    // does not have structural conformance
    static assert(!__traits(compiles, d1.wrap!Refleshable));
    static assert(!__traits(compiles, h1.wrap!Refleshable));

    // strict upcast
    Quack qd = d1.wrap!Quack;
    assert(qd is d1);
    assert(qd.quack() == 1);    // calls Duck.quack
    // strict downcast
    Duck d2 = qd.unwrap!Duck;
    assert(d2 is d1);

    // structural upcast
    Quack qh = h1.wrap!Quack;
    assert(qh.quack() == 2);    // calls Human.quack
    // structural downcast
    Human h2 = qh.unwrap!Human;
    assert(h2 is h1);

    // structural upcast (two steps)
    Quack qx = h1.wrap!Quack;   // Human -> Quack
    Flyer fx = qx.wrap!Flyer;   // Quack -> Flyer
    assert(fx.height == 20);    // calls Human.height
    // structural downcast (two steps)
    Quack qy = fx.unwrap!Quack; // Flyer -> Quack
    Human hy = qy.unwrap!Human; // Quack -> Human
    assert(hy is h1);
    // structural downcast (one step)
    Human hz = fx.unwrap!Human; // Flyer -> Human
    assert(hz is h1);
}

@system unittest
{
    import std.typecons;

    import std.traits : FunctionAttribute, functionAttributes;
    interface A { int run(); }
    interface B { int stop(); @property int status(); }
    class X
    {
        int run() { return 1; }
        int stop() { return 2; }
        @property int status() { return 3; }
    }

    auto x = new X();
    auto ab = x.wrap!(A, B);
    A a = ab;
    B b = ab;
    assert(a.run() == 1);
    assert(b.stop() == 2);
    assert(b.status == 3);
    static assert(functionAttributes!(typeof(ab).status) & FunctionAttribute.property);
}

@system unittest
{
    import std.typecons;

    import core.exception : AssertError;
    import std.exception : assertThrown;

    struct Foo
    {
        int a = 42;
    }

    SafeRefCounted!(Foo, RefCountedAutoInitialize.yes) rcAuto;
    SafeRefCounted!(Foo, RefCountedAutoInitialize.no) rcNoAuto;

    assert(rcAuto.refCountedPayload.a == 42);

    assertThrown!AssertError(rcNoAuto.refCountedPayload);
    rcNoAuto.refCountedStore.ensureInitialized;
    assert(rcNoAuto.refCountedPayload.a == 42);
}

pure @system nothrow @nogc unittest
{
    import std.typecons;

    // A pair of an `int` and a `size_t` - the latter being the
    // reference count - will be dynamically allocated
    auto rc1 = SafeRefCounted!int(5);
    assert(rc1 == 5);
    // No more allocation, add just one extra reference count
    auto rc2 = rc1;
    // Reference semantics
    rc2 = 42;
    assert(rc1 == 42);
    // the pair will be freed when rc1 and rc2 go out of scope
}

@safe pure nothrow unittest
{
    import std.typecons;

    auto rcInt = safeRefCounted(5);
    assert(rcInt.borrow!(theInt => theInt) == 5);
    auto sameInt = rcInt;
    assert(sameInt.borrow!"a" == 5);

    // using `ref` in the function
    auto arr = [0, 1, 2, 3, 4, 5, 6];
    sameInt.borrow!(ref (x) => arr[x]) = 10;
    assert(arr == [0, 1, 2, 3, 4, 10, 6]);

    // modifying the payload via an alias
    sameInt.borrow!"a*=2";
    assert(rcInt.borrow!"a" == 10);
}

@system unittest
{
    import std.typecons;

    static struct File
    {
        static size_t nDestroyed;
        string name;
        @disable this(this); // not copyable
        ~this() { name = null; ++nDestroyed; }
    }

    auto file = File("name");
    assert(file.name == "name");
    // file cannot be copied and has unique ownership
    static assert(!__traits(compiles, {auto file2 = file;}));

    assert(File.nDestroyed == 0);

    // make the file ref counted to share ownership
    // Note:
    //   We write a compound statement (brace-delimited scope) in which all `SafeRefCounted!File` handles are created and deleted.
    //   This allows us to see (after the scope) what happens after all handles have been destroyed.
    {
        // We move the content of `file` to a separate (and heap-allocated) `File` object,
        // managed-and-accessed via one-or-multiple (initially: one) `SafeRefCounted!File` objects ("handles").
        // This "moving":
        //   (1) invokes `file`'s destructor (=> `File.nDestroyed` is incremented from 0 to 1 and `file.name` becomes `null`);
        //   (2) overwrites `file` with `File.init` (=> `file.name` becomes `null`).
        // It appears that writing `name = null;` in the destructor is redundant,
        // but please note that (2) is only performed if `File` defines a destructor (or post-blit operator),
        // and in the absence of the `nDestroyed` instrumentation there would have been no reason to define a destructor.
        import std.algorithm.mutation : move;
        auto rcFile = safeRefCounted(move(file));
        assert(rcFile.name == "name");
        assert(File.nDestroyed == 1);
        assert(file.name == null);

        // We create another `SafeRefCounted!File` handle to the same separate `File` object.
        // While any of the handles is still alive, the `File` object is kept alive (=> `File.nDestroyed` is not modified).
        auto rcFile2 = rcFile;
        assert(rcFile.refCountedStore.refCount == 2);
        assert(File.nDestroyed == 1);
    }
    // The separate `File` object is deleted when the last `SafeRefCounted!File` handle is destroyed
    // (i.e. at the closing brace of the compound statement above, which destroys both handles: `rcFile` and `rcFile2`)
    // (=> `File.nDestroyed` is incremented again, from 1 to 2):
    assert(File.nDestroyed == 2);
}

@safe unittest
{
    import std.typecons;

    struct MyInt
    {
        private int value;
        mixin Proxy!value;

        this(int n){ value = n; }
    }

    MyInt n = 10;

    // Enable operations that original type has.
    ++n;
    assert(n == 11);
    assert(n * 2 == 22);

    void func(int n) { }

    // Disable implicit conversions to original type.
    //int x = n;
    //func(n);
}

@safe unittest
{
    import std.typecons;

    struct NewIntType
    {
        //Won't work; the literal '1'
        //is an rvalue, not an lvalue
        //mixin Proxy!1;

        //Okay, n is an lvalue
        int n;
        mixin Proxy!n;

        this(int n) { this.n = n; }
    }

    NewIntType nit = 0;
    nit++;
    assert(nit == 1);


    struct NewObjectType
    {
        Object obj;
        //Ok, obj is an lvalue
        mixin Proxy!obj;

        this (Object o) { obj = o; }
    }

    NewObjectType not = new Object();
    assert(__traits(compiles, not.toHash()));
}

@safe unittest
{
    import std.typecons;

    import std.math.traits : isInfinity;

    float f = 1.0;
    assert(!f.isInfinity);

    struct NewFloat
    {
        float _;
        mixin Proxy!_;

        this(float f) { _ = f; }
    }

    NewFloat nf = 1.0f;
    assert(!nf.isInfinity);
}

@safe unittest
{
    import std.typecons;

        import std.conv : to;

        int i = 123;
        auto td = Typedef!int(i);
        assert(i.to!string == td.to!string);
    
}

@safe unittest
{
    import std.typecons;

    alias MyInt = Typedef!int;
    MyInt foo = 10;
    foo++;
    assert(foo == 11);
}

@safe unittest
{
    import std.typecons;

    alias MyIntInit = Typedef!(int, 42);
    static assert(is(TypedefType!MyIntInit == int));
    static assert(MyIntInit() == 42);
}

@safe unittest
{
    import std.typecons;

    alias MyInt = Typedef!int;
    static void takeInt(int) {}
    static void takeMyInt(MyInt) {}

    int i;
    takeInt(i);    // ok
    static assert(!__traits(compiles, takeMyInt(i)));

    MyInt myInt;
    static assert(!__traits(compiles, takeInt(myInt)));
    takeMyInt(myInt);  // ok
}

@safe unittest
{
    import std.typecons;

    alias TypeInt1 = Typedef!int;
    alias TypeInt2 = Typedef!int;

    // The two Typedefs are the same type.
    static assert(is(TypeInt1 == TypeInt2));

    alias MoneyEuros = Typedef!(float, float.init, "euros");
    alias MoneyDollars = Typedef!(float, float.init, "dollars");

    // The two Typedefs are _not_ the same type.
    static assert(!is(MoneyEuros == MoneyDollars));
}

@safe unittest
{
    import std.typecons;

    import std.conv : to;

    alias MyInt = Typedef!int;
    static assert(is(TypedefType!MyInt == int));

    /// Instantiating with a non-Typedef will return that type
    static assert(is(TypedefType!int == int));

    string num = "5";

    // extract the needed type
    MyInt myInt = MyInt( num.to!(TypedefType!MyInt) );
    assert(myInt == 5);

    // cast to the underlying type to get the value that's being wrapped
    int x = cast(TypedefType!MyInt) myInt;

    alias MyIntInit = Typedef!(int, 42);
    static assert(is(TypedefType!MyIntInit == int));
    static assert(MyIntInit() == 42);
}

@system unittest
{
    import std.typecons;

    class A
    {
        int x;
        this()     {x = 0;}
        this(int i){x = i;}
        ~this()    {}
    }

    // Standard usage, constructing A on the stack
    auto a1 = scoped!A();
    a1.x = 42;

    // Result of `scoped` call implicitly converts to a class reference
    A aRef = a1;
    assert(aRef.x == 42);

    // Scoped destruction
    {
        auto a2 = scoped!A(1);
        assert(a2.x == 1);
        aRef = a2;
        // a2 is destroyed here, calling A's destructor
    }
    // aRef is now an invalid reference

    // Here the temporary scoped A is immediately destroyed.
    // This means the reference is then invalid.
    version (Bug)
    {
        // Wrong, should use `auto`
        A invalid = scoped!A();
    }

    // Restrictions
    version (Bug)
    {
        import std.algorithm.mutation : move;
        auto invalid = a1.move; // illegal, scoped objects can't be moved
    }
    static assert(!is(typeof({
        auto e1 = a1; // illegal, scoped objects can't be copied
        assert([a1][0].x == 42); // ditto
    })));
    static assert(!is(typeof({
        alias ScopedObject = typeof(a1);
        auto e2 = ScopedObject();  // illegal, must be built via scoped!A
        auto e3 = ScopedObject(1); // ditto
    })));

    // Use with alias
    alias makeScopedA = scoped!A;
    auto a3 = makeScopedA();
    auto a4 = makeScopedA(1);

    // Use as member variable
    struct B
    {
        typeof(scoped!A()) a; // note the trailing parentheses

        this(int i)
        {
            // construct member
            a = scoped!A(i);
        }
    }

    // Stack-allocate
    auto b1 = B(5);
    aRef = b1.a;
    assert(aRef.x == 5);
    destroy(b1); // calls A's destructor for b1.a
    // aRef is now an invalid reference

    // Heap-allocate
    auto b2 = new B(6);
    assert(b2.a.x == 6);
    destroy(*b2); // calls A's destructor for b2.a
}

@safe unittest
{
    import std.typecons;

    Flag!"abc" flag;

    assert(flag == Flag!"abc".no);
    assert(flag == No.abc);
    assert(!flag);
    if (flag) assert(0);
}

@safe unittest
{
    import std.typecons;

    auto flag = Yes.abc;

    assert(flag);
    assert(flag == Yes.abc);
    if (!flag) assert(0);
    if (flag) {} else assert(0);
}

@safe unittest
{
    import std.typecons;

    Flag!"abc" flag;

    assert(flag == Flag!"abc".no);
    assert(flag == No.abc);
    assert(!flag);
    if (flag) assert(0);
}

@safe unittest
{
    import std.typecons;

    auto flag = Yes.abc;

    assert(flag);
    assert(flag == Yes.abc);
    if (!flag) assert(0);
    if (flag) {} else assert(0);
}

@safe pure nothrow unittest
{
    import std.typecons;

    enum A
    {
        None,
        A = 1 << 0,
        B = 1 << 1,
        C = 1 << 2,
        D = 1 << 3,
    }

    static assert(isBitFlagEnum!A);
}

@safe pure nothrow unittest
{
    import std.typecons;

    enum B
    {
        A,
        B,
        C,
        D // D == 3
    }

    static assert(!isBitFlagEnum!B);
}

@safe pure nothrow unittest
{
    import std.typecons;

    enum C: double
    {
        A = 1 << 0,
        B = 1 << 1
    }

    static assert(!isBitFlagEnum!C);
}

@safe @nogc pure nothrow unittest
{
    import std.typecons;

    enum Enum
    {
        A = 1 << 0,
    }

    // A default constructed BitFlags has no value set
    immutable BitFlags!Enum flags_empty;
    assert(!flags_empty.A);

    // Value can be set with the | operator
    immutable flags_A = flags_empty | Enum.A;

    // and tested using property access
    assert(flags_A.A);

    // or the & operator
    assert(flags_A & Enum.A);
    // which commutes.
    assert(Enum.A & flags_A);
}

@safe @nogc pure nothrow unittest
{
    import std.typecons;

    enum Enum
    {
        None,
        A = 1 << 0,
        B = 1 << 1,
        C = 1 << 2
    }

    immutable BitFlags!Enum flags_empty;
    assert(!(flags_empty & (Enum.A | Enum.B | Enum.C)));
    assert(!(flags_empty & Enum.A) && !(flags_empty & Enum.B) && !(flags_empty & Enum.C));
}

@safe @nogc pure nothrow unittest
{
    import std.typecons;

    enum Enum
    {
        A = 1 << 0,
        B = 1 << 1,
        C = 1 << 2,
    }
    immutable BitFlags!Enum flags_AB = BitFlags!Enum(Enum.A, Enum.B);
    immutable BitFlags!Enum flags_BC = BitFlags!Enum(Enum.B, Enum.C);

    // Use the ~ operator for subtracting flags
    immutable BitFlags!Enum flags_B = flags_AB & ~BitFlags!Enum(Enum.A);
    assert(!flags_B.A && flags_B.B && !flags_B.C);

    // use & between BitFlags for intersection
    assert(flags_B == (flags_BC & flags_AB));
}

@safe @nogc pure nothrow unittest
{
    import std.typecons;

    enum Enum
    {
        A = 1 << 0,
        B = 1 << 1,
    }

    BitFlags!Enum flags_empty, temp, flags_AB;
    flags_AB = Enum.A | Enum.B;

    temp |= flags_AB;
    assert(temp == (flags_empty | flags_AB));

    temp = flags_empty;
    temp |= Enum.B;
    assert(temp == (flags_empty | Enum.B));

    temp = flags_empty;
    temp &= flags_AB;
    assert(temp == (flags_empty & flags_AB));

    temp = flags_empty;
    temp &= Enum.A;
    assert(temp == (flags_empty & Enum.A));
}

@safe @nogc pure nothrow unittest
{
    import std.typecons;

    enum Enum
    {
        A = 1 << 0,
        B = 1 << 1,
    }

    BitFlags!Enum flags;

    // BitFlags with no value set evaluate to false
    assert(!flags);

    // BitFlags with at least one value set evaluate to true
    flags |= Enum.A;
    assert(flags);

    // This can be useful to check intersection between BitFlags
    BitFlags!Enum flags_AB = Enum.A | Enum.B;
    assert(flags & flags_AB);
    assert(flags & Enum.A);

    // You can of course get you raw value out of flags
    auto value = cast(int) flags;
    assert(value == Enum.A);
}

@safe @nogc pure nothrow unittest
{
    import std.typecons;

    enum UnsafeEnum
    {
        A = 1,
        B = 2,
        C = 4,
        BC = B|C
    }
    static assert(!__traits(compiles, { BitFlags!UnsafeEnum flags; }));
    BitFlags!(UnsafeEnum, Yes.unsafe) flags;

    // property access tests for exact match of unsafe enums
    flags.B = true;
    assert(!flags.BC); // only B
    flags.C = true;
    assert(flags.BC); // both B and C
    flags.B = false;
    assert(!flags.BC); // only C

    // property access sets all bits of unsafe enum group
    flags = flags.init;
    flags.BC = true;
    assert(!flags.A && flags.B && flags.C);
    flags.A = true;
    flags.BC = false;
    assert(flags.A && !flags.B && !flags.C);
}

@safe unittest
{
    import std.typecons;

    static assert(
        is(ReplaceType!(int, string, int[]) == string[]) &&
        is(ReplaceType!(int, string, int[int]) == string[string]) &&
        is(ReplaceType!(int, string, const(int)[]) == const(string)[]) &&
        is(ReplaceType!(int, string, Tuple!(int[], float))
            == Tuple!(string[], float))
    );
}

@safe unittest
{
    import std.typecons;

    import std.traits : isArray;

    static assert(
        is(ReplaceTypeUnless!(isArray, int, string, int*) == string*) &&
        is(ReplaceTypeUnless!(isArray, int, string, int[]) == int[]) &&
        is(ReplaceTypeUnless!(isArray, int, string, Tuple!(int, int[]))
            == Tuple!(string, int[]))
   );
}

@safe @nogc nothrow pure unittest
{
    import std.typecons;

    Ternary a;
    assert(a == Ternary.unknown);

    assert(~Ternary.yes == Ternary.no);
    assert(~Ternary.no == Ternary.yes);
    assert(~Ternary.unknown == Ternary.unknown);
}

pure @system nothrow @nogc unittest
{
    import std.typecons;

    auto rc1 = RefCounted!int(5);
    assert(rc1 == 5);
    auto rc2 = rc1;
    rc2 = 42;
    assert(rc1 == 42);
}

@system unittest
{
    import std.typecons;

    static struct File
    {
        static size_t nDestroyed;
        string name;
        @disable this(this); // not copyable
        ~this() { name = null; ++nDestroyed; }
    }

    auto file = File("name");
    assert(file.name == "name");
    static assert(!__traits(compiles, {auto file2 = file;}));
    assert(File.nDestroyed == 0);

    {
        import std.algorithm.mutation : move;
        auto rcFile = refCounted(move(file));
        assert(rcFile.name == "name");
        assert(File.nDestroyed == 1);
        assert(file.name == null);

        auto rcFile2 = rcFile;
        assert(rcFile.refCountedStore.refCount == 2);
        assert(File.nDestroyed == 1);
    }

    assert(File.nDestroyed == 2);
}


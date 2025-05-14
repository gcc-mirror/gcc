@safe unittest
{
    import std.checkedint;

    int[] concatAndAdd(int[] a, int[] b, int offset)
    {
        // Aborts on overflow on size computation
        auto r = new int[(checked(a.length) + b.length).get];
        // Aborts on overflow on element computation
        foreach (i; 0 .. a.length)
            r[i] = (a[i] + checked(offset)).get;
        foreach (i; 0 .. b.length)
            r[i + a.length] = (b[i] + checked(offset)).get;
        return r;
    }
    assert(concatAndAdd([1, 2, 3], [4, 5], -1) == [0, 1, 2, 3, 4]);
}

@safe unittest
{
    import std.checkedint;

    auto x = (cast(byte) 127).checked!Saturate;
    assert(x == 127);
    x++;
    assert(x == 127);
}

@safe unittest
{
    import std.checkedint;

    auto x = 100.checked!WithNaN;
    assert(x == 100);
    x /= 0;
    assert(x.isNaN);
}

@safe unittest
{
    import std.checkedint;

    uint x = 1;
    auto y = x.checked!ProperCompare;
    assert(x < -1); // built-in comparison
    assert(y > -1); // ProperCompare
}

@safe unittest
{
    import std.checkedint;

    import std.exception : assertThrown;
    auto x = -1.checked!Throw;
    assertThrown(x / 0);
    assertThrown(x + int.min);
    assertThrown(x == uint.max);
}

@safe unittest
{
    import std.checkedint;

        auto x = checked(ubyte(42));
        static assert(is(typeof(x.get()) == ubyte));
        assert(x.get == 42);
        const y = checked(ubyte(42));
        static assert(is(typeof(y.get()) == const ubyte));
        assert(y.get == 42);
    
}

@safe unittest
{
    import std.checkedint;

            assert(Checked!short.min == -32768);
            assert(Checked!(short, WithNaN).min == -32767);
            assert(Checked!(uint, WithNaN).max == uint.max - 1);
        
}

@safe unittest
{
    import std.checkedint;

        auto a = checked(42L);
        assert(a == 42);
        auto b = Checked!long(4242); // convert 4242 to long
        assert(b == 4242);
    
}

@safe unittest
{
    import std.checkedint;

        Checked!long a;
        a = 42L;
        assert(a == 42);
        a = 4242;
        assert(a == 4242);
    
}

@safe unittest
{
    import std.checkedint;

        Checked!long a, b;
        a = b = 3;
        assert(a == 3 && b == 3);
    
}

@system unittest
{
    import std.checkedint;

        import std.conv : to;

        const a = to!long("1234");
        const b = to!(Checked!long)("1234");
        assert(a == b);
    
}

@safe unittest
{
    import std.checkedint;

        assert(cast(uint) checked(42) == 42);
        assert(cast(uint) checked!WithNaN(-42) == uint.max);
    
}

@safe unittest
{
    import std.checkedint;

        import std.traits : isUnsigned;

        static struct MyHook
        {
            static bool thereWereErrors;
            static bool hookOpEquals(L, R)(L lhs, R rhs)
            {
                if (lhs != rhs) return false;
                static if (isUnsigned!L && !isUnsigned!R)
                {
                    if (lhs > 0 && rhs < 0) thereWereErrors = true;
                }
                else static if (isUnsigned!R && !isUnsigned!L)
                    if (lhs < 0 && rhs > 0) thereWereErrors = true;
                // Preserve built-in behavior.
                return true;
            }
        }
        auto a = checked!MyHook(-42);
        assert(a == uint(-42));
        assert(MyHook.thereWereErrors);
        MyHook.thereWereErrors = false;
        assert(checked!MyHook(uint(-42)) == -42);
        assert(MyHook.thereWereErrors);
        static struct MyHook2
        {
            static bool hookOpEquals(L, R)(L lhs, R rhs)
            {
                return lhs == rhs;
            }
        }
        MyHook.thereWereErrors = false;
        assert(checked!MyHook2(uint(-42)) == a);
        // Hook on left hand side takes precedence, so no errors
        assert(!MyHook.thereWereErrors);
    
}

@system unittest
{
    import std.checkedint;

        import std.format;

        assert(format("%04d", checked(15)) == "0015");
        assert(format("0x%02x", checked(15)) == "0x0f");
    
}

@safe unittest
{
    import std.checkedint;

        import std.traits : isUnsigned;

        static struct MyHook
        {
            static bool thereWereErrors;
            static int hookOpCmp(L, R)(L lhs, R rhs)
            {
                static if (isUnsigned!L && !isUnsigned!R)
                {
                    if (rhs < 0 && rhs >= lhs)
                        thereWereErrors = true;
                }
                else static if (isUnsigned!R && !isUnsigned!L)
                {
                    if (lhs < 0 && lhs >= rhs)
                        thereWereErrors = true;
                }
                // Preserve built-in behavior.
                return lhs < rhs ? -1 : lhs > rhs;
            }
        }
        auto a = checked!MyHook(-42);
        assert(a > uint(42));
        assert(MyHook.thereWereErrors);
        static struct MyHook2
        {
            static int hookOpCmp(L, R)(L lhs, R rhs)
            {
                // Default behavior
                return lhs < rhs ? -1 : lhs > rhs;
            }
        }
        MyHook.thereWereErrors = false;
        assert(Checked!(uint, MyHook2)(uint(-42)) <= a);
        //assert(Checked!(uint, MyHook2)(uint(-42)) >= a);
        // Hook on left hand side takes precedence, so no errors
        assert(!MyHook.thereWereErrors);
        assert(a <= Checked!(uint, MyHook2)(uint(-42)));
        assert(MyHook.thereWereErrors);
    
}

@safe unittest
{
    import std.checkedint;

        static struct MyHook
        {
            static bool thereWereErrors;
            static L hookOpUnary(string x, L)(L lhs)
            {
                if (x == "-" && lhs == -lhs) thereWereErrors = true;
                return -lhs;
            }
        }
        auto a = checked!MyHook(long.min);
        assert(a == -a);
        assert(MyHook.thereWereErrors);
        auto b = checked!void(42);
        assert(++b == 43);
    
}

@safe unittest
{
    import std.checkedint;

        static struct MyHook
        {
            static bool thereWereErrors;
            static T onLowerBound(Rhs, T)(Rhs rhs, T bound)
            {
                thereWereErrors = true;
                return bound;
            }
            static T onUpperBound(Rhs, T)(Rhs rhs, T bound)
            {
                thereWereErrors = true;
                return bound;
            }
        }
        auto x = checked!MyHook(byte.min);
        x -= 1;
        assert(MyHook.thereWereErrors);
        MyHook.thereWereErrors = false;
        x = byte.max;
        x += 1;
        assert(MyHook.thereWereErrors);
    
}

@safe @nogc pure nothrow unittest
{
    import std.checkedint;

    // Hook that ignores all problems.
    static struct Ignore
    {
        @nogc nothrow pure @safe static:
        Dst onBadCast(Dst, Src)(Src src) { return cast(Dst) src; }
        Lhs onLowerBound(Rhs, T)(Rhs rhs, T bound) { return cast(T) rhs; }
        T onUpperBound(Rhs, T)(Rhs rhs, T bound) { return cast(T) rhs; }
        bool hookOpEquals(Lhs, Rhs)(Lhs lhs, Rhs rhs) { return lhs == rhs; }
        int hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs) { return (lhs > rhs) - (lhs < rhs); }
        typeof(~Lhs()) onOverflow(string x, Lhs)(ref Lhs lhs) { return mixin(x ~ "lhs"); }
        typeof(Lhs() + Rhs()) onOverflow(string x, Lhs, Rhs)(Lhs lhs, Rhs rhs)
        {
            static if (x == "/")
                return typeof(lhs / rhs).min;
            else
                return mixin("lhs" ~ x ~ "rhs");
        }
    }

    auto x = Checked!(int, Ignore)(5) + 7;
}

@safe unittest
{
    import std.checkedint;

    static assert(is(typeof(checked(42)) == Checked!int));
    assert(checked(42) == Checked!int(42));
    static assert(is(typeof(checked!WithNaN(42)) == Checked!(int, WithNaN)));
    assert(checked!WithNaN(42) == Checked!(int, WithNaN)(42));
}

@safe unittest
{
    import std.checkedint;

    void test(T)()
    {
        Checked!(int, Abort) x;
        x = 42;
        auto x1 = cast(T) x;
        assert(x1 == 42);
        //x1 += long(int.max);
    }
    test!short;
    test!(const short);
    test!(immutable short);
}

@safe unittest
{
    import std.checkedint;

    void test(T)()
    {
        Checked!(int, Throw) x;
        x = 42;
        auto x1 = cast(T) x;
        assert(x1 == 42);
        x = T.max + 1;
        import std.exception : assertThrown, assertNotThrown;
        assertThrown(cast(T) x);
        x = x.max;
        assertThrown(x += 42);
        assertThrown(x += 42L);
        x = x.min;
        assertThrown(-x);
        assertThrown(x -= 42);
        assertThrown(x -= 42L);
        x = -1;
        assertNotThrown(x == -1);
        assertThrown(x == uint(-1));
        assertNotThrown(x <= -1);
        assertThrown(x <= uint(-1));
    }
    test!short;
    test!(const short);
    test!(immutable short);
}

@safe unittest
{
    import std.checkedint;

        auto x = checked!Warn(-42);
        // Passes
        assert(x == -42);
        // Passes but prints a warning
        // assert(x == uint(-42));
    
}

@safe unittest
{
    import std.checkedint;

        auto x = checked!Warn(-42);
        // Passes
        assert(x <= -42);
        // Passes but prints a warning
        // assert(x <= uint(-42));
    
}

@safe unittest
{
    import std.checkedint;

    auto x = checked!Warn(42);
    short x1 = cast(short) x;
    //x += long(int.max);
    auto y = checked!Warn(cast(const int) 42);
    short y1 = cast(const byte) y;
}

@safe unittest
{
    import std.checkedint;

    alias opEqualsProper = ProperCompare.hookOpEquals;
    assert(opEqualsProper(42, 42));
    assert(opEqualsProper(42.0, 42.0));
    assert(opEqualsProper(42u, 42));
    assert(opEqualsProper(42, 42u));
    assert(-1 == 4294967295u);
    assert(!opEqualsProper(-1, 4294967295u));
    assert(!opEqualsProper(const uint(-1), -1));
    assert(!opEqualsProper(uint(-1), -1.0));
    assert(3_000_000_000U == -1_294_967_296);
    assert(!opEqualsProper(3_000_000_000U, -1_294_967_296));
}

@safe unittest
{
    import std.checkedint;

        auto x = checked!WithNaN(422);
        assert((cast(ubyte) x) == 255);
        x = checked!WithNaN(-422);
        assert((cast(byte) x) == -128);
        assert(cast(short) x == -422);
        assert(cast(bool) x);
        x = x.init; // set back to NaN
        assert(x != true);
        assert(x != false);
    
}

@safe unittest
{
    import std.checkedint;

        Checked!(int, WithNaN) x;
        assert(!(x < 0) && !(x > 0) && !(x == 0));
        x = 1;
        assert(x > 0 && !(x < 0) && !(x == 0));
    
}

@safe unittest
{
    import std.checkedint;

        Checked!(int, WithNaN) x;
        ++x;
        assert(x.isNaN);
        x = 1;
        assert(!x.isNaN);
        x = -x;
        ++x;
        assert(!x.isNaN);
    
}

@safe unittest
{
    import std.checkedint;

        Checked!(int, WithNaN) x;
        assert((x + 1).isNaN);
        x = 100;
        assert(!(x + 1).isNaN);
    
}

@safe unittest
{
    import std.checkedint;

        Checked!(int, WithNaN) x;
        assert((1 + x).isNaN);
        x = 100;
        assert(!(1 + x).isNaN);
    
}

@safe unittest
{
    import std.checkedint;

        Checked!(int, WithNaN) x;
        x += 4;
        assert(x.isNaN);
        x = 0;
        x += 4;
        assert(!x.isNaN);
        x += int.max;
        assert(x.isNaN);
    
}

@safe unittest
{
    import std.checkedint;

    auto x1 = Checked!(int, WithNaN)();
    assert(x1.isNaN);
    assert(x1.get == int.min);
    assert(x1 != x1);
    assert(!(x1 < x1));
    assert(!(x1 > x1));
    assert(!(x1 == x1));
    ++x1;
    assert(x1.isNaN);
    assert(x1.get == int.min);
    --x1;
    assert(x1.isNaN);
    assert(x1.get == int.min);
    x1 = 42;
    assert(!x1.isNaN);
    assert(x1 == x1);
    assert(x1 <= x1);
    assert(x1 >= x1);
    static assert(x1.min == int.min + 1);
    x1 += long(int.max);
}

@safe unittest
{
    import std.checkedint;

    auto x1 = Checked!(int, WithNaN)();
    assert(x1.isNaN);
    x1 = 1;
    assert(!x1.isNaN);
    x1 = x1.init;
    assert(x1.isNaN);
}

@safe unittest
{
    import std.checkedint;

        auto x = checked!Saturate(short(100));
        x += 33000;
        assert(x == short.max);
        x -= 70000;
        assert(x == short.min);
    
}

@safe unittest
{
    import std.checkedint;

        assert(checked!Saturate(int.max) + 1 == int.max);
        assert(checked!Saturate(100) ^^ 10 == int.max);
        assert(checked!Saturate(-100) ^^ 10 == int.max);
        assert(checked!Saturate(100) / 0 == int.max);
        assert(checked!Saturate(100) << -1 == 0);
        assert(checked!Saturate(100) << 33 == int.max);
        assert(checked!Saturate(100) >> -1 == int.max);
        assert(checked!Saturate(100) >> 33 == 0);
    
}

@safe unittest
{
    import std.checkedint;

    auto x = checked!Saturate(int.max);
    ++x;
    assert(x == int.max);
    --x;
    assert(x == int.max - 1);
    x = int.min;
    assert(-x == int.max);
    x -= 42;
    assert(x == int.min);
    assert(x * -2 == int.max);
}

@safe unittest
{
    import std.checkedint;

    bool overflow;
    assert(opChecked!"+"(const short(1), short(1), overflow) == 2 && !overflow);
    assert(opChecked!"+"(1, 1, overflow) == 2 && !overflow);
    assert(opChecked!"+"(1, 1u, overflow) == 2 && !overflow);
    assert(opChecked!"+"(-1, 1u, overflow) == 0 && !overflow);
    assert(opChecked!"+"(1u, -1, overflow) == 0 && !overflow);
}

@safe unittest
{
    import std.checkedint;

    bool overflow;
    assert(opChecked!"-"(1, 1, overflow) == 0 && !overflow);
    assert(opChecked!"-"(1, 1u, overflow) == 0 && !overflow);
    assert(opChecked!"-"(1u, -1, overflow) == 2 && !overflow);
    assert(opChecked!"-"(-1, 1u, overflow) == 0 && overflow);
}

@safe unittest
{
    import std.checkedint;

    struct MyHook
    {
        static size_t hookToHash(T)(const T payload) nothrow @trusted
        {
            return .hashOf(payload);
        }
    }

    int[Checked!(int, MyHook)] aa;
    Checked!(int, MyHook) var = 42;
    aa[var] = 100;

    assert(aa[var] == 100);

    int[Checked!(int, Abort)] bb;
    Checked!(int, Abort) var2 = 42;
    bb[var2] = 100;

    assert(bb[var2] == 100);
}


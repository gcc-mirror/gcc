@safe unittest
{
    import std.functional;

    // Strings are compiled into functions:
    alias isEven = unaryFun!("(a & 1) == 0");
    assert(isEven(2) && !isEven(1));
}

@safe unittest
{
    import std.functional;

    alias less = binaryFun!("a < b");
    assert(less(1, 2) && !less(2, 1));
    alias greater = binaryFun!("a > b");
    assert(!greater("1", "2") && greater("2", "1"));
}

pure @safe @nogc nothrow unittest
{
    import std.functional;

    assert(lessThan(2, 3));
    assert(lessThan(2U, 3U));
    assert(lessThan(2, 3.0));
    assert(lessThan(-2, 3U));
    assert(lessThan(2, 3U));
    assert(!lessThan(3U, -2));
    assert(!lessThan(3U, 2));
    assert(!lessThan(0, 0));
    assert(!lessThan(0U, 0));
    assert(!lessThan(0, 0U));
}

@safe unittest
{
    import std.functional;

    assert(!greaterThan(2, 3));
    assert(!greaterThan(2U, 3U));
    assert(!greaterThan(2, 3.0));
    assert(!greaterThan(-2, 3U));
    assert(!greaterThan(2, 3U));
    assert(greaterThan(3U, -2));
    assert(greaterThan(3U, 2));
    assert(!greaterThan(0, 0));
    assert(!greaterThan(0U, 0));
    assert(!greaterThan(0, 0U));
}

@safe unittest
{
    import std.functional;

    assert(equalTo(0U, 0));
    assert(equalTo(0, 0U));
    assert(!equalTo(-1, ~0U));
}

@safe unittest
{
    import std.functional;

    alias gt = reverseArgs!(binaryFun!("a < b"));
    assert(gt(2, 1) && !gt(1, 1));
}

@safe unittest
{
    import std.functional;

    int x = 42;
    bool xyz(int a, int b) { return a * x < b / x; }
    auto foo = &xyz;
    foo(4, 5);
    alias zyx = reverseArgs!(foo);
    assert(zyx(5, 4) == foo(4, 5));
}

@safe unittest
{
    import std.functional;

    alias gt = reverseArgs!(binaryFun!("a < b"));
    assert(gt(2, 1) && !gt(1, 1));
    int x = 42;
    bool xyz(int a, int b) { return a * x < b / x; }
    auto foo = &xyz;
    foo(4, 5);
    alias zyx = reverseArgs!(foo);
    assert(zyx(5, 4) == foo(4, 5));
}

@safe unittest
{
    import std.functional;

    int abc(int a, int b, int c) { return a * b + c; }
    alias cba = reverseArgs!abc;
    assert(abc(91, 17, 32) == cba(32, 17, 91));
}

@safe unittest
{
    import std.functional;

    int a(int a) { return a * 2; }
    alias _a = reverseArgs!a;
    assert(a(2) == _a(2));
}

@safe unittest
{
    import std.functional;

    int b() { return 4; }
    alias _b = reverseArgs!b;
    assert(b() == _b());
}

@safe unittest
{
    import std.functional;

    import std.algorithm.searching : find;
    import std.uni : isWhite;
    string a = "   Hello, world!";
    assert(find!(not!isWhite)(a) == "Hello, world!");
}

@safe unittest
{
    import std.functional;

    int fun(int a, int b) { return a + b; }
    alias fun5 = partial!(fun, 5);
    assert(fun5(6) == 11);
    // Note that in most cases you'd use an alias instead of a value
    // assignment. Using an alias allows you to partially evaluate template
    // functions without committing to a particular type of the function.
}

@safe unittest
{
    import std.functional;

    // Overloads are resolved when the partially applied function is called
    // with the remaining arguments.
    struct S
    {
        static char fun(int i, string s) { return s[i]; }
        static int fun(int a, int b) { return a * b; }
    }
    alias fun3 = partial!(S.fun, 3);
    assert(fun3("hello") == 'l');
    assert(fun3(10) == 30);
}

pure @safe @nogc nothrow unittest
{
    import std.functional;

    int f(int x, int y, int z)
    {
        return x + y + z;
    }
    auto cf = curry!f;
    auto cf1 = cf(1);
    auto cf2 = cf(2);

    assert(cf1(2)(3) == f(1, 2, 3));
    assert(cf2(2)(3) == f(2, 2, 3));
}

pure @safe @nogc nothrow unittest
{
    import std.functional;

    //works with callable structs too
    struct S
    {
        int w;
        int opCall(int x, int y, int z)
        {
            return w + x + y + z;
        }
    }

    S s;
    s.w = 5;

    auto cs = curry(s);
    auto cs1 = cs(1);
    auto cs2 = cs(2);

    assert(cs1(2)(3) == s(1, 2, 3));
    assert(cs1(2)(3) == (1 + 2 + 3 + 5));
    assert(cs2(2)(3) ==s(2, 2, 3));
}

@safe unittest
{
    import std.functional;

    import std.typecons : Tuple;
    static bool f1(int a) { return a != 0; }
    static int f2(int a) { return a / 2; }
    auto x = adjoin!(f1, f2)(5);
    assert(is(typeof(x) == Tuple!(bool, int)));
    assert(x[0] == true && x[1] == 2);
}

@safe unittest
{
    import std.functional;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.array : split;
    import std.conv : to;

    // First split a string in whitespace-separated tokens and then
    // convert each token into an integer
    assert(compose!(map!(to!(int)), split)("1 2 3").equal([1, 2, 3]));
}

@safe unittest
{
    import std.functional;

    import std.conv : to;
    string foo(int a) { return to!(string)(a); }
    int bar(string a) { return to!(int)(a) + 1; }
    double baz(int a) { return a + 0.5; }
    assert(compose!(baz, bar, foo)(1) == 2.5);
    assert(pipe!(foo, bar, baz)(1) == 2.5);

    assert(compose!(baz, `to!(int)(a) + 1`, foo)(1) == 2.5);
    assert(compose!(baz, bar)("1"[]) == 2.5);

    assert(compose!(baz, bar)("1") == 2.5);

    assert(compose!(`a + 0.5`, `to!(int)(a) + 1`, foo)(1) == 2.5);
}

@safe nothrow unittest
{
    import std.functional;

    ulong fib(ulong n) @safe nothrow
    {
        return n < 2 ? n : memoize!fib(n - 2) + memoize!fib(n - 1);
    }
    assert(fib(10) == 55);
}

@safe unittest
{
    import std.functional;

    ulong fact(ulong n) @safe
    {
        return n < 2 ? 1 : n * memoize!fact(n - 1);
    }
    assert(fact(10) == 3628800);
}

@safe unittest
{
    import std.functional;

    ulong factImpl(ulong n) @safe
    {
        return n < 2 ? 1 : n * factImpl(n - 1);
    }
    alias fact = memoize!factImpl;
    assert(fact(10) == 3628800);
}

@system unittest
{
    import std.functional;

    ulong fact(ulong n)
    {
        // Memoize no more than 8 values
        return n < 2 ? 1 : n * memoize!(fact, 8)(n - 1);
    }
    assert(fact(8) == 40320);
    // using more entries than maxSize will overwrite existing entries
    assert(fact(10) == 3628800);
}

@safe unittest
{
    import std.functional;

    static int inc(ref uint num) {
        num++;
        return 8675309;
    }

    uint myNum = 0;
    auto incMyNumDel = toDelegate(&inc);
    auto returnVal = incMyNumDel(myNum);
    assert(myNum == 1);
}

@safe unittest
{
    import std.functional;

    import std.typecons : tuple;

    auto name = tuple("John", "Doe");
    string full = name.bind!((first, last) => first ~ " " ~ last);
    assert(full == "John Doe");
}

@safe unittest
{
    import std.functional;

    import std.algorithm.comparison : min, max;

    struct Pair
    {
        int a;
        int b;
    }

    auto p = Pair(123, 456);
    assert(p.bind!min == 123); // min(p.a, p.b)
    assert(p.bind!max == 456); // max(p.a, p.b)
}

@safe unittest
{
    import std.functional;

    import std.algorithm.iteration : map, filter;
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    auto ages = [
        tuple("Alice", 35),
        tuple("Bob",   64),
        tuple("Carol", 21),
        tuple("David", 39),
        tuple("Eve",   50)
    ];

    auto overForty = ages
        .filter!(bind!((name, age) => age > 40))
        .map!(bind!((name, age) => name));

    assert(overForty.equal(["Bob", "Eve"]));
}


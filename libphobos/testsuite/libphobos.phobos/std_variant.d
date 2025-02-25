@system unittest
{
    import std.variant;

    Variant a; // Must assign before use, otherwise exception ensues
    // Initialize with an integer; make the type int
    Variant b = 42;
    assert(b.type == typeid(int));
    // Peek at the value
    assert(b.peek!(int) !is null && *b.peek!(int) == 42);
    // Automatically convert per language rules
    auto x = b.get!(real);

    // Assign any other type, including other variants
    a = b;
    a = 3.14;
    assert(a.type == typeid(double));
    // Implicit conversions work just as with built-in types
    assert(a < b);
    // Check for convertibility
    assert(!a.convertsTo!(int)); // double not convertible to int
    // Strings and all other arrays are supported
    a = "now I'm a string";
    assert(a == "now I'm a string");

    // can also assign arrays
    a = new int[42];
    assert(a.length == 42);
    a[5] = 7;
    assert(a[5] == 7);

    // Can also assign class values
    class Foo {}
    auto foo = new Foo;
    a = foo;
    assert(*a.peek!(Foo) == foo); // and full type information is preserved
}

@safe unittest
{
    import std.variant;

    struct Cat { int a, b, c; }

    align(1) struct S
    {
        long l;
        ubyte b;
    }

    align(1) struct T
    {
        ubyte b;
        long l;
    }

    static assert(maxSize!(int, long) == 8);
    static assert(maxSize!(bool, byte) == 1);
    static assert(maxSize!(bool, Cat) == 12);
    static assert(maxSize!(char) == 1);
    static assert(maxSize!(char, short, ubyte) == 2);
    static assert(maxSize!(char, long, ubyte) == 8);
    import std.algorithm.comparison : max;
    static assert(maxSize!(long, S) == max(long.sizeof, S.sizeof));
    static assert(maxSize!(S, T) == max(S.sizeof, T.sizeof));
    static assert(maxSize!(int, ubyte[7]) == 7);
    static assert(maxSize!(int, ubyte[3]) == 4);
    static assert(maxSize!(int, int, ubyte[3]) == 4);
    static assert(maxSize!(void, int, ubyte[3]) == 4);
    static assert(maxSize!(void) == 1);
}

@system unittest
{
    import std.variant;

    alias Var = VariantN!(maxSize!(int, double, string));

    Var a; // Must assign before use, otherwise exception ensues
    // Initialize with an integer; make the type int
    Var b = 42;
    assert(b.type == typeid(int));
    // Peek at the value
    assert(b.peek!(int) !is null && *b.peek!(int) == 42);
    // Automatically convert per language rules
    auto x = b.get!(real);

    // Assign any other type, including other variants
    a = b;
    a = 3.14;
    assert(a.type == typeid(double));
    // Implicit conversions work just as with built-in types
    assert(a < b);
    // Check for convertibility
    assert(!a.convertsTo!(int)); // double not convertible to int
    // Strings and all other arrays are supported
    a = "now I'm a string";
    assert(a == "now I'm a string");
}

@system unittest
{
    import std.variant;

    alias Var = VariantN!(maxSize!(int[]));

    Var a = new int[42];
    assert(a.length == 42);
    a[5] = 7;
    assert(a[5] == 7);
}

@system unittest
{
    import std.variant;

    alias Var = VariantN!(maxSize!(int*)); // classes are pointers
    Var a;

    class Foo {}
    auto foo = new Foo;
    a = foo;
    assert(*a.peek!(Foo) == foo); // and full type information is preserved
}

@system unittest
{
    import std.variant;

    auto v = Algebraic!(int, double, string)(5);
    assert(v.peek!(int));
    v = 3.14;
    assert(v.peek!(double));
    // auto x = v.peek!(long); // won't compile, type long not allowed
    // v = '1'; // won't compile, type char not allowed
}

@system unittest
{
    import std.variant;

    import std.typecons : Tuple, tuple;

    // A tree is either a leaf or a branch of two other trees
    alias Tree(Leaf) = Algebraic!(Leaf, Tuple!(This*, This*));
    Tree!int tree = tuple(new Tree!int(42), new Tree!int(43));
    Tree!int* right = tree.get!1[1];
    assert(*right == 43);

    // An object is a double, a string, or a hash of objects
    alias Obj = Algebraic!(double, string, This[string]);
    Obj obj = "hello";
    assert(obj.get!1 == "hello");
    obj = 42.0;
    assert(obj.get!0 == 42);
    obj = ["customer": Obj("John"), "paid": Obj(23.95)];
    assert(obj.get!2["customer"] == "John");
}

@system unittest
{
    import std.variant;

    Variant a; // Must assign before use, otherwise exception ensues
    // Initialize with an integer; make the type int
    Variant b = 42;
    assert(b.type == typeid(int));
    // Peek at the value
    assert(b.peek!(int) !is null && *b.peek!(int) == 42);
    // Automatically convert per language rules
    auto x = b.get!(real);

    // Assign any other type, including other variants
    a = b;
    a = 3.14;
    assert(a.type == typeid(double));
    // Implicit conversions work just as with built-in types
    assert(a < b);
    // Check for convertibility
    assert(!a.convertsTo!(int)); // double not convertible to int
    // Strings and all other arrays are supported
    a = "now I'm a string";
    assert(a == "now I'm a string");
}

@system unittest
{
    import std.variant;

    Variant a = new int[42];
    assert(a.length == 42);
    a[5] = 7;
    assert(a[5] == 7);
}

@system unittest
{
    import std.variant;

    Variant a;

    class Foo {}
    auto foo = new Foo;
    a = foo;
    assert(*a.peek!(Foo) == foo); // and full type information is preserved
}

@system unittest
{
    import std.variant;

    auto a = variantArray(1, 3.14, "Hi!");
    assert(a[1] == 3.14);
    auto b = Variant(a); // variant array as variant
    assert(b[1] == 3.14);
}

@system unittest
{
    import std.variant;

    import std.exception : assertThrown;

    Variant v;

    // uninitialized use
    assertThrown!VariantException(v + 1);
    assertThrown!VariantException(v.length);

    // .get with an incompatible target type
    assertThrown!VariantException(Variant("a").get!int);

    // comparison between incompatible types
    assertThrown!VariantException(Variant(3) < Variant("a"));
}

@system unittest
{
    import std.variant;

    Algebraic!(int, string) variant;

    variant = 10;
    assert(variant.visit!((string s) => cast(int) s.length,
                          (int i)    => i)()
                          == 10);
    variant = "string";
    assert(variant.visit!((int i) => i,
                          (string s) => cast(int) s.length)()
                          == 6);

    // Error function usage
    Algebraic!(int, string) emptyVar;
    auto rslt = emptyVar.visit!((string s) => cast(int) s.length,
                          (int i)    => i,
                          () => -1)();
    assert(rslt == -1);

    // Generic function usage
    Algebraic!(int, float, real) number = 2;
    assert(number.visit!(x => x += 1) == 3);

    // Generic function for int/float with separate behavior for string
    Algebraic!(int, float, string) something = 2;
    assert(something.visit!((string s) => s.length, x => x) == 2); // generic
    something = "asdf";
    assert(something.visit!((string s) => s.length, x => x) == 4); // string

    // Generic handler and empty handler
    Algebraic!(int, float, real) empty2;
    assert(empty2.visit!(x => x + 1, () => -1) == -1);
}

@system unittest
{
    import std.variant;

    Algebraic!(int, string) variant;

    variant = 10;
    auto which = -1;
    variant.tryVisit!((int i) { which = 0; })();
    assert(which == 0);

    // Error function usage
    variant = "test";
    variant.tryVisit!((int i) { which = 0; },
                      ()      { which = -100; })();
    assert(which == -100);
}


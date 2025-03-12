@safe unittest
{
    import std.sumtype;

    import std.math.operations : isClose;

    struct Fahrenheit { double degrees; }
    struct Celsius { double degrees; }
    struct Kelvin { double degrees; }

    alias Temperature = SumType!(Fahrenheit, Celsius, Kelvin);

    // Construct from any of the member types.
    Temperature t1 = Fahrenheit(98.6);
    Temperature t2 = Celsius(100);
    Temperature t3 = Kelvin(273);

    // Use pattern matching to access the value.
    Fahrenheit toFahrenheit(Temperature t)
    {
        return Fahrenheit(
            t.match!(
                (Fahrenheit f) => f.degrees,
                (Celsius c) => c.degrees * 9.0/5 + 32,
                (Kelvin k) => k.degrees * 9.0/5 - 459.4
            )
        );
    }

    assert(toFahrenheit(t1).degrees.isClose(98.6));
    assert(toFahrenheit(t2).degrees.isClose(212));
    assert(toFahrenheit(t3).degrees.isClose(32));

    // Use ref to modify the value in place.
    void freeze(ref Temperature t)
    {
        t.match!(
            (ref Fahrenheit f) => f.degrees = 32,
            (ref Celsius c) => c.degrees = 0,
            (ref Kelvin k) => k.degrees = 273
        );
    }

    freeze(t1);
    assert(toFahrenheit(t1).degrees.isClose(32));

    // Use a catch-all handler to give a default result.
    bool isFahrenheit(Temperature t)
    {
        return t.match!(
            (Fahrenheit f) => true,
            _ => false
        );
    }

    assert(isFahrenheit(t1));
    assert(!isFahrenheit(t2));
    assert(!isFahrenheit(t3));
}

@safe unittest
{
    import std.sumtype;

    alias ExampleSumType = SumType!(int, string, double);

    ExampleSumType a = 123;
    ExampleSumType b = "hello";
    ExampleSumType c = 3.14;

    assert(a.handle == "got an int");
    assert(b.handle == "got a string");
    assert(c.handle == "got a double");
}

@system unittest
{
    import std.sumtype;

    import std.functional : partial;
    import std.traits : EnumMembers;
    import std.typecons : Tuple;

    enum Op : string
    {
        Plus  = "+",
        Minus = "-",
        Times = "*",
        Div   = "/"
    }

    // An expression is either
    //  - a number,
    //  - a variable, or
    //  - a binary operation combining two sub-expressions.
    alias Expr = SumType!(
        double,
        string,
        Tuple!(Op, "op", This*, "lhs", This*, "rhs")
    );

    // Shorthand for Tuple!(Op, "op", Expr*, "lhs", Expr*, "rhs"),
    // the Tuple type above with Expr substituted for This.
    alias BinOp = Expr.Types[2];

    // Factory function for number expressions
    Expr* num(double value)
    {
        return new Expr(value);
    }

    // Factory function for variable expressions
    Expr* var(string name)
    {
        return new Expr(name);
    }

    // Factory function for binary operation expressions
    Expr* binOp(Op op, Expr* lhs, Expr* rhs)
    {
        return new Expr(BinOp(op, lhs, rhs));
    }

    // Convenience wrappers for creating BinOp expressions
    alias sum  = partial!(binOp, Op.Plus);
    alias diff = partial!(binOp, Op.Minus);
    alias prod = partial!(binOp, Op.Times);
    alias quot = partial!(binOp, Op.Div);

    // Evaluate expr, looking up variables in env
    double eval(Expr expr, double[string] env)
    {
        return expr.match!(
            (double num) => num,
            (string var) => env[var],
            (BinOp bop)
            {
                double lhs = eval(*bop.lhs, env);
                double rhs = eval(*bop.rhs, env);
                final switch (bop.op)
                {
                    static foreach (op; EnumMembers!Op)
                    {
                        case op:
                            return mixin("lhs" ~ op ~ "rhs");
                    }
                }
            }
        );
    }

    // Return a "pretty-printed" representation of expr
    string pprint(Expr expr)
    {
        import std.format : format;

        return expr.match!(
            (double num) => "%g".format(num),
            (string var) => var,
            (BinOp bop) => "(%s %s %s)".format(
                pprint(*bop.lhs),
                cast(string) bop.op,
                pprint(*bop.rhs)
            )
        );
    }

    Expr* myExpr = sum(var("a"), prod(num(2), var("b")));
    double[string] myEnv = ["a":3, "b":4, "c":7];

    assert(eval(*myExpr, myEnv) == 11);
    assert(pprint(*myExpr) == "(a + (2 * b))");
}

@safe unittest
{
    import std.sumtype;

    static struct ConvertsToSumType
    {
        SumType!int payload;
        alias payload this;
    }

    static struct ContainsSumType
    {
        SumType!int payload;
    }

    assert(isSumType!(SumType!int));
    assert(isSumType!ConvertsToSumType);
    assert(!isSumType!ContainsSumType);
}

@safe unittest
{
    import std.sumtype;

    alias Number = SumType!(double, int);

    Number x;

    // Problem: because int implicitly converts to double, the double
    // handler is used for both types, and the int handler never matches.
    assert(!__traits(compiles,
        x.match!(
            (double d) => "got double",
            (int n) => "got int"
        )
    ));

    // Solution 1: put the handler for the "more specialized" type (in this
    // case, int) before the handler for the type it converts to.
    assert(__traits(compiles,
        x.match!(
            (int n) => "got int",
            (double d) => "got double"
        )
    ));

    // Solution 2: use a template that only accepts the exact type it's
    // supposed to match, instead of any type that implicitly converts to it.
    alias exactly(T, alias fun) = function (arg)
    {
        static assert(is(typeof(arg) == T));
        return fun(arg);
    };

    // Now, even if we put the double handler first, it will only be used for
    // doubles, not ints.
    assert(__traits(compiles,
        x.match!(
            exactly!(double, d => "got double"),
            exactly!(int, n => "got int")
        )
    ));
}

@safe unittest
{
    import std.sumtype;

    struct Point2D { double x, y; }
    struct Point3D { double x, y, z; }

    alias Point = SumType!(Point2D, Point3D);

    version (none)
    {
        // This function works, but the code is ugly and repetitive.
        // It uses three separate calls to match!
        @safe pure nothrow @nogc
        bool sameDimensions(Point p1, Point p2)
        {
            return p1.match!(
                (Point2D _) => p2.match!(
                    (Point2D _) => true,
                    _ => false
                ),
                (Point3D _) => p2.match!(
                    (Point3D _) => true,
                    _ => false
                )
            );
        }
    }

    // This version is much nicer.
    @safe pure nothrow @nogc
    bool sameDimensions(Point p1, Point p2)
    {
        alias doMatch = match!(
            (Point2D _1, Point2D _2) => true,
            (Point3D _1, Point3D _2) => true,
            (_1, _2) => false
        );

        return doMatch(p1, p2);
    }

    Point a = Point2D(1, 2);
    Point b = Point2D(3, 4);
    Point c = Point3D(5, 6, 7);
    Point d = Point3D(8, 9, 0);

    assert( sameDimensions(a, b));
    assert( sameDimensions(c, d));
    assert(!sameDimensions(a, c));
    assert(!sameDimensions(d, b));
}

@safe unittest
{
    import std.sumtype;

    alias handleInt = (int i) => "got an int";

    assert( canMatch!(handleInt, int));
    assert(!canMatch!(handleInt, string));
}

@safe unittest
{
    import std.sumtype;

    SumType!(string, double) example = "hello";

    assert( example.has!string);
    assert(!example.has!double);

    // If T isn't part of the SumType, has!T will always return false.
    assert(!example.has!int);
}

@safe unittest
{
    import std.sumtype;

    alias Example = SumType!(string, double);

    Example m = "mutable";
    const Example c = "const";
    immutable Example i = "immutable";

    assert( m.has!string);
    assert(!m.has!(const(string)));
    assert(!m.has!(immutable(string)));

    assert(!c.has!string);
    assert( c.has!(const(string)));
    assert(!c.has!(immutable(string)));

    assert(!i.has!string);
    assert(!i.has!(const(string)));
    assert( i.has!(immutable(string)));
}

@safe unittest
{
    import std.sumtype;

    import std.algorithm.iteration : filter;
    import std.algorithm.comparison : equal;

    alias Example = SumType!(string, double);

    auto arr = [
        Example("foo"),
        Example(0),
        Example("bar"),
        Example(1),
        Example(2),
        Example("baz")
    ];

    auto strings = arr.filter!(has!string);
    auto nums = arr.filter!(has!double);

    assert(strings.equal([Example("foo"), Example("bar"), Example("baz")]));
    assert(nums.equal([Example(0), Example(1), Example(2)]));
}

@safe unittest
{
    import std.sumtype;

    SumType!(string, double) example1 = "hello";
    SumType!(string, double) example2 = 3.14;

    assert(example1.get!string == "hello");
    assert(example2.get!double == 3.14);
}

@safe unittest
{
    import std.sumtype;

    alias Example = SumType!(string, double);

    Example m = "mutable";
    const(Example) c = "const";
    immutable(Example) i = "immutable";

    assert(m.get!string == "mutable");
    assert(c.get!(const(string)) == "const");
    assert(i.get!(immutable(string)) == "immutable");
}

@safe unittest
{
    import std.sumtype;

    import std.algorithm.iteration : map;
    import std.algorithm.comparison : equal;

    alias Example = SumType!(string, double);

    auto arr = [Example(0), Example(1), Example(2)];
    auto values = arr.map!(get!double);

    assert(values.equal([0, 1, 2]));
}

@safe unittest
{
    import std.sumtype;

    SumType!(string, double) example = "hello";

    assert(example.tryGet!string == "hello");

    double result = double.nan;
    try
        result = example.tryGet!double;
    catch (MatchException e)
        result = 0;

    // Exception was thrown
    assert(result == 0);
}

@safe unittest
{
    import std.sumtype;

    import std.exception : assertThrown;

    const(SumType!(string, double)) example = "const";

    // Qualifier mismatch; throws exception
    assertThrown!MatchException(example.tryGet!string);
    // Qualifier matches; no exception
    assert(example.tryGet!(const(string)) == "const");
}

@safe unittest
{
    import std.sumtype;

    import std.algorithm.iteration : map, sum;
    import std.functional : pipe;
    import std.exception : assertThrown;

    alias Example = SumType!(string, double);

    auto arr1 = [Example(0), Example(1), Example(2)];
    auto arr2 = [Example("foo"), Example("bar"), Example("baz")];

    alias trySum = pipe!(map!(tryGet!double), sum);

    assert(trySum(arr1) == 0 + 1 + 2);
    assertThrown!MatchException(trySum(arr2));
}


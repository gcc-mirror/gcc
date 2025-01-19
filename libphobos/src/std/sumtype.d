/++
[SumType] is a generic discriminated union implementation that uses
design-by-introspection to generate safe and efficient code. Its features
include:

* [Pattern matching.][match]
* Support for self-referential types.
* Full attribute correctness (`pure`, `@safe`, `@nogc`, and `nothrow` are
    inferred whenever possible).
* A type-safe and memory-safe API compatible with DIP 1000 (`scope`).
* No dependency on runtime type information (`TypeInfo`).
* Compatibility with BetterC.

$(H3 List of examples)

* [Basic usage](#basic-usage)
* [Matching with an overload set](#matching-with-an-overload-set)
* [Recursive SumTypes](#recursive-sumtypes)
* [Memory corruption](#memory-corruption) (why assignment can be `@system`)
* [Avoiding unintentional matches](#avoiding-unintentional-matches)
* [Multiple dispatch](#multiple-dispatch)

License: Boost License 1.0
Authors: Paul Backus
Source: $(PHOBOSSRC std/sumtype.d)
+/
module std.sumtype;

/// $(DIVID basic-usage,$(H3 Basic usage))
version (D_BetterC) {} else
@safe unittest
{
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

/** $(DIVID matching-with-an-overload-set, $(H3 Matching with an overload set))
 *
 * Instead of writing `match` handlers inline as lambdas, you can write them as
 * overloads of a function. An `alias` can be used to create an additional
 * overload for the `SumType` itself.
 *
 * For example, with this overload set:
 *
 * ---
 * string handle(int n) { return "got an int"; }
 * string handle(string s) { return "got a string"; }
 * string handle(double d) { return "got a double"; }
 * alias handle = match!handle;
 * ---
 *
 * Usage would look like this:
 */
version (D_BetterC) {} else
@safe unittest
{
    alias ExampleSumType = SumType!(int, string, double);

    ExampleSumType a = 123;
    ExampleSumType b = "hello";
    ExampleSumType c = 3.14;

    assert(a.handle == "got an int");
    assert(b.handle == "got a string");
    assert(c.handle == "got a double");
}

/** $(DIVID recursive-sumtypes, $(H3 Recursive SumTypes))
 *
 * This example makes use of the special placeholder type `This` to define a
 * [recursive data type](https://en.wikipedia.org/wiki/Recursive_data_type): an
 * [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) for
 * representing simple arithmetic expressions.
 */
version (D_BetterC) {} else
@system unittest
{
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

// For the "Matching with an overload set" example above
// Needs public import to work with `make publictests`
version (unittest) public import std.internal.test.sumtype_example_overloads;

import std.format.spec : FormatSpec, singleSpec;
import std.meta : AliasSeq, Filter, IndexOf = staticIndexOf, Map = staticMap;
import std.meta : NoDuplicates;
import std.meta : anySatisfy, allSatisfy;
import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor;
import std.traits : isAssignable, isCopyable, isStaticArray, isRvalueAssignable;
import std.traits : ConstOf, ImmutableOf, InoutOf, TemplateArgsOf;
import std.traits : CommonType, DeducedParameterType;
import std.typecons : ReplaceTypeUnless;
import std.typecons : Flag;
import std.conv : toCtString;

/// Placeholder used to refer to the enclosing [SumType].
struct This {}

// True if a variable of type T can appear on the lhs of an assignment
private enum isAssignableTo(T) =
    isAssignable!T || (!isCopyable!T && isRvalueAssignable!T);

// toHash is required by the language spec to be nothrow and @safe
private enum isHashable(T) = __traits(compiles,
    () nothrow @safe { hashOf(T.init); }
);

private enum hasPostblit(T) = __traits(hasPostblit, T);

private enum isInout(T) = is(T == inout);

/**
 * A [tagged union](https://en.wikipedia.org/wiki/Tagged_union) that can hold a
 * single value from any of a specified set of types.
 *
 * The value in a `SumType` can be operated on using [pattern matching][match].
 *
 * To avoid ambiguity, duplicate types are not allowed (but see the
 * ["basic usage" example](#basic-usage) for a workaround).
 *
 * The special type `This` can be used as a placeholder to create
 * self-referential types, just like with `Algebraic`. See the
 * ["Recursive SumTypes" example](#recursive-sumtypes) for usage.
 *
 * A `SumType` is initialized by default to hold the `.init` value of its
 * first member type, just like a regular union. The version identifier
 * `SumTypeNoDefaultCtor` can be used to disable this behavior.
 *
 * See_Also: $(REF Algebraic, std,variant)
 */
struct SumType(Types...)
if (is(NoDuplicates!Types == Types) && Types.length > 0)
{
    /// The types a `SumType` can hold.
    alias Types = AliasSeq!(
        ReplaceTypeUnless!(isSumTypeInstance, This, typeof(this), TemplateArgsOf!SumType)
    );

private:

    enum bool canHoldTag(T) = Types.length <= T.max;
    alias unsignedInts = AliasSeq!(ubyte, ushort, uint, ulong);

    alias Tag = Filter!(canHoldTag, unsignedInts)[0];

    union Storage
    {
        // Workaround for https://issues.dlang.org/show_bug.cgi?id=20068
        template memberName(T)
        if (IndexOf!(T, Types) >= 0)
        {
            enum tid = IndexOf!(T, Types);
            mixin("enum memberName = `values_", toCtString!tid, "`;");
        }

        static foreach (T; Types)
        {
            mixin("T ", memberName!T, ";");
        }
    }

    Storage storage;
    Tag tag;

    /* Accesses the value stored in a SumType.
     *
     * This method is memory-safe, provided that:
     *
     *   1. A SumType's tag is always accurate.
     *   2. A SumType cannot be assigned to in @safe code if that assignment
     *      could cause unsafe aliasing.
     *
     * All code that accesses a SumType's tag or storage directly, including
     * @safe code in this module, must be manually checked to ensure that it
     * does not violate either of the above requirements.
     */
    @trusted
    ref inout(T) get(T)() inout
    if (IndexOf!(T, Types) >= 0)
    {
        enum tid = IndexOf!(T, Types);
        assert(tag == tid,
            "This `" ~ SumType.stringof ~
            "` does not contain a(n) `" ~ T.stringof ~ "`"
        );
        return __traits(getMember, storage, Storage.memberName!T);
    }

public:

    // Workaround for https://issues.dlang.org/show_bug.cgi?id=21399
    version (StdDdoc)
    {
        // Dummy type to stand in for loop variable
        private struct T;

        /// Constructs a `SumType` holding a specific value.
        this(T value);

        /// ditto
        this(const(T) value) const;

        /// ditto
        this(immutable(T) value) immutable;

        /// ditto
        this(Value)(Value value) inout
        if (is(Value == DeducedParameterType!(inout(T))));
    }

    static foreach (tid, T; Types)
    {
        /// Constructs a `SumType` holding a specific value.
        this(T value)
        {
            import core.lifetime : forward;

            static if (isCopyable!T)
            {
                // Workaround for https://issues.dlang.org/show_bug.cgi?id=21542
                __traits(getMember, storage, Storage.memberName!T) = __ctfe ? value : forward!value;
            }
            else
            {
                __traits(getMember, storage, Storage.memberName!T) = forward!value;
            }

            tag = tid;
        }

        static if (isCopyable!(const(T)))
        {
            static if (IndexOf!(const(T), Map!(ConstOf, Types)) == tid)
            {
                /// ditto
                this(const(T) value) const
                {
                    __traits(getMember, storage, Storage.memberName!T) = value;
                    tag = tid;
                }
            }
        }
        else
        {
            @disable this(const(T) value) const;
        }

        static if (isCopyable!(immutable(T)))
        {
            static if (IndexOf!(immutable(T), Map!(ImmutableOf, Types)) == tid)
            {
                /// ditto
                this(immutable(T) value) immutable
                {
                    __traits(getMember, storage, Storage.memberName!T) = value;
                    tag = tid;
                }
            }
        }
        else
        {
            @disable this(immutable(T) value) immutable;
        }

        static if (isCopyable!(inout(T)))
        {
            static if (IndexOf!(inout(T), Map!(InoutOf, Types)) == tid)
            {
                /// ditto
                this(Value)(Value value) inout
                if (is(Value == DeducedParameterType!(inout(T))))
                {
                    __traits(getMember, storage, Storage.memberName!T) = value;
                    tag = tid;
                }
            }
        }
        else
        {
            @disable this(Value)(Value value) inout
            if (is(Value == DeducedParameterType!(inout(T))));
        }
    }

    static if (anySatisfy!(hasElaborateCopyConstructor, Types))
    {
        static if
        (
            allSatisfy!(isCopyable, Map!(InoutOf, Types))
            && !anySatisfy!(hasPostblit, Map!(InoutOf, Types))
            && allSatisfy!(isInout, Map!(InoutOf, Types))
        )
        {
            /// Constructs a `SumType` that's a copy of another `SumType`.
            this(ref inout(SumType) other) inout
            {
                storage = other.match!((ref value) {
                    alias OtherTypes = Map!(InoutOf, Types);
                    enum tid = IndexOf!(typeof(value), OtherTypes);
                    alias T = Types[tid];

                    mixin("inout(Storage) newStorage = { ",
                        Storage.memberName!T, ": value",
                    " };");

                    return newStorage;
                });

                tag = other.tag;
            }
        }
        else
        {
            static if (allSatisfy!(isCopyable, Types))
            {
                /// ditto
                this(ref SumType other)
                {
                    storage = other.match!((ref value) {
                        alias T = typeof(value);

                        mixin("Storage newStorage = { ",
                            Storage.memberName!T, ": value",
                        " };");

                        return newStorage;
                    });

                    tag = other.tag;
                }
            }
            else
            {
                @disable this(ref SumType other);
            }

            static if (allSatisfy!(isCopyable, Map!(ConstOf, Types)))
            {
                /// ditto
                this(ref const(SumType) other) const
                {
                    storage = other.match!((ref value) {
                        alias OtherTypes = Map!(ConstOf, Types);
                        enum tid = IndexOf!(typeof(value), OtherTypes);
                        alias T = Types[tid];

                        mixin("const(Storage) newStorage = { ",
                            Storage.memberName!T, ": value",
                        " };");

                        return newStorage;
                    });

                    tag = other.tag;
                }
            }
            else
            {
                @disable this(ref const(SumType) other) const;
            }

            static if (allSatisfy!(isCopyable, Map!(ImmutableOf, Types)))
            {
                /// ditto
                this(ref immutable(SumType) other) immutable
                {
                    storage = other.match!((ref value) {
                        alias OtherTypes = Map!(ImmutableOf, Types);
                        enum tid = IndexOf!(typeof(value), OtherTypes);
                        alias T = Types[tid];

                        mixin("immutable(Storage) newStorage = { ",
                            Storage.memberName!T, ": value",
                        " };");

                        return newStorage;
                    });

                    tag = other.tag;
                }
            }
            else
            {
                @disable this(ref immutable(SumType) other) immutable;
            }
        }
    }

    version (SumTypeNoDefaultCtor)
    {
        @disable this();
    }

    // Workaround for https://issues.dlang.org/show_bug.cgi?id=21399
    version (StdDdoc)
    {
        // Dummy type to stand in for loop variable
        private struct T;

        /**
         * Assigns a value to a `SumType`.
         *
         * If any of the `SumType`'s members other than the one being assigned
         * to contain pointers or references, it is possible for the assignment
         * to cause memory corruption (see the
         * ["Memory corruption" example](#memory-corruption) below for an
         * illustration of how). Therefore, such assignments are considered
         * `@system`.
         *
         * An individual assignment can be `@trusted` if the caller can
         * guarantee that there are no outstanding references to any `SumType`
         * members that contain pointers or references at the time the
         * assignment occurs.
         *
         * Examples:
         *
         * $(DIVID memory-corruption, $(H3 Memory corruption))
         *
         * This example shows how assignment to a `SumType` can be used to
         * cause memory corruption in `@system` code. In `@safe` code, the
         * assignment `s = 123` would not be allowed.
         *
         * ---
         * SumType!(int*, int) s = new int;
         * s.tryMatch!(
         *     (ref int* p) {
         *         s = 123; // overwrites `p`
         *         return *p; // undefined behavior
         *     }
         * );
         * ---
         */
        ref SumType opAssign(T rhs);
    }

    static foreach (tid, T; Types)
    {
        static if (isAssignableTo!T)
        {
            /**
             * Assigns a value to a `SumType`.
             *
             * If any of the `SumType`'s members other than the one being assigned
             * to contain pointers or references, it is possible for the assignment
             * to cause memory corruption (see the
             * ["Memory corruption" example](#memory-corruption) below for an
             * illustration of how). Therefore, such assignments are considered
             * `@system`.
             *
             * An individual assignment can be `@trusted` if the caller can
             * guarantee that there are no outstanding references to any `SumType`
             * members that contain pointers or references at the time the
             * assignment occurs.
             *
             * Examples:
             *
             * $(DIVID memory-corruption, $(H3 Memory corruption))
             *
             * This example shows how assignment to a `SumType` can be used to
             * cause memory corruption in `@system` code. In `@safe` code, the
             * assignment `s = 123` would not be allowed.
             *
             * ---
             * SumType!(int*, int) s = new int;
             * s.tryMatch!(
             *     (ref int* p) {
             *         s = 123; // overwrites `p`
             *         return *p; // undefined behavior
             *     }
             * );
             * ---
             */
            ref SumType opAssign(T rhs)
            {
                import core.lifetime : forward;
                import std.traits : hasIndirections, hasNested;
                import std.meta : AliasSeq, Or = templateOr;

                alias OtherTypes =
                    AliasSeq!(Types[0 .. tid], Types[tid + 1 .. $]);
                enum unsafeToOverwrite =
                    anySatisfy!(Or!(hasIndirections, hasNested), OtherTypes);

                static if (unsafeToOverwrite)
                {
                    cast(void) () @system {}();
                }

                this.match!destroyIfOwner;

                static if (isCopyable!T)
                {
                    // Workaround for https://issues.dlang.org/show_bug.cgi?id=21542
                    mixin("Storage newStorage = { ",
                        Storage.memberName!T, ": __ctfe ? rhs : forward!rhs",
                    " };");
                }
                else
                {
                    mixin("Storage newStorage = { ",
                        Storage.memberName!T, ": forward!rhs",
                    " };");
                }

                storage = newStorage;
                tag = tid;

                return this;
            }
        }
    }

    static if (allSatisfy!(isAssignableTo, Types))
    {
        static if (allSatisfy!(isCopyable, Types))
        {
            /**
             * Copies the value from another `SumType` into this one.
             *
             * See the value-assignment overload for details on `@safe`ty.
             *
             * Copy assignment is `@disable`d if any of `Types` is non-copyable.
             */
            ref SumType opAssign(ref SumType rhs)
            {
                rhs.match!((ref value) { this = value; });
                return this;
            }
        }
        else
        {
            @disable ref SumType opAssign(ref SumType rhs);
        }

        /**
         * Moves the value from another `SumType` into this one.
         *
         * See the value-assignment overload for details on `@safe`ty.
         */
        ref SumType opAssign(SumType rhs)
        {
            import core.lifetime : move;

            rhs.match!((ref value) {
                static if (isCopyable!(typeof(value)))
                {
                    // Workaround for https://issues.dlang.org/show_bug.cgi?id=21542
                    this = __ctfe ? value : move(value);
                }
                else
                {
                    this = move(value);
                }
            });
            return this;
        }
    }

    /**
     * Compares two `SumType`s for equality.
     *
     * Two `SumType`s are equal if they are the same kind of `SumType`, they
     * contain values of the same type, and those values are equal.
     */
    bool opEquals(this This, Rhs)(auto ref Rhs rhs)
    if (!is(CommonType!(This, Rhs) == void))
    {
        static if (is(This == Rhs))
        {
            return AliasSeq!(this, rhs).match!((ref value, ref rhsValue) {
                static if (is(typeof(value) == typeof(rhsValue)))
                {
                    return value == rhsValue;
                }
                else
                {
                    return false;
                }
            });
        }
        else
        {
            alias CommonSumType = CommonType!(This, Rhs);
            return cast(CommonSumType) this == cast(CommonSumType) rhs;
        }
    }

    // Workaround for https://issues.dlang.org/show_bug.cgi?id=19407
    static if (__traits(compiles, anySatisfy!(hasElaborateDestructor, Types)))
    {
        // If possible, include the destructor only when it's needed
        private enum includeDtor = anySatisfy!(hasElaborateDestructor, Types);
    }
    else
    {
        // If we can't tell, always include it, even when it does nothing
        private enum includeDtor = true;
    }

    static if (includeDtor)
    {
        /// Calls the destructor of the `SumType`'s current value.
        ~this()
        {
            this.match!destroyIfOwner;
        }
    }

    // Workaround for https://issues.dlang.org/show_bug.cgi?id=21400
    version (StdDdoc)
    {
        /**
         * Returns a string representation of the `SumType`'s current value.
         *
         * Not available when compiled with `-betterC`.
         */
        string toString(this This)();

        /**
         * Handles formatted writing of the `SumType`'s current value.
         *
         * Not available when compiled with `-betterC`.
         *
         * Params:
         *   sink = Output range to write to.
         *   fmt = Format specifier to use.
         *
         * See_Also: $(REF formatValue, std,format)
         */
        void toString(this This, Sink, Char)(ref Sink sink, const ref FormatSpec!Char fmt);
    }

    version (D_BetterC) {} else
    /**
     * Returns a string representation of the `SumType`'s current value.
     *
     * Not available when compiled with `-betterC`.
     */
    string toString(this This)()
    {
        import std.conv : to;

        return this.match!(to!string);
    }

    version (D_BetterC) {} else
    /**
     * Handles formatted writing of the `SumType`'s current value.
     *
     * Not available when compiled with `-betterC`.
     *
     * Params:
     *   sink = Output range to write to.
     *   fmt = Format specifier to use.
     *
     * See_Also: $(REF formatValue, std,format)
     */
    void toString(this This, Sink, Char)(ref Sink sink, const ref FormatSpec!Char fmt)
    {
        import std.format.write : formatValue;

        this.match!((ref value) {
            formatValue(sink, value, fmt);
        });
    }

    static if (allSatisfy!(isHashable, Map!(ConstOf, Types)))
    {
        // Workaround for https://issues.dlang.org/show_bug.cgi?id=21400
        version (StdDdoc)
        {
            /**
             * Returns the hash of the `SumType`'s current value.
             *
             * Not available when compiled with `-betterC`.
             */
            size_t toHash() const;
        }

        // Workaround for https://issues.dlang.org/show_bug.cgi?id=20095
        version (D_BetterC) {} else
        /**
         * Returns the hash of the `SumType`'s current value.
         *
         * Not available when compiled with `-betterC`.
         */
        size_t toHash() const
        {
            return this.match!hashOf;
        }
    }
}

// Construction
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    MySum y = MySum(3.14);
}

// Assignment
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    x = 3.14;
}

// Self assignment
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    MySum y = MySum(3.14);
    y = x;
}

// Equality
@safe unittest
{
    alias MySum = SumType!(int, float);

    assert(MySum(123) == MySum(123));
    assert(MySum(123) != MySum(456));
    assert(MySum(123) != MySum(123.0));
    assert(MySum(123) != MySum(456.0));

}

// Equality of differently-qualified SumTypes
// Disabled in BetterC due to use of dynamic arrays
version (D_BetterC) {} else
@safe unittest
{
    alias SumA = SumType!(int, float);
    alias SumB = SumType!(const(int[]), int[]);
    alias SumC = SumType!(int[], const(int[]));

    int[] ma = [1, 2, 3];
    const(int[]) ca = [1, 2, 3];

    assert(const(SumA)(123) == SumA(123));
    assert(const(SumB)(ma[]) == SumB(ca[]));
    assert(const(SumC)(ma[]) == SumC(ca[]));
}

// Imported types
@safe unittest
{
    import std.typecons : Tuple;

    alias MySum = SumType!(Tuple!(int, int));
}

// const and immutable types
@safe unittest
{
    alias MySum = SumType!(const(int[]), immutable(float[]));
}

// Recursive types
@safe unittest
{
    alias MySum = SumType!(This*);
    assert(is(MySum.Types[0] == MySum*));
}

// Allowed types
@safe unittest
{
    import std.meta : AliasSeq;

    alias MySum = SumType!(int, float, This*);

    assert(is(MySum.Types == AliasSeq!(int, float, MySum*)));
}

// Types with destructors and postblits
@system unittest
{
    int copies;

    static struct Test
    {
        bool initialized = false;
        int* copiesPtr;

        this(this) { (*copiesPtr)++; }
        ~this() { if (initialized) (*copiesPtr)--; }
    }

    alias MySum = SumType!(int, Test);

    Test t = Test(true, &copies);

    {
        MySum x = t;
        assert(copies == 1);
    }
    assert(copies == 0);

    {
        MySum x = 456;
        assert(copies == 0);
    }
    assert(copies == 0);

    {
        MySum x = t;
        assert(copies == 1);
        x = 456;
        assert(copies == 0);
    }

    {
        MySum x = 456;
        assert(copies == 0);
        x = t;
        assert(copies == 1);
    }

    {
        MySum x = t;
        MySum y = x;
        assert(copies == 2);
    }

    {
        MySum x = t;
        MySum y;
        y = x;
        assert(copies == 2);
    }
}

// Doesn't destroy reference types
// Disabled in BetterC due to use of classes
version (D_BetterC) {} else
@system unittest
{
    bool destroyed;

    class C
    {
        ~this()
        {
            destroyed = true;
        }
    }

    struct S
    {
        ~this() {}
    }

    alias MySum = SumType!(S, C);

    C c = new C();
    {
        MySum x = c;
        destroyed = false;
    }
    assert(!destroyed);

    {
        MySum x = c;
        destroyed = false;
        x = S();
        assert(!destroyed);
    }
}

// Types with @disable this()
@safe unittest
{
    static struct NoInit
    {
        @disable this();
    }

    alias MySum = SumType!(NoInit, int);

    assert(!__traits(compiles, MySum()));
    auto _ = MySum(42);
}

// const SumTypes
version (D_BetterC) {} else // not @nogc, https://issues.dlang.org/show_bug.cgi?id=22117
@safe unittest
{
    auto _ = const(SumType!(int[]))([1, 2, 3]);
}

// Equality of const SumTypes
@safe unittest
{
    alias MySum = SumType!int;

    auto _ = const(MySum)(123) == const(MySum)(456);
}

// Compares reference types using value equality
@safe unittest
{
    import std.array : staticArray;

    static struct Field {}
    static struct Struct { Field[] fields; }
    alias MySum = SumType!Struct;

    static arr1 = staticArray([Field()]);
    static arr2 = staticArray([Field()]);

    auto a = MySum(Struct(arr1[]));
    auto b = MySum(Struct(arr2[]));

    assert(a == b);
}

// toString
// Disabled in BetterC due to use of std.conv.text
version (D_BetterC) {} else
@safe unittest
{
    import std.conv : text;

    static struct Int { int i; }
    static struct Double { double d; }
    alias Sum = SumType!(Int, Double);

    assert(Sum(Int(42)).text == Int(42).text, Sum(Int(42)).text);
    assert(Sum(Double(33.3)).text == Double(33.3).text, Sum(Double(33.3)).text);
    assert((const(Sum)(Int(42))).text == (const(Int)(42)).text, (const(Sum)(Int(42))).text);
}

// string formatting
// Disabled in BetterC due to use of std.format.format
version (D_BetterC) {} else
@safe unittest
{
    import std.format : format;

    SumType!int x = 123;

    assert(format!"%s"(x) == format!"%s"(123));
    assert(format!"%x"(x) == format!"%x"(123));
}

// string formatting of qualified SumTypes
// Disabled in BetterC due to use of std.format.format and dynamic arrays
version (D_BetterC) {} else
@safe unittest
{
    import std.format : format;

    int[] a = [1, 2, 3];
    const(SumType!(int[])) x = a;

    assert(format!"%(%d, %)"(x) == format!"%(%s, %)"(a));
}

// Github issue #16
// Disabled in BetterC due to use of dynamic arrays
version (D_BetterC) {} else
@safe unittest
{
    alias Node = SumType!(This[], string);

    // override inference of @system attribute for cyclic functions
    assert((() @trusted =>
        Node([Node([Node("x")])])
        ==
        Node([Node([Node("x")])])
    )());
}

// Github issue #16 with const
// Disabled in BetterC due to use of dynamic arrays
version (D_BetterC) {} else
@safe unittest
{
    alias Node = SumType!(const(This)[], string);

    // override inference of @system attribute for cyclic functions
    assert((() @trusted =>
        Node([Node([Node("x")])])
        ==
        Node([Node([Node("x")])])
    )());
}

// Stale pointers
// Disabled in BetterC due to use of dynamic arrays
version (D_BetterC) {} else
@system unittest
{
    alias MySum = SumType!(ubyte, void*[2]);

    MySum x = [null, cast(void*) 0x12345678];
    void** p = &x.get!(void*[2])[1];
    x = ubyte(123);

    assert(*p != cast(void*) 0x12345678);
}

// Exception-safe assignment
// Disabled in BetterC due to use of exceptions
version (D_BetterC) {} else
@safe unittest
{
    static struct A
    {
        int value = 123;
    }

    static struct B
    {
        int value = 456;
        this(this) { throw new Exception("oops"); }
    }

    alias MySum = SumType!(A, B);

    MySum x;
    try
    {
        x = B();
    }
    catch (Exception e) {}

    assert(
        (x.tag == 0 && x.get!A.value == 123) ||
        (x.tag == 1 && x.get!B.value == 456)
    );
}

// Types with @disable this(this)
@safe unittest
{
    import core.lifetime : move;

    static struct NoCopy
    {
        @disable this(this);
    }

    alias MySum = SumType!NoCopy;

    NoCopy lval = NoCopy();

    MySum x = NoCopy();
    MySum y = NoCopy();


    assert(!__traits(compiles, SumType!NoCopy(lval)));

    y = NoCopy();
    y = move(x);
    assert(!__traits(compiles, y = lval));
    assert(!__traits(compiles, y = x));

    bool b = x == y;
}

// Github issue #22
// Disabled in BetterC due to use of std.typecons.Nullable
version (D_BetterC) {} else
@safe unittest
{
    import std.typecons;

    static struct A
    {
        SumType!(Nullable!int) a = Nullable!int.init;
    }
}

// Static arrays of structs with postblits
// Disabled in BetterC due to use of dynamic arrays
version (D_BetterC) {} else
@safe unittest
{
    static struct S
    {
        int n;
        this(this) { n++; }
    }

    SumType!(S[1]) x = [S(0)];
    SumType!(S[1]) y = x;

    auto xval = x.get!(S[1])[0].n;
    auto yval = y.get!(S[1])[0].n;

    assert(xval != yval);
}

// Replacement does not happen inside SumType
// Disabled in BetterC due to use of associative arrays
version (D_BetterC) {} else
@safe unittest
{
    import std.typecons : Tuple, ReplaceTypeUnless;
    alias A = Tuple!(This*,SumType!(This*))[SumType!(This*,string)[This]];
    alias TR = ReplaceTypeUnless!(isSumTypeInstance, This, int, A);
    static assert(is(TR == Tuple!(int*,SumType!(This*))[SumType!(This*, string)[int]]));
}

// Supports nested self-referential SumTypes
@safe unittest
{
    import std.typecons : Tuple, Flag;
    alias Nat = SumType!(Flag!"0", Tuple!(This*));
    alias Inner = SumType!Nat;
    alias Outer = SumType!(Nat*, Tuple!(This*, This*));
}

// Self-referential SumTypes inside Algebraic
// Disabled in BetterC due to use of std.variant.Algebraic
version (D_BetterC) {} else
@safe unittest
{
    import std.variant : Algebraic;

    alias T = Algebraic!(SumType!(This*));

    assert(is(T.AllowedTypes[0].Types[0] == T.AllowedTypes[0]*));
}

// Doesn't call @system postblits in @safe code
@safe unittest
{
    static struct SystemCopy { @system this(this) {} }
    SystemCopy original;

    assert(!__traits(compiles, () @safe
            {
        SumType!SystemCopy copy = original;
    }));

    assert(!__traits(compiles, () @safe
            {
        SumType!SystemCopy copy; copy = original;
    }));
}

// Doesn't overwrite pointers in @safe code
@safe unittest
{
    alias MySum = SumType!(int*, int);

    MySum x;

    assert(!__traits(compiles, () @safe
            {
        x = 123;
    }));

    assert(!__traits(compiles, () @safe
            {
        x = MySum(123);
    }));
}

// Calls value postblit on self-assignment
@safe unittest
{
    static struct S
    {
        int n;
        this(this) { n++; }
    }

    SumType!S x = S();
    SumType!S y;
    y = x;

    auto xval = x.get!S.n;
    auto yval = y.get!S.n;

    assert(xval != yval);
}

// Github issue #29
@safe unittest
{
    alias A = SumType!string;

    @safe A createA(string arg)
    {
        return A(arg);
    }

    @safe void test()
    {
        A a = createA("");
    }
}

// SumTypes as associative array keys
// Disabled in BetterC due to use of associative arrays
version (D_BetterC) {} else
@safe unittest
{
    int[SumType!(int, string)] aa;
}

// toString with non-copyable types
// Disabled in BetterC due to use of std.conv.to (in toString)
version (D_BetterC) {} else
@safe unittest
{
    struct NoCopy
    {
        @disable this(this);
    }

    SumType!NoCopy x;

    auto _ = x.toString();
}

// Can use the result of assignment
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum a = MySum(123);
    MySum b = MySum(3.14);

    assert((a = b) == b);
    assert((a = MySum(123)) == MySum(123));
    assert((a = 3.14) == MySum(3.14));
    assert(((a = b) = MySum(123)) == MySum(123));
}

// Types with copy constructors
@safe unittest
{
    static struct S
    {
        int n;

        this(ref return scope inout S other) inout
        {
            n = other.n + 1;
        }
    }

    SumType!S x = S();
    SumType!S y = x;

    auto xval = x.get!S.n;
    auto yval = y.get!S.n;

    assert(xval != yval);
}

// Copyable by generated copy constructors
@safe unittest
{
    static struct Inner
    {
        ref this(ref inout Inner other) {}
    }

    static struct Outer
    {
        SumType!Inner inner;
    }

    Outer x;
    Outer y = x;
}

// Types with qualified copy constructors
@safe unittest
{
    static struct ConstCopy
    {
        int n;
        this(inout int n) inout { this.n = n; }
        this(ref const typeof(this) other) const { this.n = other.n; }
    }

    static struct ImmutableCopy
    {
        int n;
        this(inout int n) inout { this.n = n; }
        this(ref immutable typeof(this) other) immutable { this.n = other.n; }
    }

    const SumType!ConstCopy x = const(ConstCopy)(1);
    immutable SumType!ImmutableCopy y = immutable(ImmutableCopy)(1);
}

// Types with disabled opEquals
@safe unittest
{
    static struct S
    {
        @disable bool opEquals(const S rhs) const;
    }

    auto _ = SumType!S(S());
}

// Types with non-const opEquals
@safe unittest
{
    static struct S
    {
        int i;
        bool opEquals(S rhs) { return i == rhs.i; }
    }

    auto _ = SumType!S(S(123));
}

// Incomparability of different SumTypes
@safe unittest
{
    SumType!(int, string) x = 123;
    SumType!(string, int) y = 123;

    assert(!__traits(compiles, x != y));
}

// Self-reference in return/parameter type of function pointer member
// Disabled in BetterC due to use of delegates
version (D_BetterC) {} else
@safe unittest
{
    alias T = SumType!(int, This delegate(This));
}

// Construction and assignment from implicitly-convertible lvalue
@safe unittest
{
    alias MySum = SumType!bool;

    const(bool) b = true;

    MySum x = b;
    MySum y; y = b;
}

// @safe assignment to the only pointer type in a SumType
@safe unittest
{
    SumType!(string, int) sm = 123;
    sm = "this should be @safe";
}

// Pointers to local variables
// https://issues.dlang.org/show_bug.cgi?id=22117
@safe unittest
{
    int n = 123;
    immutable int ni = 456;

    SumType!(int*) s = &n;
    const SumType!(int*) sc = &n;
    immutable SumType!(int*) si = &ni;
}

// Immutable member type with copy constructor
// https://issues.dlang.org/show_bug.cgi?id=22572
@safe unittest
{
    static struct CopyConstruct
    {
        this(ref inout CopyConstruct other) inout {}
    }

    static immutable struct Value
    {
        CopyConstruct c;
    }

    SumType!Value s;
}

// Construction of inout-qualified SumTypes
// https://issues.dlang.org/show_bug.cgi?id=22901
@safe unittest
{
    static inout(SumType!(int[])) example(inout(int[]) arr)
    {
        return inout(SumType!(int[]))(arr);
    }
}

// Assignment of struct with overloaded opAssign in CTFE
// https://issues.dlang.org/show_bug.cgi?id=23182
@safe unittest
{
    static struct HasOpAssign
    {
        void opAssign(HasOpAssign rhs) {}
    }

    static SumType!HasOpAssign test()
    {
        SumType!HasOpAssign s;
        // Test both overloads
        s = HasOpAssign();
        s = SumType!HasOpAssign();
        return s;
    }

    // Force CTFE
    enum result = test();
}

/// True if `T` is an instance of the `SumType` template, otherwise false.
private enum bool isSumTypeInstance(T) = is(T == SumType!Args, Args...);

@safe unittest
{
    static struct Wrapper
    {
        SumType!int s;
        alias s this;
    }

    assert(isSumTypeInstance!(SumType!int));
    assert(!isSumTypeInstance!Wrapper);
}

/// True if `T` is a [SumType] or implicitly converts to one, otherwise false.
enum bool isSumType(T) = is(T : SumType!Args, Args...);

///
@safe unittest
{
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

/**
 * Calls a type-appropriate function with the value held in a [SumType].
 *
 * For each possible type the [SumType] can hold, the given handlers are
 * checked, in order, to see whether they accept a single argument of that type.
 * The first one that does is chosen as the match for that type. (Note that the
 * first match may not always be the most exact match.
 * See ["Avoiding unintentional matches"](#avoiding-unintentional-matches) for
 * one common pitfall.)
 *
 * Every type must have a matching handler, and every handler must match at
 * least one type. This is enforced at compile time.
 *
 * Handlers may be functions, delegates, or objects with `opCall` overloads. If
 * a function with more than one overload is given as a handler, all of the
 * overloads are considered as potential matches.
 *
 * Templated handlers are also accepted, and will match any type for which they
 * can be [implicitly instantiated](https://dlang.org/glossary.html#ifti).
 * (Remember that a $(DDSUBLINK spec/expression,function_literals, function literal)
 * without an explicit argument type is considered a template.)
 *
 * If multiple [SumType]s are passed to match, their values are passed to the
 * handlers as separate arguments, and matching is done for each possible
 * combination of value types. See ["Multiple dispatch"](#multiple-dispatch) for
 * an example.
 *
 * Returns:
 *   The value returned from the handler that matches the currently-held type.
 *
 * See_Also: $(REF visit, std,variant)
 */
template match(handlers...)
{
    import std.typecons : Yes;

    /**
     * The actual `match` function.
     *
     * Params:
     *   args = One or more [SumType] objects.
     */
    auto ref match(SumTypes...)(auto ref SumTypes args)
    if (allSatisfy!(isSumType, SumTypes) && args.length > 0)
    {
        return matchImpl!(Yes.exhaustive, handlers)(args);
    }
}

/** $(DIVID avoiding-unintentional-matches, $(H3 Avoiding unintentional matches))
 *
 * Sometimes, implicit conversions may cause a handler to match more types than
 * intended. The example below shows two solutions to this problem.
 */
@safe unittest
{
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

/** $(DIVID multiple-dispatch, $(H3 Multiple dispatch))
 *
 * Pattern matching can be performed on multiple `SumType`s at once by passing
 * handlers with multiple arguments. This usually leads to more concise code
 * than using nested calls to `match`, as show below.
 */
@safe unittest
{
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

/**
 * Attempts to call a type-appropriate function with the value held in a
 * [SumType], and throws on failure.
 *
 * Matches are chosen using the same rules as [match], but are not required to
 * be exhaustive—in other words, a type (or combination of types) is allowed to
 * have no matching handler. If a type without a handler is encountered at
 * runtime, a [MatchException] is thrown.
 *
 * Not available when compiled with `-betterC`.
 *
 * Returns:
 *   The value returned from the handler that matches the currently-held type,
 *   if a handler was given for that type.
 *
 * Throws:
 *   [MatchException], if the currently-held type has no matching handler.
 *
 * See_Also: $(REF tryVisit, std,variant)
 */
version (D_Exceptions)
template tryMatch(handlers...)
{
    import std.typecons : No;

    /**
     * The actual `tryMatch` function.
     *
     * Params:
     *   args = One or more [SumType] objects.
     */
    auto ref tryMatch(SumTypes...)(auto ref SumTypes args)
    if (allSatisfy!(isSumType, SumTypes) && args.length > 0)
    {
        return matchImpl!(No.exhaustive, handlers)(args);
    }
}

/**
 * Thrown by [tryMatch] when an unhandled type is encountered.
 *
 * Not available when compiled with `-betterC`.
 */
version (D_Exceptions)
class MatchException : Exception
{
    ///
    pure @safe @nogc nothrow
    this(string msg, string file = __FILE__, size_t line = __LINE__)
    {
        super(msg, file, line);
    }
}

/**
 * True if `handler` is a potential match for `Ts`, otherwise false.
 *
 * See the documentation for [match] for a full explanation of how matches are
 * chosen.
 */
template canMatch(alias handler, Ts...)
if (Ts.length > 0)
{
    enum canMatch = is(typeof((ref Ts args) => handler(args)));
}

///
@safe unittest
{
    alias handleInt = (int i) => "got an int";

    assert( canMatch!(handleInt, int));
    assert(!canMatch!(handleInt, string));
}

// Includes all overloads of the given handler
@safe unittest
{
    static struct OverloadSet
    {
        static void fun(int n) {}
        static void fun(double d) {}
    }

    assert(canMatch!(OverloadSet.fun, int));
    assert(canMatch!(OverloadSet.fun, double));
}

// Like aliasSeqOf!(iota(n)), but works in BetterC
private template Iota(size_t n)
{
    static if (n == 0)
    {
        alias Iota = AliasSeq!();
    }
    else
    {
        alias Iota = AliasSeq!(Iota!(n - 1), n - 1);
    }
}

@safe unittest
{
    assert(is(Iota!0 == AliasSeq!()));
    assert(Iota!1 == AliasSeq!(0));
    assert(Iota!3 == AliasSeq!(0, 1, 2));
}

private template matchImpl(Flag!"exhaustive" exhaustive, handlers...)
{
    auto ref matchImpl(SumTypes...)(auto ref SumTypes args)
    if (allSatisfy!(isSumType, SumTypes) && args.length > 0)
    {
        // Single dispatch (fast path)
        static if (args.length == 1)
        {
            /* When there's only one argument, the caseId is just that
             * argument's tag, so there's no need for TagTuple.
             */
            enum handlerArgs(size_t caseId) =
                "args[0].get!(SumTypes[0].Types[" ~ toCtString!caseId ~ "])()";

            alias valueTypes(size_t caseId) =
                typeof(args[0].get!(SumTypes[0].Types[caseId])());

            enum numCases = SumTypes[0].Types.length;
        }
        // Multiple dispatch (slow path)
        else
        {
            alias typeCounts = Map!(typeCount, SumTypes);
            alias stride(size_t i) = .stride!(i, typeCounts);
            alias TagTuple = .TagTuple!typeCounts;

            alias handlerArgs(size_t caseId) = .handlerArgs!(caseId, typeCounts);

            /* An AliasSeq of the types of the member values in the argument list
             * returned by `handlerArgs!caseId`.
             *
             * Note that these are the actual (that is, qualified) types of the
             * member values, which may not be the same as the types listed in
             * the arguments' `.Types` properties.
             */
            template valueTypes(size_t caseId)
            {
                enum tags = TagTuple.fromCaseId(caseId);

                template getType(size_t i)
                {
                    enum tid = tags[i];
                    alias T = SumTypes[i].Types[tid];
                    alias getType = typeof(args[i].get!T());
                }

                alias valueTypes = Map!(getType, Iota!(tags.length));
            }

            /* The total number of cases is
             *
             *   Π SumTypes[i].Types.length for 0 ≤ i < SumTypes.length
             *
             * Conveniently, this is equal to stride!(SumTypes.length), so we can
             * use that function to compute it.
             */
            enum numCases = stride!(SumTypes.length);
        }

        /* Guaranteed to never be a valid handler index, since
         * handlers.length <= size_t.max.
         */
        enum noMatch = size_t.max;

        // An array that maps caseIds to handler indices ("hids").
        enum matches = ()
        {
            size_t[numCases] result;

            // Workaround for https://issues.dlang.org/show_bug.cgi?id=19561
            foreach (ref match; result)
            {
                match = noMatch;
            }

            static foreach (caseId; 0 .. numCases)
            {
                static foreach (hid, handler; handlers)
                {
                    static if (canMatch!(handler, valueTypes!caseId))
                    {
                        if (result[caseId] == noMatch)
                        {
                            result[caseId] = hid;
                        }
                    }
                }
            }

            return result;
        }();

        import std.algorithm.searching : canFind;

        // Check for unreachable handlers
        static foreach (hid, handler; handlers)
        {
            static assert(matches[].canFind(hid),
                "`handlers[" ~ toCtString!hid ~ "]` " ~
                "of type `" ~ ( __traits(isTemplate, handler)
                    ? "template"
                    : typeof(handler).stringof
                ) ~ "` " ~
                "never matches. Perhaps the handler failed to compile"
            );
        }

        // Workaround for https://issues.dlang.org/show_bug.cgi?id=19993
        enum handlerName(size_t hid) = "handler" ~ toCtString!hid;

        static foreach (size_t hid, handler; handlers)
        {
            mixin("alias ", handlerName!hid, " = handler;");
        }

        // Single dispatch (fast path)
        static if (args.length == 1)
            immutable argsId = args[0].tag;
        // Multiple dispatch (slow path)
        else
            immutable argsId = TagTuple(args).toCaseId;

        final switch (argsId)
        {
            static foreach (caseId; 0 .. numCases)
            {
                case caseId:
                    static if (matches[caseId] != noMatch)
                    {
                        return mixin(handlerName!(matches[caseId]), "(", handlerArgs!caseId, ")");
                    }
                    else
                    {
                        static if (exhaustive)
                        {
                            static assert(false,
                                "No matching handler for types `" ~ valueTypes!caseId.stringof ~ "`");
                        }
                        else
                        {
                            throw new MatchException(
                                "No matching handler for types `" ~ valueTypes!caseId.stringof ~ "`");
                        }
                    }
            }
        }

        assert(false, "unreachable");
    }
}

// Predicate for staticMap
private enum typeCount(SumType) = SumType.Types.length;

/* A TagTuple represents a single possible set of tags that the arguments to
 * `matchImpl` could have at runtime.
 *
 * Because D does not allow a struct to be the controlling expression
 * of a switch statement, we cannot dispatch on the TagTuple directly.
 * Instead, we must map each TagTuple to a unique integer and generate
 * a case label for each of those integers.
 *
 * This mapping is implemented in `fromCaseId` and `toCaseId`. It uses
 * the same technique that's used to map index tuples to memory offsets
 * in a multidimensional static array.
 *
 * For example, when `args` consists of two SumTypes with two member
 * types each, the TagTuples corresponding to each case label are:
 *
 *   case 0:  TagTuple([0, 0])
 *   case 1:  TagTuple([1, 0])
 *   case 2:  TagTuple([0, 1])
 *   case 3:  TagTuple([1, 1])
 *
 * When there is only one argument, the caseId is equal to that
 * argument's tag.
 */
private struct TagTuple(typeCounts...)
{
    size_t[typeCounts.length] tags;
    alias tags this;

    alias stride(size_t i) = .stride!(i, typeCounts);

    invariant
    {
        static foreach (i; 0 .. tags.length)
        {
            assert(tags[i] < typeCounts[i], "Invalid tag");
        }
    }

    this(SumTypes...)(ref const SumTypes args)
    if (allSatisfy!(isSumType, SumTypes) && args.length == typeCounts.length)
    {
        static foreach (i; 0 .. tags.length)
        {
            tags[i] = args[i].tag;
        }
    }

    static TagTuple fromCaseId(size_t caseId)
    {
        TagTuple result;

        // Most-significant to least-significant
        static foreach_reverse (i; 0 .. result.length)
        {
            result[i] = caseId / stride!i;
            caseId %= stride!i;
        }

        return result;
    }

    size_t toCaseId()
    {
        size_t result;

        static foreach (i; 0 .. tags.length)
        {
            result += tags[i] * stride!i;
        }

        return result;
    }
}

/* The number that the dim-th argument's tag is multiplied by when
 * converting TagTuples to and from case indices ("caseIds").
 *
 * Named by analogy to the stride that the dim-th index into a
 * multidimensional static array is multiplied by to calculate the
 * offset of a specific element.
 */
private size_t stride(size_t dim, lengths...)()
{
    import core.checkedint : mulu;

    size_t result = 1;
    bool overflow = false;

    static foreach (i; 0 .. dim)
    {
        result = mulu(result, lengths[i], overflow);
    }

    /* The largest number matchImpl uses, numCases, is calculated with
     * stride!(SumTypes.length), so as long as this overflow check
     * passes, we don't need to check for overflow anywhere else.
     */
    assert(!overflow, "Integer overflow");
    return result;
}

/* A list of arguments to be passed to a handler needed for the case
 * labeled with `caseId`.
 */
private template handlerArgs(size_t caseId, typeCounts...)
{
    enum tags = TagTuple!typeCounts.fromCaseId(caseId);

    alias handlerArgs = AliasSeq!();

    static foreach (i; 0 .. tags.length)
    {
        handlerArgs = AliasSeq!(
            handlerArgs,
            "args[" ~ toCtString!i ~ "].get!(SumTypes[" ~ toCtString!i ~ "]" ~
            ".Types[" ~ toCtString!(tags[i]) ~ "])(), "
        );
    }
}

// Matching
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    MySum y = MySum(3.14);

    assert(x.match!((int v) => true, (float v) => false));
    assert(y.match!((int v) => false, (float v) => true));
}

// Missing handlers
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum x = MySum(42);

    assert(!__traits(compiles, x.match!((int x) => true)));
    assert(!__traits(compiles, x.match!()));
}

// Handlers with qualified parameters
// Disabled in BetterC due to use of dynamic arrays
version (D_BetterC) {} else
@safe unittest
{
    alias MySum = SumType!(int[], float[]);

    MySum x = MySum([1, 2, 3]);
    MySum y = MySum([1.0, 2.0, 3.0]);

    assert(x.match!((const(int[]) v) => true, (const(float[]) v) => false));
    assert(y.match!((const(int[]) v) => false, (const(float[]) v) => true));
}

// Handlers for qualified types
// Disabled in BetterC due to use of dynamic arrays
version (D_BetterC) {} else
@safe unittest
{
    alias MySum = SumType!(immutable(int[]), immutable(float[]));

    MySum x = MySum([1, 2, 3]);

    assert(x.match!((immutable(int[]) v) => true, (immutable(float[]) v) => false));
    assert(x.match!((const(int[]) v) => true, (const(float[]) v) => false));
    // Tail-qualified parameters
    assert(x.match!((immutable(int)[] v) => true, (immutable(float)[] v) => false));
    assert(x.match!((const(int)[] v) => true, (const(float)[] v) => false));
    // Generic parameters
    assert(x.match!((immutable v) => true));
    assert(x.match!((const v) => true));
    // Unqualified parameters
    assert(!__traits(compiles,
        x.match!((int[] v) => true, (float[] v) => false)
    ));
}

// Delegate handlers
// Disabled in BetterC due to use of closures
version (D_BetterC) {} else
@safe unittest
{
    alias MySum = SumType!(int, float);

    int answer = 42;
    MySum x = MySum(42);
    MySum y = MySum(3.14);

    assert(x.match!((int v) => v == answer, (float v) => v == answer));
    assert(!y.match!((int v) => v == answer, (float v) => v == answer));
}

version (unittest)
{
    version (D_BetterC)
    {
        // std.math.isClose depends on core.runtime.math, so use a
        // libc-based version for testing with -betterC
        @safe pure @nogc nothrow
        private bool isClose(double lhs, double rhs)
        {
            import core.stdc.math : fabs;

            return fabs(lhs - rhs) < 1e-5;
        }
    }
    else
    {
        import std.math.operations : isClose;
    }
}

// Generic handler
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    MySum y = MySum(3.14);

    assert(x.match!(v => v*2) == 84);
    assert(y.match!(v => v*2).isClose(6.28));
}

// Fallback to generic handler
// Disabled in BetterC due to use of std.conv.to
version (D_BetterC) {} else
@safe unittest
{
    import std.conv : to;

    alias MySum = SumType!(int, float, string);

    MySum x = MySum(42);
    MySum y = MySum("42");

    assert(x.match!((string v) => v.to!int, v => v*2) == 84);
    assert(y.match!((string v) => v.to!int, v => v*2) == 42);
}

// Multiple non-overlapping generic handlers
@safe unittest
{
    import std.array : staticArray;

    alias MySum = SumType!(int, float, int[], char[]);

    static ints = staticArray([1, 2, 3]);
    static chars = staticArray(['a', 'b', 'c']);

    MySum x = MySum(42);
    MySum y = MySum(3.14);
    MySum z = MySum(ints[]);
    MySum w = MySum(chars[]);

    assert(x.match!(v => v*2, v => v.length) == 84);
    assert(y.match!(v => v*2, v => v.length).isClose(6.28));
    assert(w.match!(v => v*2, v => v.length) == 3);
    assert(z.match!(v => v*2, v => v.length) == 3);
}

// Structural matching
@safe unittest
{
    static struct S1 { int x; }
    static struct S2 { int y; }
    alias MySum = SumType!(S1, S2);

    MySum a = MySum(S1(0));
    MySum b = MySum(S2(0));

    assert(a.match!(s1 => s1.x + 1, s2 => s2.y - 1) == 1);
    assert(b.match!(s1 => s1.x + 1, s2 => s2.y - 1) == -1);
}

// Separate opCall handlers
@safe unittest
{
    static struct IntHandler
    {
        bool opCall(int arg)
        {
            return true;
        }
    }

    static struct FloatHandler
    {
        bool opCall(float arg)
        {
            return false;
        }
    }

    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    MySum y = MySum(3.14);

    assert(x.match!(IntHandler.init, FloatHandler.init));
    assert(!y.match!(IntHandler.init, FloatHandler.init));
}

// Compound opCall handler
@safe unittest
{
    static struct CompoundHandler
    {
        bool opCall(int arg)
        {
            return true;
        }

        bool opCall(float arg)
        {
            return false;
        }
    }

    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    MySum y = MySum(3.14);

    assert(x.match!(CompoundHandler.init));
    assert(!y.match!(CompoundHandler.init));
}

// Ordered matching
@safe unittest
{
    alias MySum = SumType!(int, float);

    MySum x = MySum(42);

    assert(x.match!((int v) => true, v => false));
}

// Non-exhaustive matching
version (D_Exceptions)
@system unittest
{
    import std.exception : assertThrown, assertNotThrown;

    alias MySum = SumType!(int, float);

    MySum x = MySum(42);
    MySum y = MySum(3.14);

    assertNotThrown!MatchException(x.tryMatch!((int n) => true));
    assertThrown!MatchException(y.tryMatch!((int n) => true));
}

// Non-exhaustive matching in @safe code
version (D_Exceptions)
@safe unittest
{
    SumType!(int, float) x;

    auto _ = x.tryMatch!(
        (int n) => n + 1,
    );
}

// Handlers with ref parameters
@safe unittest
{
    alias Value = SumType!(long, double);

    auto value = Value(3.14);

    value.match!(
        (long) {},
        (ref double d) { d *= 2; }
    );

    assert(value.get!double.isClose(6.28));
}

// Unreachable handlers
@safe unittest
{
    alias MySum = SumType!(int, string);

    MySum s;

    assert(!__traits(compiles,
        s.match!(
            (int _) => 0,
            (string _) => 1,
            (double _) => 2
        )
    ));

    assert(!__traits(compiles,
        s.match!(
            _ => 0,
            (int _) => 1
        )
    ));
}

// Unsafe handlers
@system unittest
{
    SumType!int x;
    alias unsafeHandler = (int x) @system { return; };

    assert(!__traits(compiles, () @safe
            {
        x.match!unsafeHandler;
    }));

    auto test() @system
    {
        return x.match!unsafeHandler;
    }
}

// Overloaded handlers
@safe unittest
{
    static struct OverloadSet
    {
        static string fun(int i) { return "int"; }
        static string fun(double d) { return "double"; }
    }

    alias MySum = SumType!(int, double);

    MySum a = 42;
    MySum b = 3.14;

    assert(a.match!(OverloadSet.fun) == "int");
    assert(b.match!(OverloadSet.fun) == "double");
}

// Overload sets that include SumType arguments
@safe unittest
{
    alias Inner = SumType!(int, double);
    alias Outer = SumType!(Inner, string);

    static struct OverloadSet
    {
        @safe:
        static string fun(int i) { return "int"; }
        static string fun(double d) { return "double"; }
        static string fun(string s) { return "string"; }
        static string fun(Inner i) { return i.match!fun; }
        static string fun(Outer o) { return o.match!fun; }
    }

    Outer a = Inner(42);
    Outer b = Inner(3.14);
    Outer c = "foo";

    assert(OverloadSet.fun(a) == "int");
    assert(OverloadSet.fun(b) == "double");
    assert(OverloadSet.fun(c) == "string");
}

// Overload sets with ref arguments
@safe unittest
{
    static struct OverloadSet
    {
        static void fun(ref int i) { i = 42; }
        static void fun(ref double d) { d = 3.14; }
    }

    alias MySum = SumType!(int, double);

    MySum x = 0;
    MySum y = 0.0;

    x.match!(OverloadSet.fun);
    y.match!(OverloadSet.fun);

    assert(x.match!((value) => is(typeof(value) == int) && value == 42));
    assert(y.match!((value) => is(typeof(value) == double) && value == 3.14));
}

// Overload sets with templates
@safe unittest
{
    import std.traits : isNumeric;

    static struct OverloadSet
    {
        static string fun(string arg)
        {
            return "string";
        }

        static string fun(T)(T arg)
        if (isNumeric!T)
        {
            return "numeric";
        }
    }

    alias MySum = SumType!(int, string);

    MySum x = 123;
    MySum y = "hello";

    assert(x.match!(OverloadSet.fun) == "numeric");
    assert(y.match!(OverloadSet.fun) == "string");
}

// Github issue #24
@safe unittest
{
    void test() @nogc
    {
        int acc = 0;
        SumType!int(1).match!((int x) => acc += x);
    }
}

// Github issue #31
@safe unittest
{
    void test() @nogc
    {
        int acc = 0;

        SumType!(int, string)(1).match!(
            (int x) => acc += x,
            (string _) => 0,
        );
    }
}

// Types that `alias this` a SumType
@safe unittest
{
    static struct A {}
    static struct B {}
    static struct D { SumType!(A, B) value; alias value this; }

    auto _ = D().match!(_ => true);
}

// Multiple dispatch
@safe unittest
{
    alias MySum = SumType!(int, string);

    static int fun(MySum x, MySum y)
    {
        import std.meta : Args = AliasSeq;

        return Args!(x, y).match!(
            (int    xv, int    yv) => 0,
            (string xv, int    yv) => 1,
            (int    xv, string yv) => 2,
            (string xv, string yv) => 3
        );
    }

    assert(fun(MySum(0),  MySum(0))  == 0);
    assert(fun(MySum(""), MySum(0))  == 1);
    assert(fun(MySum(0),  MySum("")) == 2);
    assert(fun(MySum(""), MySum("")) == 3);
}

// inout SumTypes
@safe unittest
{
    inout(int[]) fun(inout(SumType!(int[])) x)
    {
        return x.match!((inout(int[]) a) => a);
    }
}

// return ref
// issue: https://issues.dlang.org/show_bug.cgi?id=23101
@safe unittest
{
    static assert(!__traits(compiles, () {
        SumType!(int, string) st;
        return st.match!(
            function int* (string x) => assert(0),
            function int* (return ref int i) => &i,
        );
    }));

    SumType!(int, string) st;
    static assert(__traits(compiles, () {
        return st.match!(
            function int* (string x) => null,
            function int* (return ref int i) => &i,
        );
    }));
}

private void destroyIfOwner(T)(ref T value)
{
    static if (hasElaborateDestructor!T)
    {
        destroy(value);
    }
}

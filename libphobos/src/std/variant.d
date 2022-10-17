// Written in the D programming language.

/**
This module implements a
$(HTTP erdani.org/publications/cuj-04-2002.php.html,discriminated union)
type (a.k.a.
$(HTTP en.wikipedia.org/wiki/Tagged_union,tagged union),
$(HTTP en.wikipedia.org/wiki/Algebraic_data_type,algebraic type)).
Such types are useful
for type-uniform binary interfaces, interfacing with scripting
languages, and comfortable exploratory programming.

A $(LREF Variant) object can hold a value of any type, with very few
restrictions (such as `shared` types and noncopyable types). Setting the value
is as immediate as assigning to the `Variant` object. To read back the value of
the appropriate type `T`, use the $(LREF get) method. To query whether a
`Variant` currently holds a value of type `T`, use $(LREF peek). To fetch the
exact type currently held, call $(LREF type), which returns the `TypeInfo` of
the current value.

In addition to $(LREF Variant), this module also defines the $(LREF Algebraic)
type constructor. Unlike `Variant`, `Algebraic` only allows a finite set of
types, which are specified in the instantiation (e.g. $(D Algebraic!(int,
string)) may only hold an `int` or a `string`).

$(RED Warning: $(LREF Algebraic) is outdated and not recommended for use in new
code. Instead, use $(REF SumType, std,sumtype).)

Credits: Reviewed by Brad Roberts. Daniel Keep provided a detailed code review
prompting the following improvements: (1) better support for arrays; (2) support
for associative arrays; (3) friendlier behavior towards the garbage collector.
Copyright: Copyright Andrei Alexandrescu 2007 - 2015.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP erdani.org, Andrei Alexandrescu)
Source:    $(PHOBOSSRC std/variant.d)
*/
module std.variant;

import std.meta, std.traits, std.typecons;

///
@system unittest
{
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

/++
    Gives the `sizeof` the largest type given.

    See_Also: $(LINK https://forum.dlang.org/thread/wbpnncxepehgcswhuazl@forum.dlang.org?page=1)
  +/
template maxSize(Ts...)
{
    align(1) union Impl
    {
        static foreach (i, T; Ts)
        {
            static if (!is(T == void))
                mixin("T _field_", i, ";");
        }
    }
    enum maxSize = Impl.sizeof;
}

///
@safe unittest
{
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

struct This;

private alias This2Variant(V, T...) = AliasSeq!(ReplaceTypeUnless!(isAlgebraic, This, V, T));

// We can't just use maxAlignment because no types might be specified
// to VariantN, so handle that here and then pass along the rest.
private template maxVariantAlignment(U...)
if (isTypeTuple!U)
{
    static if (U.length == 0)
    {
        import std.algorithm.comparison : max;
        enum maxVariantAlignment = max(real.alignof, size_t.alignof);
    }
    else
        enum maxVariantAlignment = maxAlignment!(U);
}

/**
 * Back-end type seldom used directly by user
 * code. Two commonly-used types using `VariantN` are:
 *
 * $(OL $(LI $(LREF Algebraic): A closed discriminated union with a
 * limited type universe (e.g., $(D Algebraic!(int, double,
 * string)) only accepts these three types and rejects anything
 * else).) $(LI $(LREF Variant): An open discriminated union allowing an
 * unbounded set of types. If any of the types in the `Variant`
 * are larger than the largest built-in type, they will automatically
 * be boxed. This means that even large types will only be the size
 * of a pointer within the `Variant`, but this also implies some
 * overhead. `Variant` can accommodate all primitive types and
 * all user-defined types.))
 *
 * Both `Algebraic` and `Variant` share $(D
 * VariantN)'s interface. (See their respective documentations below.)
 *
 * `VariantN` is a discriminated union type parameterized
 * with the largest size of the types stored (`maxDataSize`)
 * and with the list of allowed types (`AllowedTypes`). If
 * the list is empty, then any type up of size up to $(D
 * maxDataSize) (rounded up for alignment) can be stored in a
 * `VariantN` object without being boxed (types larger
 * than this will be boxed).
 *
 */
struct VariantN(size_t maxDataSize, AllowedTypesParam...)
{
    /**
    The list of allowed types. If empty, any type is allowed.
    */
    alias AllowedTypes = This2Variant!(VariantN, AllowedTypesParam);

private:
    // Compute the largest practical size from maxDataSize
    struct SizeChecker
    {
        int function() fptr;
        ubyte[maxDataSize] data;
    }
    enum size = SizeChecker.sizeof - (int function()).sizeof;

    /** Tells whether a type `T` is statically _allowed for
     * storage inside a `VariantN` object by looking
     * `T` up in `AllowedTypes`.
     */
    public template allowed(T)
    {
        enum bool allowed
            = is(T == VariantN)
            ||
            //T.sizeof <= size &&
            (AllowedTypes.length == 0 || staticIndexOf!(T, AllowedTypes) >= 0);
    }

    // Each internal operation is encoded with an identifier. See
    // the "handler" function below.
    enum OpID { getTypeInfo, get, compare, equals, testConversion, toString,
            index, indexAssign, catAssign, copyOut, length,
            apply, postblit, destruct }

    // state
    union
    {
        align(maxVariantAlignment!(AllowedTypes)) ubyte[size] store;
        // conservatively mark the region as pointers
        static if (size >= (void*).sizeof)
            void*[size / (void*).sizeof] p;
    }
    ptrdiff_t function(OpID selector, ubyte[size]* store, void* data) fptr
        = &handler!(void);

    // internals
    // Handler for an uninitialized value
    static ptrdiff_t handler(A : void)(OpID selector, ubyte[size]*, void* parm)
    {
        switch (selector)
        {
        case OpID.getTypeInfo:
            *cast(TypeInfo *) parm = typeid(A);
            break;
        case OpID.copyOut:
            auto target = cast(VariantN *) parm;
            target.fptr = &handler!(A);
            // no need to copy the data (it's garbage)
            break;
        case OpID.compare:
        case OpID.equals:
            auto rhs = cast(const VariantN *) parm;
            return rhs.peek!(A)
                ? 0 // all uninitialized are equal
                : ptrdiff_t.min; // uninitialized variant is not comparable otherwise
        case OpID.toString:
            string * target = cast(string*) parm;
            *target = "<Uninitialized VariantN>";
            break;
        case OpID.postblit:
        case OpID.destruct:
            break;
        case OpID.get:
        case OpID.testConversion:
        case OpID.index:
        case OpID.indexAssign:
        case OpID.catAssign:
        case OpID.length:
            throw new VariantException(
                "Attempt to use an uninitialized VariantN");
        default: assert(false, "Invalid OpID");
        }
        return 0;
    }

    // Handler for all of a type's operations
    static ptrdiff_t handler(A)(OpID selector, ubyte[size]* pStore, void* parm)
    {
        import std.conv : to;
        static A* getPtr(void* untyped)
        {
            if (untyped)
            {
                static if (A.sizeof <= size)
                    return cast(A*) untyped;
                else
                    return *cast(A**) untyped;
            }
            return null;
        }

        static ptrdiff_t compare(A* rhsPA, A* zis, OpID selector)
        {
            static if (is(typeof(*rhsPA == *zis)))
            {
                enum isEmptyStructWithoutOpEquals = is(A == struct) && A.tupleof.length == 0 &&
                                                    !__traits(hasMember, A, "opEquals");
                static if (isEmptyStructWithoutOpEquals)
                {
                    // The check above will always succeed if A is an empty struct.
                    // Don't generate unreachable code as seen in
                    // https://issues.dlang.org/show_bug.cgi?id=21231
                    return 0;
                }
                else
                {
                    if (*rhsPA == *zis)
                        return 0;
                    static if (is(typeof(*zis < *rhsPA)))
                    {
                        // Many types (such as any using the default Object opCmp)
                        // will throw on an invalid opCmp, so do it only
                        // if the caller requests it.
                        if (selector == OpID.compare)
                            return *zis < *rhsPA ? -1 : 1;
                        else
                            return ptrdiff_t.min;
                    }
                    else
                    {
                        // Not equal, and type does not support ordering
                        // comparisons.
                        return ptrdiff_t.min;
                    }
                }
            }
            else
            {
                // Type does not support comparisons at all.
                return ptrdiff_t.min;
            }
        }

        auto zis = getPtr(pStore);
        // Input: TypeInfo object
        // Output: target points to a copy of *me, if me was not null
        // Returns: true iff the A can be converted to the type represented
        // by the incoming TypeInfo
        static bool tryPutting(A* src, TypeInfo targetType, void* target)
        {
            alias UA = Unqual!A;
            static if (isStaticArray!A && is(typeof(UA.init[0])))
            {
                alias MutaTypes = AliasSeq!(UA, typeof(UA.init[0])[], AllImplicitConversionTargets!UA);
            }
            else
            {
                alias MutaTypes = AliasSeq!(UA, AllImplicitConversionTargets!UA);
            }
            alias ConstTypes = staticMap!(ConstOf, MutaTypes);
            alias SharedTypes = staticMap!(SharedOf, MutaTypes);
            alias SharedConstTypes = staticMap!(SharedConstOf, MutaTypes);
            alias ImmuTypes  = staticMap!(ImmutableOf, MutaTypes);

            static if (is(A == immutable))
                alias AllTypes = AliasSeq!(ImmuTypes, ConstTypes, SharedConstTypes);
            else static if (is(A == shared))
            {
                static if (is(A == const))
                    alias AllTypes = SharedConstTypes;
                else
                    alias AllTypes = AliasSeq!(SharedTypes, SharedConstTypes);
            }
            else
            {
                static if (is(A == const))
                    alias AllTypes = ConstTypes;
                else
                    alias AllTypes = AliasSeq!(MutaTypes, ConstTypes);
            }

            foreach (T ; AllTypes)
            {
                if (targetType != typeid(T))
                    continue;

                // SPECIAL NOTE: variant only will ever create a new value with
                // tryPutting (effectively), and T is ALWAYS the same type of
                // A, but with different modifiers (and a limited set of
                // implicit targets). So this checks to see if we can construct
                // a T from A, knowing that prerequisite. This handles issues
                // where the type contains some constant data aside from the
                // modifiers on the type itself.
                static if (is(typeof(delegate T() {return *src;})) ||
                           is(T ==        const(U), U) ||
                           is(T ==       shared(U), U) ||
                           is(T == shared const(U), U) ||
                           is(T ==    immutable(U), U))
                {
                    import core.internal.lifetime : emplaceRef;

                    auto zat = cast(T*) target;
                    if (src)
                    {
                        static if (T.sizeof > 0)
                            assert(target, "target must be non-null");

                        static if (isStaticArray!A && isDynamicArray!T)
                        {
                            auto this_ = (*src)[];
                            emplaceRef(*cast(Unqual!T*) zat, cast(Unqual!T) this_);
                        }
                        else
                        {
                            emplaceRef(*cast(Unqual!T*) zat, *cast(UA*) src);
                        }
                    }
                }
                else
                {
                    // type T is not constructible from A
                    if (src)
                        assert(false, A.stringof);
                }
                return true;
            }
            return false;
        }

        switch (selector)
        {
        case OpID.getTypeInfo:
            *cast(TypeInfo *) parm = typeid(A);
            break;
        case OpID.copyOut:
            auto target = cast(VariantN *) parm;
            assert(target);

            static if (target.size < A.sizeof)
            {
                if (target.type.tsize < A.sizeof)
                {
                    static if (is(A == U[n], U, size_t n))
                    {
                        A* p = cast(A*)(new U[n]).ptr;
                    }
                    else
                    {
                        A* p = new A;
                    }
                    *cast(A**)&target.store = p;
                }
            }
            tryPutting(zis, typeid(A), cast(void*) getPtr(&target.store))
                || assert(false);
            target.fptr = &handler!(A);
            break;
        case OpID.get:
            auto t = * cast(Tuple!(TypeInfo, void*)*) parm;
            return !tryPutting(zis, t[0], t[1]);
        case OpID.testConversion:
            return !tryPutting(null, *cast(TypeInfo*) parm, null);
        case OpID.compare:
        case OpID.equals:
            auto rhsP = cast(VariantN *) parm;
            auto rhsType = rhsP.type;
            // Are we the same?
            if (rhsType == typeid(A))
            {
                // cool! Same type!
                auto rhsPA = getPtr(&rhsP.store);
                return compare(rhsPA, zis, selector);
            }
            else if (rhsType == typeid(void))
            {
                // No support for ordering comparisons with
                // uninitialized vars
                return ptrdiff_t.min;
            }
            VariantN temp;
            // Do I convert to rhs?
            if (tryPutting(zis, rhsType, &temp.store))
            {
                // cool, I do; temp's store contains my data in rhs's type!
                // also fix up its fptr
                temp.fptr = rhsP.fptr;
                // now lhsWithRhsType is a full-blown VariantN of rhs's type
                if (selector == OpID.compare)
                    return temp.opCmp(*rhsP);
                else
                    return temp.opEquals(*rhsP) ? 0 : 1;
            }
            // Does rhs convert to zis?
            auto t = tuple(typeid(A), &temp.store);
            if (rhsP.fptr(OpID.get, &rhsP.store, &t) == 0)
            {
                // cool! Now temp has rhs in my type!
                auto rhsPA = getPtr(&temp.store);
                return compare(rhsPA, zis, selector);
            }
            // Generate the function below only if the Variant's type is
            // comparable with 'null'
            static if (__traits(compiles, () => A.init == null))
            {
                if (rhsType == typeid(null))
                {
                    // if rhsType is typeof(null), then we're comparing with 'null'
                    // this takes into account 'opEquals' and 'opCmp'
                    // all types that can compare with null have to following properties:
                    // if it's 'null' then it's equal to null, otherwise it's always greater
                    // than 'null'
                    return *zis == null ? 0 : 1;
                }
            }
            return ptrdiff_t.min; // dunno
        case OpID.toString:
            auto target = cast(string*) parm;
            static if (is(typeof(to!(string)(*zis))))
            {
                *target = to!(string)(*zis);
                break;
            }
            // TODO: The following test evaluates to true for shared objects.
            //       Use __traits for now until this is sorted out.
            // else static if (is(typeof((*zis).toString)))
            else static if (__traits(compiles, {(*zis).toString();}))
            {
                *target = (*zis).toString();
                break;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(string));
            }

        case OpID.index:
            auto result = cast(Variant*) parm;
            static if (isArray!(A) && !is(immutable typeof(A.init[0]) == immutable void))
            {
                // array type; input and output are the same VariantN
                size_t index = result.convertsTo!(int)
                    ? result.get!(int) : result.get!(size_t);
                *result = (*zis)[index];
                break;
            }
            else static if (isAssociativeArray!(A))
            {
                *result = (*zis)[result.get!(typeof(A.init.keys[0]))];
                break;
            }
            else
            {
                throw new VariantException(typeid(A), result[0].type);
            }

        case OpID.indexAssign:
            // array type; result comes first, index comes second
            auto args = cast(Variant*) parm;
            static if (isArray!(A) && is(typeof((*zis)[0] = (*zis)[0])))
            {
                size_t index = args[1].convertsTo!(int)
                    ? args[1].get!(int) : args[1].get!(size_t);
                (*zis)[index] = args[0].get!(typeof((*zis)[0]));
                break;
            }
            else static if (isAssociativeArray!(A) && is(typeof((*zis)[A.init.keys[0]] = A.init.values[0])))
            {
                (*zis)[args[1].get!(typeof(A.init.keys[0]))]
                    = args[0].get!(typeof(A.init.values[0]));
                break;
            }
            else
            {
                throw new VariantException(typeid(A), args[0].type);
            }

        case OpID.catAssign:
            static if (!is(immutable typeof((*zis)[0]) == immutable void) &&
                    is(typeof((*zis)[0])) && is(typeof(*zis ~= *zis)))
            {
                // array type; parm is the element to append
                auto arg = cast(Variant*) parm;
                alias E = typeof((*zis)[0]);
                if (arg[0].convertsTo!(E))
                {
                    // append one element to the array
                    (*zis) ~= [ arg[0].get!(E) ];
                }
                else
                {
                    // append a whole array to the array
                    (*zis) ~= arg[0].get!(A);
                }
                break;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(void[]));
            }

        case OpID.length:
            static if (isArray!(A) || isAssociativeArray!(A))
            {
                return zis.length;
            }
            else
            {
                throw new VariantException(typeid(A), typeid(void[]));
            }

        case OpID.apply:
            static if (!isFunctionPointer!A && !isDelegate!A)
            {
                import std.conv : text;
                import std.exception : enforce;
                enforce(0, text("Cannot apply `()' to a value of type `",
                                A.stringof, "'."));
            }
            else
            {
                import std.conv : text;
                import std.exception : enforce;
                alias ParamTypes = Parameters!A;
                auto p = cast(Variant*) parm;
                auto argCount = p.get!size_t;
                // To assign the tuple we need to use the unqualified version,
                // otherwise we run into issues such as with const values.
                // We still get the actual type from the Variant though
                // to ensure that we retain const correctness.
                Tuple!(staticMap!(Unqual, ParamTypes)) t;
                enforce(t.length == argCount,
                        text("Argument count mismatch: ",
                             A.stringof, " expects ", t.length,
                             " argument(s), not ", argCount, "."));
                auto variantArgs = p[1 .. argCount + 1];
                foreach (i, T; ParamTypes)
                {
                    t[i] = cast() variantArgs[i].get!T;
                }

                auto args = cast(Tuple!(ParamTypes))t;
                static if (is(ReturnType!A == void))
                {
                    (*zis)(args.expand);
                    *p = Variant.init; // void returns uninitialized Variant.
                }
                else
                {
                    *p = (*zis)(args.expand);
                }
            }
            break;

        case OpID.postblit:
            static if (hasElaborateCopyConstructor!A)
            {
                zis.__xpostblit();
            }
            break;

        case OpID.destruct:
            static if (hasElaborateDestructor!A)
            {
                zis.__xdtor();
            }
            break;

        default: assert(false);
        }
        return 0;
    }

public:
    /** Constructs a `VariantN` value given an argument of a
     * generic type. Statically rejects disallowed types.
     */

    this(T)(T value)
    {
        static assert(allowed!(T), "Cannot store a " ~ T.stringof
            ~ " in a " ~ VariantN.stringof);
        opAssign(value);
    }

    /// Allows assignment from a subset algebraic type
    this(T : VariantN!(tsize, Types), size_t tsize, Types...)(T value)
        if (!is(T : VariantN) && Types.length > 0 && allSatisfy!(allowed, Types))
    {
        opAssign(value);
    }

    static if (!AllowedTypes.length || anySatisfy!(hasElaborateCopyConstructor, AllowedTypes))
    {
        this(this)
        {
            fptr(OpID.postblit, &store, null);
        }
    }

    static if (!AllowedTypes.length || anySatisfy!(hasElaborateDestructor, AllowedTypes))
    {
        ~this()
        {
            // Infer the safety of the provided types
            static if (AllowedTypes.length)
            {
                if (0)
                {
                    AllowedTypes var;
                }
            }
            (() @trusted => fptr(OpID.destruct, &store, null))();
        }
    }

    /** Assigns a `VariantN` from a generic
     * argument. Statically rejects disallowed types. */

    VariantN opAssign(T)(T rhs)
    {
        static assert(allowed!(T), "Cannot store a " ~ T.stringof
            ~ " in a " ~ VariantN.stringof ~ ". Valid types are "
                ~ AllowedTypes.stringof);

        static if (is(T : VariantN))
        {
            rhs.fptr(OpID.copyOut, &rhs.store, &this);
        }
        else static if (is(T : const(VariantN)))
        {
            static assert(false,
                    "Assigning Variant objects from const Variant"~
                    " objects is currently not supported.");
        }
        else
        {
            import core.lifetime : copyEmplace;

            static if (!AllowedTypes.length || anySatisfy!(hasElaborateDestructor, AllowedTypes))
            {
                // Assignment should destruct previous value
                fptr(OpID.destruct, &store, null);
            }

            static if (T.sizeof <= size)
                copyEmplace(rhs, *cast(T*) &store);
            else
            {
                static if (is(T == U[n], U, size_t n))
                    auto p = cast(T*) (new U[n]).ptr;
                else
                    auto p = new T;
                copyEmplace(rhs, *p);
                *(cast(T**) &store) = p;
            }

            fptr = &handler!(T);
        }
        return this;
    }

    // Allow assignment from another variant which is a subset of this one
    VariantN opAssign(T : VariantN!(tsize, Types), size_t tsize, Types...)(T rhs)
        if (!is(T : VariantN) && Types.length > 0 && allSatisfy!(allowed, Types))
    {
        // discover which type rhs is actually storing
        foreach (V; T.AllowedTypes)
            if (rhs.type == typeid(V))
                return this = rhs.get!V;
        assert(0, T.AllowedTypes.stringof);
    }


    Variant opCall(P...)(auto ref P params)
    {
        Variant[P.length + 1] pack;
        pack[0] = P.length;
        foreach (i, _; params)
        {
            pack[i + 1] = params[i];
        }
        fptr(OpID.apply, &store, &pack);
        return pack[0];
    }

    /** Returns true if and only if the `VariantN` object
     * holds a valid value (has been initialized with, or assigned
     * from, a valid value).
     */
    @property bool hasValue() const pure nothrow
    {
        // @@@BUG@@@ in compiler, the cast shouldn't be needed
        return cast(typeof(&handler!(void))) fptr != &handler!(void);
    }

    ///
    version (StdDdoc)
    @system unittest
    {
        Variant a;
        assert(!a.hasValue);
        Variant b;
        a = b;
        assert(!a.hasValue); // still no value
        a = 5;
        assert(a.hasValue);
    }

    /**
     * If the `VariantN` object holds a value of the
     * $(I exact) type `T`, returns a pointer to that
     * value. Otherwise, returns `null`. In cases
     * where `T` is statically disallowed, $(D
     * peek) will not compile.
     */
    @property inout(T)* peek(T)() inout
    {
        static if (!is(T == void))
            static assert(allowed!(T), "Cannot store a " ~ T.stringof
                    ~ " in a " ~ VariantN.stringof);
        if (type != typeid(T))
            return null;
        static if (T.sizeof <= size)
            return cast(inout T*)&store;
        else
            return *cast(inout T**)&store;
    }

    ///
    version (StdDdoc)
    @system unittest
    {
        Variant a = 5;
        auto b = a.peek!(int);
        assert(b !is null);
        *b = 6;
        assert(a == 6);
    }

    /**
     * Returns the `typeid` of the currently held value.
     */

    @property TypeInfo type() const nothrow @trusted
    {
        scope(failure) assert(0);

        TypeInfo result;
        fptr(OpID.getTypeInfo, null, &result);
        return result;
    }

    /**
     * Returns `true` if and only if the `VariantN`
     * object holds an object implicitly convertible to type `T`.
     * Implicit convertibility is defined as per
     * $(REF_ALTTEXT AllImplicitConversionTargets, AllImplicitConversionTargets, std,traits).
     */

    @property bool convertsTo(T)() const
    {
        TypeInfo info = typeid(T);
        return fptr(OpID.testConversion, null, &info) == 0;
    }

    /**
    Returns the value stored in the `VariantN` object, either by specifying the
    needed type or the index in the list of allowed types. The latter overload
    only applies to bounded variants (e.g. $(LREF Algebraic)).

    Params:
    T = The requested type. The currently stored value must implicitly convert
    to the requested type, in fact `DecayStaticToDynamicArray!T`. If an
    implicit conversion is not possible, throws a `VariantException`.
    index = The index of the type among `AllowedTypesParam`, zero-based.
     */
    @property inout(T) get(T)() inout
    {
        inout(T) result = void;
        static if (is(T == shared))
            alias R = shared Unqual!T;
        else
            alias R = Unqual!T;
        auto buf = tuple(typeid(T), cast(R*)&result);

        if (fptr(OpID.get, cast(ubyte[size]*) &store, &buf))
        {
            throw new VariantException(type, typeid(T));
        }
        return result;
    }

    /// Ditto
    @property auto get(uint index)() inout
    if (index < AllowedTypes.length)
    {
        foreach (i, T; AllowedTypes)
        {
            static if (index == i) return get!T;
        }
        assert(0);
    }

    /**
     * Returns the value stored in the `VariantN` object,
     * explicitly converted (coerced) to the requested type $(D
     * T). If `T` is a string type, the value is formatted as
     * a string. If the `VariantN` object is a string, a
     * parse of the string to type `T` is attempted. If a
     * conversion is not possible, throws a $(D
     * VariantException).
     */

    @property T coerce(T)()
    {
        import std.conv : to, text;
        static if (isNumeric!T || isBoolean!T)
        {
            if (convertsTo!real)
            {
                // maybe optimize this fella; handle ints separately
                return to!T(get!real);
            }
            else if (convertsTo!(const(char)[]))
            {
                return to!T(get!(const(char)[]));
            }
            // I'm not sure why this doesn't convert to const(char),
            // but apparently it doesn't (probably a deeper bug).
            //
            // Until that is fixed, this quick addition keeps a common
            // function working. "10".coerce!int ought to work.
            else if (convertsTo!(immutable(char)[]))
            {
                return to!T(get!(immutable(char)[]));
            }
            else
            {
                import std.exception : enforce;
                enforce(false, text("Type ", type, " does not convert to ",
                                typeid(T)));
                assert(0);
            }
        }
        else static if (is(T : Object))
        {
            return to!(T)(get!(Object));
        }
        else static if (isSomeString!(T))
        {
            return to!(T)(toString());
        }
        else
        {
            // Fix for bug 1649
            static assert(false, "unsupported type for coercion");
        }
    }

    /**
     * Formats the stored value as a string.
     */

    string toString()
    {
        string result;
        fptr(OpID.toString, &store, &result) == 0 || assert(false);
        return result;
    }

    /**
     * Comparison for equality used by the "==" and "!="  operators.
     */

    // returns 1 if the two are equal
    bool opEquals(T)(auto ref T rhs) const
    if (allowed!T || is(immutable T == immutable VariantN))
    {
        static if (is(immutable T == immutable VariantN))
            alias temp = rhs;
        else
            auto temp = VariantN(rhs);
        return !fptr(OpID.equals, cast(ubyte[size]*) &store,
                     cast(void*) &temp);
    }

    // workaround for bug 10567 fix
    int opCmp(ref const VariantN rhs) const
    {
        return (cast() this).opCmp!(VariantN)(cast() rhs);
    }

    /**
     * Ordering comparison used by the "<", "<=", ">", and ">="
     * operators. In case comparison is not sensible between the held
     * value and `rhs`, an exception is thrown.
     */

    int opCmp(T)(T rhs)
    if (allowed!T)  // includes T == VariantN
    {
        static if (is(T == VariantN))
            alias temp = rhs;
        else
            auto temp = VariantN(rhs);
        auto result = fptr(OpID.compare, &store, &temp);
        if (result == ptrdiff_t.min)
        {
            throw new VariantException(type, temp.type);
        }

        assert(result >= -1 && result <= 1);  // Should be true for opCmp.
        return cast(int) result;
    }

    /**
     * Computes the hash of the held value.
     */

    size_t toHash() const nothrow @safe
    {
        return type.getHash(&store);
    }

    private VariantN opArithmetic(T, string op)(T other)
    {
        static if (isInstanceOf!(.VariantN, T))
        {
            string tryUseType(string tp)
            {
                import std.format : format;
                return q{
                    static if (allowed!%1$s && T.allowed!%1$s)
                        if (convertsTo!%1$s && other.convertsTo!%1$s)
                            return VariantN(get!%1$s %2$s other.get!%1$s);
                }.format(tp, op);
            }

            mixin(tryUseType("uint"));
            mixin(tryUseType("int"));
            mixin(tryUseType("ulong"));
            mixin(tryUseType("long"));
            mixin(tryUseType("float"));
            mixin(tryUseType("double"));
            mixin(tryUseType("real"));
        }
        else
        {
            static if (allowed!T)
                if (auto pv = peek!T) return VariantN(mixin("*pv " ~ op ~ " other"));
            static if (allowed!uint && is(typeof(T.max) : uint) && isUnsigned!T)
                if (convertsTo!uint) return VariantN(mixin("get!(uint) " ~ op ~ " other"));
            static if (allowed!int && is(typeof(T.max) : int) && !isUnsigned!T)
                if (convertsTo!int) return VariantN(mixin("get!(int) " ~ op ~ " other"));
            static if (allowed!ulong && is(typeof(T.max) : ulong) && isUnsigned!T)
                if (convertsTo!ulong) return VariantN(mixin("get!(ulong) " ~ op ~ " other"));
            static if (allowed!long && is(typeof(T.max) : long) && !isUnsigned!T)
                if (convertsTo!long) return VariantN(mixin("get!(long) " ~ op ~ " other"));
            static if (allowed!float && is(T : float))
                if (convertsTo!float) return VariantN(mixin("get!(float) " ~ op ~ " other"));
            static if (allowed!double && is(T : double))
                if (convertsTo!double) return VariantN(mixin("get!(double) " ~ op ~ " other"));
            static if (allowed!real && is (T : real))
                if (convertsTo!real) return VariantN(mixin("get!(real) " ~ op ~ " other"));
        }

        throw new VariantException("No possible match found for VariantN "~op~" "~T.stringof);
    }

    private VariantN opLogic(T, string op)(T other)
    {
        VariantN result;
        static if (is(T == VariantN))
        {
            if (convertsTo!(uint) && other.convertsTo!(uint))
                result = mixin("get!(uint) " ~ op ~ " other.get!(uint)");
            else if (convertsTo!(int) && other.convertsTo!(int))
                result = mixin("get!(int) " ~ op ~ " other.get!(int)");
            else if (convertsTo!(ulong) && other.convertsTo!(ulong))
                result = mixin("get!(ulong) " ~ op ~ " other.get!(ulong)");
            else
                result = mixin("get!(long) " ~ op ~ " other.get!(long)");
        }
        else
        {
            if (is(typeof(T.max) : uint) && T.min == 0 && convertsTo!(uint))
                result = mixin("get!(uint) " ~ op ~ " other");
            else if (is(typeof(T.max) : int) && T.min < 0 && convertsTo!(int))
                result = mixin("get!(int) " ~ op ~ " other");
            else if (is(typeof(T.max) : ulong) && T.min == 0
                     && convertsTo!(ulong))
                result = mixin("get!(ulong) " ~ op ~ " other");
            else
                result = mixin("get!(long) " ~ op ~ " other");
        }
        return result;
    }

    /**
     * Arithmetic between `VariantN` objects and numeric
     * values. All arithmetic operations return a `VariantN`
     * object typed depending on the types of both values
     * involved. The conversion rules mimic D's built-in rules for
     * arithmetic conversions.
     */
    VariantN opBinary(string op, T)(T rhs)
    if ((op == "+" || op == "-" || op == "*" || op == "/" || op == "^^" || op == "%") &&
        is(typeof(opArithmetic!(T, op)(rhs))))
    { return opArithmetic!(T, op)(rhs); }
    ///ditto
    VariantN opBinary(string op, T)(T rhs)
    if ((op == "&" || op == "|" || op == "^" || op == ">>" || op == "<<" || op == ">>>") &&
        is(typeof(opLogic!(T, op)(rhs))))
    { return opLogic!(T, op)(rhs); }
    ///ditto
    VariantN opBinaryRight(string op, T)(T lhs)
    if ((op == "+" || op == "*") &&
        is(typeof(opArithmetic!(T, op)(lhs))))
    { return opArithmetic!(T, op)(lhs); }
    ///ditto
    VariantN opBinaryRight(string op, T)(T lhs)
    if ((op == "&" || op == "|" || op == "^") &&
        is(typeof(opLogic!(T, op)(lhs))))
    { return opLogic!(T, op)(lhs); }
    ///ditto
    VariantN opBinary(string op, T)(T rhs)
        if (op == "~")
    {
        auto temp = this;
        temp ~= rhs;
        return temp;
    }
    // ///ditto
    // VariantN opBinaryRight(string op, T)(T rhs)
    //     if (op == "~")
    // {
    //     VariantN temp = rhs;
    //     temp ~= this;
    //     return temp;
    // }

    ///ditto
    VariantN opOpAssign(string op, T)(T rhs)
    {
        static if (op != "~")
        {
            mixin("return this = this" ~ op ~ "rhs;");
        }
        else
        {
            auto toAppend = Variant(rhs);
            fptr(OpID.catAssign, &store, &toAppend) == 0 || assert(false);
            return this;
        }
    }

    /**
     * Array and associative array operations. If a $(D
     * VariantN) contains an (associative) array, it can be indexed
     * into. Otherwise, an exception is thrown.
     */
    inout(Variant) opIndex(K)(K i) inout
    {
        auto result = Variant(i);
        fptr(OpID.index, cast(ubyte[size]*) &store, &result) == 0 || assert(false);
        return result;
    }

    ///
    version (StdDdoc)
    @system unittest
    {
        Variant a = new int[10];
        a[5] = 42;
        assert(a[5] == 42);
        a[5] += 8;
        assert(a[5] == 50);

        int[int] hash = [ 42:24 ];
        a = hash;
        assert(a[42] == 24);
        a[42] /= 2;
        assert(a[42] == 12);
    }

    /// ditto
    Variant opIndexAssign(T, N)(T value, N i)
    {
        static if (AllowedTypes.length && !isInstanceOf!(.VariantN, T))
        {
            enum canAssign(U) = __traits(compiles, (U u){ u[i] = value; });
            static assert(anySatisfy!(canAssign, AllowedTypes),
                "Cannot assign " ~ T.stringof ~ " to " ~ VariantN.stringof ~
                " indexed with " ~ N.stringof);
        }
        Variant[2] args = [ Variant(value), Variant(i) ];
        fptr(OpID.indexAssign, &store, &args) == 0 || assert(false);
        return args[0];
    }

    /// ditto
    Variant opIndexOpAssign(string op, T, N)(T value, N i)
    {
        return opIndexAssign(mixin(`opIndex(i)` ~ op ~ `value`), i);
    }

    /** If the `VariantN` contains an (associative) array,
     * returns the _length of that array. Otherwise, throws an
     * exception.
     */
    @property size_t length()
    {
        return cast(size_t) fptr(OpID.length, &store, null);
    }

    /**
       If the `VariantN` contains an array, applies `dg` to each
       element of the array in turn. Otherwise, throws an exception.
     */
    int opApply(Delegate)(scope Delegate dg) if (is(Delegate == delegate))
    {
        alias A = Parameters!(Delegate)[0];
        if (type == typeid(A[]))
        {
            auto arr = get!(A[]);
            foreach (ref e; arr)
            {
                if (dg(e)) return 1;
            }
        }
        else static if (is(A == VariantN))
        {
            foreach (i; 0 .. length)
            {
                // @@@TODO@@@: find a better way to not confuse
                // clients who think they change values stored in the
                // Variant when in fact they are only changing tmp.
                auto tmp = this[i];
                debug scope(exit) assert(tmp == this[i]);
                if (dg(tmp)) return 1;
            }
        }
        else
        {
            import std.conv : text;
            import std.exception : enforce;
            enforce(false, text("Variant type ", type,
                            " not iterable with values of type ",
                            A.stringof));
        }
        return 0;
    }
}

///
@system unittest
{
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

/// can also assign arrays
@system unittest
{
    alias Var = VariantN!(maxSize!(int[]));

    Var a = new int[42];
    assert(a.length == 42);
    a[5] = 7;
    assert(a[5] == 7);
}

@safe unittest
{
    alias V = VariantN!24;
    const alignMask = V.alignof - 1;
    assert(V.sizeof == ((24 + (void*).sizeof + alignMask) & ~alignMask));
}

/// Can also assign class values
@system unittest
{
    alias Var = VariantN!(maxSize!(int*)); // classes are pointers
    Var a;

    class Foo {}
    auto foo = new Foo;
    a = foo;
    assert(*a.peek!(Foo) == foo); // and full type information is preserved
}

@system unittest
{
    import std.conv : to;
    Variant v;
    int foo() { return 42; }
    v = &foo;
    assert(v() == 42);

    static int bar(string s) { return to!int(s); }
    v = &bar;
    assert(v("43") == 43);
}

@system unittest
{
    int[int] hash = [ 42:24 ];
    Variant v = hash;
    assert(v[42] == 24);
    v[42] = 5;
    assert(v[42] == 5);
}

// opIndex with static arrays, https://issues.dlang.org/show_bug.cgi?id=12771
@system unittest
{
    int[4] elements = [0, 1, 2, 3];
    Variant v = elements;
    assert(v == elements);
    assert(v[2] == 2);
    assert(v[3] == 3);
    v[2] = 6;
    assert(v[2] == 6);
    assert(v != elements);
}

@system unittest
{
    import std.exception : assertThrown;
    Algebraic!(int[]) v = [2, 2];

    assert(v == [2, 2]);
    v[0] = 1;
    assert(v[0] == 1);
    assert(v != [2, 2]);

    // opIndexAssign from Variant
    v[1] = v[0];
    assert(v[1] == 1);

    static assert(!__traits(compiles, (v[1] = null)));
    assertThrown!VariantException(v[1] = Variant(null));
}

// https://issues.dlang.org/show_bug.cgi?id=10879
@system unittest
{
    int[10] arr = [1,2,3,4,5,6,7,8,9,10];
    Variant v1 = arr;
    Variant v2;
    v2 = arr;
    assert(v1 == arr);
    assert(v2 == arr);
    foreach (i, e; arr)
    {
        assert(v1[i] == e);
        assert(v2[i] == e);
    }
    static struct LargeStruct
    {
        int[100] data;
    }
    LargeStruct ls;
    ls.data[] = 4;
    v1 = ls;
    Variant v3 = ls;
    assert(v1 == ls);
    assert(v3 == ls);
}

// https://issues.dlang.org/show_bug.cgi?id=8195
@system unittest
{
    struct S
    {
        int a;
        long b;
        string c;
        real d = 0.0;
        bool e;
    }

    static assert(S.sizeof >= Variant.sizeof);
    alias Types = AliasSeq!(string, int, S);
    alias MyVariant = VariantN!(maxSize!Types, Types);

    auto v = MyVariant(S.init);
    assert(v == S.init);
}

// https://issues.dlang.org/show_bug.cgi?id=10961
@system unittest
{
    // Primarily test that we can assign a void[] to a Variant.
    void[] elements = cast(void[])[1, 2, 3];
    Variant v = elements;
    void[] returned = v.get!(void[]);
    assert(returned == elements);
}

// https://issues.dlang.org/show_bug.cgi?id=13352
@system unittest
{
    alias TP = Algebraic!(long);
    auto a = TP(1L);
    auto b = TP(2L);
    assert(!TP.allowed!ulong);
    assert(a + b == 3L);
    assert(a + 2 == 3L);
    assert(1 + b == 3L);

    alias TP2 = Algebraic!(long, string);
    auto c = TP2(3L);
    assert(a + c == 4L);
}

// https://issues.dlang.org/show_bug.cgi?id=13354
@system unittest
{
    alias A = Algebraic!(string[]);
    A a = ["a", "b"];
    assert(a[0] == "a");
    assert(a[1] == "b");
    a[1] = "c";
    assert(a[1] == "c");

    alias AA = Algebraic!(int[string]);
    AA aa = ["a": 1, "b": 2];
    assert(aa["a"] == 1);
    assert(aa["b"] == 2);
    aa["b"] = 3;
    assert(aa["b"] == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=14198
@system unittest
{
    Variant a = true;
    assert(a.type == typeid(bool));
}

// https://issues.dlang.org/show_bug.cgi?id=14233
@system unittest
{
    alias Atom = Algebraic!(string, This[]);

    Atom[] values = [];
    auto a = Atom(values);
}

pure nothrow @nogc
@system unittest
{
    Algebraic!(int, double) a;
    a = 100;
    a = 1.0;
}

// https://issues.dlang.org/show_bug.cgi?id=14457
@system unittest
{
    alias A = Algebraic!(int, float, double);
    alias B = Algebraic!(int, float);

    A a = 1;
    B b = 6f;
    a = b;

    assert(a.type == typeid(float));
    assert(a.get!float == 6f);
}

// https://issues.dlang.org/show_bug.cgi?id=14585
@system unittest
{
    static struct S
    {
        int x = 42;
        ~this() {assert(x == 42);}
    }
    Variant(S()).get!S;
}

// https://issues.dlang.org/show_bug.cgi?id=14586
@system unittest
{
    const Variant v = new immutable Object;
    v.get!(immutable Object);
}

@system unittest
{
    static struct S
    {
        T opCast(T)() {assert(false);}
    }
    Variant v = S();
    v.get!S;
}

// https://issues.dlang.org/show_bug.cgi?id=13262
@system unittest
{
    static void fun(T)(Variant v){
        T x;
        v = x;
        auto r = v.get!(T);
    }
    Variant v;
    fun!(shared(int))(v);
    fun!(shared(int)[])(v);

    static struct S1
    {
        int c;
        string a;
    }

    static struct S2
    {
        string a;
        shared int[] b;
    }

    static struct S3
    {
        string a;
        shared int[] b;
        int c;
    }

    fun!(S1)(v);
    fun!(shared(S1))(v);
    fun!(S2)(v);
    fun!(shared(S2))(v);
    fun!(S3)(v);
    fun!(shared(S3))(v);

    // ensure structs that are shared, but don't have shared postblits
    // can't be used.
    static struct S4
    {
        int x;
        this(this) {x = 0;}
    }

    fun!(S4)(v);
    static assert(!is(typeof(fun!(shared(S4))(v))));
}

@safe unittest
{
    Algebraic!(int) x;

    static struct SafeS
    {
        @safe ~this() {}
    }

    Algebraic!(SafeS) y;
}

// https://issues.dlang.org/show_bug.cgi?id=19986
@system unittest
{
    VariantN!32 v;
    v = const(ubyte[33]).init;

    struct S
    {
        ubyte[33] s;
    }

    VariantN!32 v2;
    v2 = const(S).init;
}

// https://issues.dlang.org/show_bug.cgi?id=21021
@system unittest
{
    static struct S
    {
        int h;
        int[5] array;
        alias h this;
    }

    S msg;
    msg.array[] = 3;
    Variant a = msg;
    auto other = a.get!S;
    assert(msg.array[0] == 3);
    assert(other.array[0] == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=21231
// Compatibility with -preview=fieldwise
@system unittest
{
    static struct Empty
    {
        bool opCmp(const scope ref Empty) const
        { return false; }
    }

    Empty a, b;
    assert(a == b);
    assert(!(a < b));

    VariantN!(4, Empty) v = a;
    assert(v == b);
    assert(!(v < b));
}

// Compatibility with -preview=fieldwise
@system unittest
{
    static struct Empty
    {
        bool opEquals(const scope ref Empty) const
        { return false; }
    }

    Empty a, b;
    assert(a != b);

    VariantN!(4, Empty) v = a;
    assert(v != b);
}

// https://issues.dlang.org/show_bug.cgi?id=22647
// Can compare with 'null'
@system unittest
{
    static struct Bar
    {
        int* ptr;
        alias ptr this;
    }

    static class Foo {}
    int* iptr;
    int[] arr;

    Variant v = Foo.init; // 'null'
    assert(v != null); // can only compare objects with 'null' by using 'is'

    v = iptr;
    assert(v == null); // pointers can be compared with 'null'

    v = arr;
    assert(v == null); // arrays can be compared with 'null'

    v = "";
    assert(v == null); // strings are arrays, an empty string is considered 'null'

    v = Bar.init;
    assert(v == null); // works with alias this

    v = [3];
    assert(v != null);
    assert(v > null);
    assert(v >= null);
    assert(!(v < null));
}

/**
_Algebraic data type restricted to a closed set of possible
types. It's an alias for $(LREF VariantN) with an
appropriately-constructed maximum size. `Algebraic` is
useful when it is desirable to restrict what a discriminated type
could hold to the end of defining simpler and more efficient
manipulation.

$(RED Warning: $(LREF Algebraic) is outdated and not recommended for use in new
code. Instead, use $(REF SumType, std,sumtype).)
*/
template Algebraic(T...)
{
    alias Algebraic = VariantN!(maxSize!T, T);
}

///
@system unittest
{
    auto v = Algebraic!(int, double, string)(5);
    assert(v.peek!(int));
    v = 3.14;
    assert(v.peek!(double));
    // auto x = v.peek!(long); // won't compile, type long not allowed
    // v = '1'; // won't compile, type char not allowed
}

/**
$(H4 Self-Referential Types)

A useful and popular use of algebraic data structures is for defining $(LUCKY
self-referential data structures), i.e. structures that embed references to
values of their own type within.

This is achieved with `Algebraic` by using `This` as a placeholder whenever a
reference to the type being defined is needed. The `Algebraic` instantiation
will perform $(LINK2 https://en.wikipedia.org/wiki/Name_resolution_(programming_languages)#Alpha_renaming_to_make_name_resolution_trivial,
alpha renaming) on its constituent types, replacing `This`
with the self-referenced type. The structure of the type involving `This` may
be arbitrarily complex.
*/
@system unittest
{
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

private struct FakeComplexReal
{
    real re, im;
}

/**
Alias for $(LREF VariantN) instantiated with the largest size of `creal`,
`char[]`, and `void delegate()`. This ensures that `Variant` is large enough
to hold all of D's predefined types unboxed, including all numeric types,
pointers, delegates, and class references.  You may want to use
`VariantN` directly with a different maximum size either for
storing larger types unboxed, or for saving memory.
 */
alias Variant = VariantN!(maxSize!(FakeComplexReal, char[], void delegate()));

///
@system unittest
{
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

/// can also assign arrays
@system unittest
{
    Variant a = new int[42];
    assert(a.length == 42);
    a[5] = 7;
    assert(a[5] == 7);
}

/// Can also assign class values
@system unittest
{
    Variant a;

    class Foo {}
    auto foo = new Foo;
    a = foo;
    assert(*a.peek!(Foo) == foo); // and full type information is preserved
}

/**
 * Returns an array of variants constructed from `args`.
 *
 * This is by design. During construction the `Variant` needs
 * static type information about the type being held, so as to store a
 * pointer to function for fast retrieval.
 */
Variant[] variantArray(T...)(T args)
{
    Variant[] result;
    foreach (arg; args)
    {
        result ~= Variant(arg);
    }
    return result;
}

///
@system unittest
{
    auto a = variantArray(1, 3.14, "Hi!");
    assert(a[1] == 3.14);
    auto b = Variant(a); // variant array as variant
    assert(b[1] == 3.14);
}

/**
 * Thrown in three cases:
 *
 * $(OL $(LI An uninitialized `Variant` is used in any way except
 * assignment and `hasValue`;) $(LI A `get` or
 * `coerce` is attempted with an incompatible target type;)
 * $(LI A comparison between `Variant` objects of
 * incompatible types is attempted.))
 *
 */

// @@@ BUG IN COMPILER. THE 'STATIC' BELOW SHOULD NOT COMPILE
static class VariantException : Exception
{
    /// The source type in the conversion or comparison
    TypeInfo source;
    /// The target type in the conversion or comparison
    TypeInfo target;
    this(string s)
    {
        super(s);
    }
    this(TypeInfo source, TypeInfo target)
    {
        super("Variant: attempting to use incompatible types "
                            ~ source.toString()
                            ~ " and " ~ target.toString());
        this.source = source;
        this.target = target;
    }
}

///
@system unittest
{
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
    alias W1 = This2Variant!(char, int, This[int]);
    alias W2 = AliasSeq!(int, char[int]);
    static assert(is(W1 == W2));

    alias var_t = Algebraic!(void, string);
    var_t foo = "quux";
}

@system unittest
{
     alias A = Algebraic!(real, This[], This[int], This[This]);
     A v1, v2, v3;
     v2 = 5.0L;
     v3 = 42.0L;
     //v1 = [ v2 ][];
      auto v = v1.peek!(A[]);
     //writeln(v[0]);
     v1 = [ 9 : v3 ];
     //writeln(v1);
     v1 = [ v3 : v3 ];
     //writeln(v1);
}

@system unittest
{
    import std.conv : ConvException;
    import std.exception : assertThrown, collectException;
    // try it with an oddly small size
    VariantN!(1) test;
    assert(test.size > 1);

    // variantArray tests
    auto heterogeneous = variantArray(1, 4.5, "hi");
    assert(heterogeneous.length == 3);
    auto variantArrayAsVariant = Variant(heterogeneous);
    assert(variantArrayAsVariant[0] == 1);
    assert(variantArrayAsVariant.length == 3);

    // array tests
    auto arr = Variant([1.2].dup);
    auto e = arr[0];
    assert(e == 1.2);
    arr[0] = 2.0;
    assert(arr[0] == 2);
    arr ~= 4.5;
    assert(arr[1] == 4.5);

    // general tests
    Variant a;
    auto b = Variant(5);
    assert(!b.peek!(real) && b.peek!(int));
    // assign
    a = *b.peek!(int);
    // comparison
    assert(a == b, a.type.toString() ~ " " ~ b.type.toString());
    auto c = Variant("this is a string");
    assert(a != c);
    // comparison via implicit conversions
    a = 42; b = 42.0; assert(a == b);

    // try failing conversions
    bool failed = false;
    try
    {
        auto d = c.get!(int);
    }
    catch (Exception e)
    {
        //writeln(stderr, e.toString);
        failed = true;
    }
    assert(failed); // :o)

    // toString tests
    a = Variant(42); assert(a.toString() == "42");
    a = Variant(42.22); assert(a.toString() == "42.22");

    // coerce tests
    a = Variant(42.22); assert(a.coerce!(int) == 42);
    a = cast(short) 5; assert(a.coerce!(double) == 5);
    a = Variant("10"); assert(a.coerce!int == 10);

    a = Variant(1);
    assert(a.coerce!bool);
    a = Variant(0);
    assert(!a.coerce!bool);

    a = Variant(1.0);
    assert(a.coerce!bool);
    a = Variant(0.0);
    assert(!a.coerce!bool);
    a = Variant(float.init);
    assertThrown!ConvException(a.coerce!bool);

    a = Variant("true");
    assert(a.coerce!bool);
    a = Variant("false");
    assert(!a.coerce!bool);
    a = Variant("");
    assertThrown!ConvException(a.coerce!bool);

    // Object tests
    class B1 {}
    class B2 : B1 {}
    a = new B2;
    assert(a.coerce!(B1) !is null);
    a = new B1;
    assert(collectException(a.coerce!(B2) is null));
    a = cast(Object) new B2; // lose static type info; should still work
    assert(a.coerce!(B2) !is null);

//     struct Big { int a[45]; }
//     a = Big.init;

    // hash
    assert(a.toHash() != 0);
}

// tests adapted from
// http://www.dsource.org/projects/tango/browser/trunk/tango/core/Variant.d?rev=2601
@system unittest
{
    Variant v;

    assert(!v.hasValue);
    v = 42;
    assert( v.peek!(int) );
    assert( v.convertsTo!(long) );
    assert( v.get!(int) == 42 );
    assert( v.get!(long) == 42L );
    assert( v.get!(ulong) == 42uL );

    v = "Hello, World!";
    assert( v.peek!(string) );

    assert( v.get!(string) == "Hello, World!" );
    assert(!is(char[] : wchar[]));
    assert( !v.convertsTo!(wchar[]) );
    assert( v.get!(string) == "Hello, World!" );

    // Literal arrays are dynamically-typed
    v = cast(int[4]) [1,2,3,4];
    assert( v.peek!(int[4]) );
    assert( v.get!(int[4]) == [1,2,3,4] );

    {
         v = [1,2,3,4,5];
         assert( v.peek!(int[]) );
         assert( v.get!(int[]) == [1,2,3,4,5] );
    }

    v = 3.1413;
    assert( v.peek!(double) );
    assert( v.convertsTo!(real) );
    //@@@ BUG IN COMPILER: DOUBLE SHOULD NOT IMPLICITLY CONVERT TO FLOAT
    assert( v.convertsTo!(float) );
    assert( *v.peek!(double) == 3.1413 );

    auto u = Variant(v);
    assert( u.peek!(double) );
    assert( *u.peek!(double) == 3.1413 );

    // operators
    v = 38;
    assert( v + 4 == 42 );
    assert( 4 + v == 42 );
    assert( v - 4 == 34 );
    assert( Variant(4) - v == -34 );
    assert( v * 2 == 76 );
    assert( 2 * v == 76 );
    assert( v / 2 == 19 );
    assert( Variant(2) / v == 0 );
    assert( v % 2 == 0 );
    assert( Variant(2) % v == 2 );
    assert( (v & 6) == 6 );
    assert( (6 & v) == 6 );
    assert( (v | 9) == 47 );
    assert( (9 | v) == 47 );
    assert( (v ^ 5) == 35 );
    assert( (5 ^ v) == 35 );
    assert( v << 1 == 76 );
    assert( Variant(1) << Variant(2) == 4 );
    assert( v >> 1 == 19 );
    assert( Variant(4) >> Variant(2) == 1 );
    assert( Variant("abc") ~ "def" == "abcdef" );
    assert( Variant("abc") ~ Variant("def") == "abcdef" );

    v = 38;
    v += 4;
    assert( v == 42 );
    v = 38; v -= 4; assert( v == 34 );
    v = 38; v *= 2; assert( v == 76 );
    v = 38; v /= 2; assert( v == 19 );
    v = 38; v %= 2; assert( v == 0 );
    v = 38; v &= 6; assert( v == 6 );
    v = 38; v |= 9; assert( v == 47 );
    v = 38; v ^= 5; assert( v == 35 );
    v = 38; v <<= 1; assert( v == 76 );
    v = 38; v >>= 1; assert( v == 19 );
    v = 38; v += 1;  assert( v < 40 );

    v = "abc";
    v ~= "def";
    assert( v == "abcdef", *v.peek!(char[]) );
    assert( Variant(0) < Variant(42) );
    assert( Variant(42) > Variant(0) );
    assert( Variant(42) > Variant(0.1) );
    assert( Variant(42.1) > Variant(1) );
    assert( Variant(21) == Variant(21) );
    assert( Variant(0) != Variant(42) );
    assert( Variant("bar") == Variant("bar") );
    assert( Variant("foo") != Variant("bar") );

    {
        auto v1 = Variant(42);
        auto v2 = Variant("foo");

        int[Variant] hash;
        hash[v1] = 0;
        hash[v2] = 1;

        assert( hash[v1] == 0 );
        assert( hash[v2] == 1 );
    }

    {
        int[char[]] hash;
        hash["a"] = 1;
        hash["b"] = 2;
        hash["c"] = 3;
        Variant vhash = hash;

        assert( vhash.get!(int[char[]])["a"] == 1 );
        assert( vhash.get!(int[char[]])["b"] == 2 );
        assert( vhash.get!(int[char[]])["c"] == 3 );
    }
}

@system unittest
{
    // check comparisons incompatible with AllowedTypes
    Algebraic!int v = 2;

    assert(v == 2);
    assert(v < 3);
    static assert(!__traits(compiles, () => v == long.max));
    static assert(!__traits(compiles, () => v == null));
    static assert(!__traits(compiles, () => v < long.max));
    static assert(!__traits(compiles, () => v > null));
}

// https://issues.dlang.org/show_bug.cgi?id=1558
@system unittest
{
    Variant va=1;
    Variant vb=-2;
    assert((va+vb).get!(int) == -1);
    assert((va-vb).get!(int) == 3);
}

@system unittest
{
    Variant a;
    a=5;
    Variant b;
    b=a;
    Variant[] c;
    c = variantArray(1, 2, 3.0, "hello", 4);
    assert(c[3] == "hello");
}

@system unittest
{
    Variant v = 5;
    assert(!__traits(compiles, v.coerce!(bool delegate())));
}


@system unittest
{
    struct Huge {
        real a, b, c, d, e, f, g;
    }

    Huge huge;
    huge.e = 42;
    Variant v;
    v = huge;  // Compile time error.
    assert(v.get!(Huge).e == 42);
}

@system unittest
{
    const x = Variant(42);
    auto y1 = x.get!(const int);
    // @@@BUG@@@
    //auto y2 = x.get!(immutable int)();
}

// test iteration
@system unittest
{
    auto v = Variant([ 1, 2, 3, 4 ][]);
    auto j = 0;
    foreach (int i; v)
    {
        assert(i == ++j);
    }
    assert(j == 4);
}

// test convertibility
@system unittest
{
    auto v = Variant("abc".dup);
    assert(v.convertsTo!(char[]));
}

// https://issues.dlang.org/show_bug.cgi?id=5424
@system unittest
{
    interface A {
        void func1();
    }
    static class AC: A {
        void func1() {
        }
    }

    A a = new AC();
    a.func1();
    Variant b = Variant(a);
}

// https://issues.dlang.org/show_bug.cgi?id=7070
@system unittest
{
    Variant v;
    v = null;
}

// Class and interface opEquals, https://issues.dlang.org/show_bug.cgi?id=12157
@system unittest
{
    class Foo { }

    class DerivedFoo : Foo { }

    Foo f1 = new Foo();
    Foo f2 = new DerivedFoo();

    Variant v1 = f1, v2 = f2;
    assert(v1 == f1);
    assert(v1 != new Foo());
    assert(v1 != f2);
    assert(v2 != v1);
    assert(v2 == f2);
}

// Const parameters with opCall, https://issues.dlang.org/show_bug.cgi?id=11361
@system unittest
{
    static string t1(string c) {
        return c ~ "a";
    }

    static const(char)[] t2(const(char)[] p) {
        return p ~ "b";
    }

    static char[] t3(int p) {
        import std.conv : text;
        return p.text.dup;
    }

    Variant v1 = &t1;
    Variant v2 = &t2;
    Variant v3 = &t3;

    assert(v1("abc") == "abca");
    assert(v1("abc").type == typeid(string));
    assert(v2("abc") == "abcb");

    assert(v2(cast(char[])("abc".dup)) == "abcb");
    assert(v2("abc").type == typeid(const(char)[]));

    assert(v3(4) == ['4']);
    assert(v3(4).type == typeid(char[]));
}

// https://issues.dlang.org/show_bug.cgi?id=12071
@system unittest
{
    static struct Structure { int data; }
    alias VariantTest = Algebraic!(Structure delegate() pure nothrow @nogc @safe);

    bool called = false;
    Structure example() pure nothrow @nogc @safe
    {
        called = true;
        return Structure.init;
    }
    auto m = VariantTest(&example);
    m();
    assert(called);
}

// Ordering comparisons of incompatible types
// e.g. https://issues.dlang.org/show_bug.cgi?id=7990
@system unittest
{
    import std.exception : assertThrown;
    assertThrown!VariantException(Variant(3) < "a");
    assertThrown!VariantException("a" < Variant(3));
    assertThrown!VariantException(Variant(3) < Variant("a"));

    assertThrown!VariantException(Variant.init < Variant(3));
    assertThrown!VariantException(Variant(3) < Variant.init);
}

// Handling of unordered types
// https://issues.dlang.org/show_bug.cgi?id=9043
@system unittest
{
    import std.exception : assertThrown;
    static struct A { int a; }

    assert(Variant(A(3)) != A(4));

    assertThrown!VariantException(Variant(A(3)) < A(4));
    assertThrown!VariantException(A(3) < Variant(A(4)));
    assertThrown!VariantException(Variant(A(3)) < Variant(A(4)));
}

// Handling of empty types and arrays
// https://issues.dlang.org/show_bug.cgi?id=10958
@system unittest
{
    class EmptyClass { }
    struct EmptyStruct { }
    alias EmptyArray = void[0];
    alias Alg = Algebraic!(EmptyClass, EmptyStruct, EmptyArray);

    Variant testEmpty(T)()
    {
        T inst;
        Variant v = inst;
        assert(v.get!T == inst);
        assert(v.peek!T !is null);
        assert(*v.peek!T == inst);
        Alg alg = inst;
        assert(alg.get!T == inst);
        return v;
    }

    testEmpty!EmptyClass();
    testEmpty!EmptyStruct();
    testEmpty!EmptyArray();

    // EmptyClass/EmptyStruct sizeof is 1, so we have this to test just size 0.
    EmptyArray arr = EmptyArray.init;
    Algebraic!(EmptyArray) a = arr;
    assert(a.length == 0);
    assert(a.get!EmptyArray == arr);
}

// Handling of void function pointers / delegates
// https://issues.dlang.org/show_bug.cgi?id=11360
@system unittest
{
    static void t1() { }
    Variant v = &t1;
    assert(v() == Variant.init);

    static int t2() { return 3; }
    Variant v2 = &t2;
    assert(v2() == 3);
}

// Using peek for large structs
// https://issues.dlang.org/show_bug.cgi?id=8580
@system unittest
{
    struct TestStruct(bool pad)
    {
        int val1;
        static if (pad)
            ubyte[Variant.size] padding;
        int val2;
    }

    void testPeekWith(T)()
    {
        T inst;
        inst.val1 = 3;
        inst.val2 = 4;
        Variant v = inst;
        T* original = v.peek!T;
        assert(original.val1 == 3);
        assert(original.val2 == 4);
        original.val1 = 6;
        original.val2 = 8;
        T modified = v.get!T;
        assert(modified.val1 == 6);
        assert(modified.val2 == 8);
    }

    testPeekWith!(TestStruct!false)();
    testPeekWith!(TestStruct!true)();
}

// https://issues.dlang.org/show_bug.cgi?id=18780
@system unittest
{
    int x = 7;
    Variant a = x;
    assert(a.convertsTo!ulong);
    assert(a.convertsTo!uint);
}

/**
 * Applies a delegate or function to the given $(LREF Algebraic) depending on the held type,
 * ensuring that all types are handled by the visiting functions.
 *
 * The delegate or function having the currently held value as parameter is called
 * with `variant`'s current value. Visiting handlers are passed
 * in the template parameter list.
 * It is statically ensured that all held types of
 * `variant` are handled across all handlers.
 * `visit` allows delegates and static functions to be passed
 * as parameters.
 *
 * If a function with an untyped parameter is specified, this function is called
 * when the variant contains a type that does not match any other function.
 * This can be used to apply the same function across multiple possible types.
 * Exactly one generic function is allowed.
 *
 * If a function without parameters is specified, this function is called
 * when `variant` doesn't hold a value. Exactly one parameter-less function
 * is allowed.
 *
 * Duplicate overloads matching the same type in one of the visitors are disallowed.
 *
 * Returns: The return type of visit is deduced from the visiting functions and must be
 * the same across all overloads.
 * Throws: $(LREF VariantException) if `variant` doesn't hold a value and no
 * parameter-less fallback function is specified.
 */
template visit(Handlers...)
if (Handlers.length > 0)
{
    ///
    auto visit(VariantType)(VariantType variant)
        if (isAlgebraic!VariantType)
    {
        return visitImpl!(true, VariantType, Handlers)(variant);
    }
}

///
@system unittest
{
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
    Algebraic!(size_t, string) variant;

    // not all handled check
    static assert(!__traits(compiles, variant.visit!((size_t i){ })() ));

    variant = cast(size_t) 10;
    auto which = 0;
    variant.visit!( (string s) => which = 1,
                    (size_t i) => which = 0
                    )();

    // integer overload was called
    assert(which == 0);

    // mustn't compile as generic Variant not supported
    Variant v;
    static assert(!__traits(compiles, v.visit!((string s) => which = 1,
                                               (size_t i) => which = 0
                                                )()
                                                ));

    static size_t func(string s) {
        return s.length;
    }

    variant = "test";
    assert( 4 == variant.visit!(func,
                                (size_t i) => i
                                )());

    Algebraic!(int, float, string) variant2 = 5.0f;
    // Shouldn' t compile as float not handled by visitor.
    static assert(!__traits(compiles, variant2.visit!(
                        (int _) {},
                        (string _) {})()));

    Algebraic!(size_t, string, float) variant3;
    variant3 = 10.0f;
    auto floatVisited = false;

    assert(variant3.visit!(
                 (float f) { floatVisited = true; return cast(size_t) f; },
                 func,
                 (size_t i) { return i; }
                 )() == 10);
    assert(floatVisited == true);

    Algebraic!(float, string) variant4;

    assert(variant4.visit!(func, (float f) => cast(size_t) f, () => size_t.max)() == size_t.max);

    // double error func check
    static assert(!__traits(compiles,
                            visit!(() => size_t.max, func, (float f) => cast(size_t) f, () => size_t.max)(variant4))
                 );
}

// disallow providing multiple generic handlers to visit
// disallow a generic handler that does not apply to all types
@system unittest
{
    Algebraic!(int, float) number = 2;
    // ok, x + 1 valid for int and float
    static assert( __traits(compiles, number.visit!(x => x + 1)));
    // bad, two generic handlers
    static assert(!__traits(compiles, number.visit!(x => x + 1, x => x + 2)));
    // bad, x ~ "a" does not apply to int or float
    static assert(!__traits(compiles, number.visit!(x => x ~ "a")));
    // bad, x ~ "a" does not apply to int or float
    static assert(!__traits(compiles, number.visit!(x => x + 1, x => x ~ "a")));

    Algebraic!(int, string) maybenumber = 2;
    // ok, x ~ "a" valid for string, x + 1 valid for int, only 1 generic
    static assert( __traits(compiles, maybenumber.visit!((string x) => x ~ "a", x => "foobar"[0 .. x + 1])));
    // bad, x ~ "a" valid for string but not int
    static assert(!__traits(compiles, maybenumber.visit!(x => x ~ "a")));
    // bad, two generics, each only applies in one case
    static assert(!__traits(compiles, maybenumber.visit!(x => x + 1, x => x ~ "a")));
}

/**
 * Behaves as $(LREF visit) but doesn't enforce that all types are handled
 * by the visiting functions.
 *
 * If a parameter-less function is specified it is called when
 * either `variant` doesn't hold a value or holds a type
 * which isn't handled by the visiting functions.
 *
 * Returns: The return type of tryVisit is deduced from the visiting functions and must be
 * the same across all overloads.
 * Throws: $(LREF VariantException) if `variant` doesn't hold a value or
 * `variant` holds a value which isn't handled by the visiting functions,
 * when no parameter-less fallback function is specified.
 */
template tryVisit(Handlers...)
if (Handlers.length > 0)
{
    ///
    auto tryVisit(VariantType)(VariantType variant)
        if (isAlgebraic!VariantType)
    {
        return visitImpl!(false, VariantType, Handlers)(variant);
    }
}

///
@system unittest
{
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

@system unittest
{
    import std.exception : assertThrown;
    Algebraic!(int, string) variant;

    variant = 10;
    auto which = -1;
    variant.tryVisit!((int i){ which = 0; })();

    assert(which == 0);

    variant = "test";

    assertThrown!VariantException(variant.tryVisit!((int i) { which = 0; })());

    void errorfunc()
    {
        which = -1;
    }

    variant.tryVisit!((int i) { which = 0; }, errorfunc)();

    assert(which == -1);
}

private template isAlgebraic(Type)
{
    static if (is(Type _ == VariantN!T, T...))
        enum isAlgebraic = T.length >= 2; // T[0] == maxDataSize, T[1..$] == AllowedTypesParam
    else
        enum isAlgebraic = false;
}

@system unittest
{
    static assert(!isAlgebraic!(Variant));
    static assert( isAlgebraic!(Algebraic!(string)));
    static assert( isAlgebraic!(Algebraic!(int, int[])));
}

private auto visitImpl(bool Strict, VariantType, Handler...)(VariantType variant)
if (isAlgebraic!VariantType && Handler.length > 0)
{
    alias AllowedTypes = VariantType.AllowedTypes;


    /**
     * Returns: Struct where `indices`  is an array which
     * contains at the n-th position the index in Handler which takes the
     * n-th type of AllowedTypes. If an Handler doesn't match an
     * AllowedType, -1 is set. If a function in the delegates doesn't
     * have parameters, the field `exceptionFuncIdx` is set;
     * otherwise it's -1.
     */
    auto visitGetOverloadMap()
    {
        struct Result {
            int[AllowedTypes.length] indices;
            int exceptionFuncIdx = -1;
            int generalFuncIdx = -1;
        }

        Result result;

        enum int nonmatch = ()
        {
            foreach (int dgidx, dg; Handler)
            {
                bool found = false;
                foreach (T; AllowedTypes)
                {
                    found |= __traits(compiles, { static assert(isSomeFunction!(dg!T)); });
                    found |= __traits(compiles, (T t) { dg(t); });
                    found |= __traits(compiles, dg());
                }
                if (!found) return dgidx;
            }
            return -1;
        }();
        static assert(nonmatch == -1, "No match for visit handler #"~
            nonmatch.stringof~" ("~Handler[nonmatch].stringof~")");

        foreach (tidx, T; AllowedTypes)
        {
            bool added = false;
            foreach (dgidx, dg; Handler)
            {
                // Handle normal function objects
                static if (isSomeFunction!dg)
                {
                    alias Params = Parameters!dg;
                    static if (Params.length == 0)
                    {
                        // Just check exception functions in the first
                        // inner iteration (over delegates)
                        if (tidx > 0)
                            continue;
                        else
                        {
                            if (result.exceptionFuncIdx != -1)
                                assert(false, "duplicate parameter-less (error-)function specified");
                            result.exceptionFuncIdx = dgidx;
                        }
                    }
                    else static if (is(Params[0] == T) || is(Unqual!(Params[0]) == T))
                    {
                        if (added)
                            assert(false, "duplicate overload specified for type '" ~ T.stringof ~ "'");

                        added = true;
                        result.indices[tidx] = dgidx;
                    }
                }
                else static if (__traits(compiles, { static assert(isSomeFunction!(dg!T)); }))
                {
                    assert(result.generalFuncIdx == -1 ||
                           result.generalFuncIdx == dgidx,
                           "Only one generic visitor function is allowed");
                    result.generalFuncIdx = dgidx;
                }
                // Handle composite visitors with opCall overloads
            }

            if (!added)
                result.indices[tidx] = -1;
        }

        return result;
    }

    enum HandlerOverloadMap = visitGetOverloadMap();

    if (!variant.hasValue)
    {
        // Call the exception function. The HandlerOverloadMap
        // will have its exceptionFuncIdx field set to value != -1 if an
        // exception function has been specified; otherwise we just through an exception.
        static if (HandlerOverloadMap.exceptionFuncIdx != -1)
            return Handler[ HandlerOverloadMap.exceptionFuncIdx ]();
        else
            throw new VariantException("variant must hold a value before being visited.");
    }

    foreach (idx, T; AllowedTypes)
    {
        if (auto ptr = variant.peek!T)
        {
            enum dgIdx = HandlerOverloadMap.indices[idx];

            static if (dgIdx == -1)
            {
                static if (HandlerOverloadMap.generalFuncIdx >= 0)
                    return Handler[HandlerOverloadMap.generalFuncIdx](*ptr);
                else static if (Strict)
                    static assert(false, "overload for type '" ~ T.stringof ~ "' hasn't been specified");
                else static if (HandlerOverloadMap.exceptionFuncIdx != -1)
                    return Handler[HandlerOverloadMap.exceptionFuncIdx]();
                else
                    throw new VariantException(
                        "variant holds value of type '"
                        ~ T.stringof ~
                        "' but no visitor has been provided"
                    );
            }
            else
            {
                return Handler[ dgIdx ](*ptr);
            }
        }
    }

    assert(false);
}

// https://issues.dlang.org/show_bug.cgi?id=21253
@system unittest
{
    static struct A { int n; }
    static struct B {        }

    auto a = Algebraic!(A, B)(B());
    assert(a.visit!(
        (B _) => 42,
        (a  ) => a.n
    ) == 42);
}

@system unittest
{
    // validate that visit can be called with a const type
    struct Foo { int depth; }
    struct Bar { int depth; }
    alias FooBar = Algebraic!(Foo, Bar);

    int depth(in FooBar fb) {
        return fb.visit!((Foo foo) => foo.depth,
                         (Bar bar) => bar.depth);
    }

    FooBar fb = Foo(3);
    assert(depth(fb) == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=16383
@system unittest
{
    class Foo {this() immutable {}}
    alias V = Algebraic!(immutable Foo);

    auto x = V(new immutable Foo).visit!(
        (immutable(Foo) _) => 3
    );
    assert(x == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=5310
@system unittest
{
    const Variant a;
    assert(a == a);
    Variant b;
    assert(a == b);
    assert(b == a);
}

@system unittest
{
    const Variant a = [2];
    assert(a[0] == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=10017
@system unittest
{
    static struct S
    {
        ubyte[Variant.size + 1] s;
    }

    Variant v1, v2;
    v1 = S(); // the payload is allocated on the heap
    v2 = v1;  // AssertError: target must be non-null
    assert(v1 == v2);
}

// https://issues.dlang.org/show_bug.cgi?id=7069
@system unittest
{
    import std.exception : assertThrown;
    Variant v;

    int i = 10;
    v = i;
    static foreach (qual; AliasSeq!(Alias, ConstOf))
    {
        assert(v.get!(qual!int) == 10);
        assert(v.get!(qual!float) == 10.0f);
    }
    static foreach (qual; AliasSeq!(ImmutableOf, SharedOf, SharedConstOf))
    {
        assertThrown!VariantException(v.get!(qual!int));
    }

    const(int) ci = 20;
    v = ci;
    static foreach (qual; AliasSeq!(ConstOf))
    {
        assert(v.get!(qual!int) == 20);
        assert(v.get!(qual!float) == 20.0f);
    }
    static foreach (qual; AliasSeq!(Alias, ImmutableOf, SharedOf, SharedConstOf))
    {
        assertThrown!VariantException(v.get!(qual!int));
        assertThrown!VariantException(v.get!(qual!float));
    }

    immutable(int) ii = ci;
    v = ii;
    static foreach (qual; AliasSeq!(ImmutableOf, ConstOf, SharedConstOf))
    {
        assert(v.get!(qual!int) == 20);
        assert(v.get!(qual!float) == 20.0f);
    }
    static foreach (qual; AliasSeq!(Alias, SharedOf))
    {
        assertThrown!VariantException(v.get!(qual!int));
        assertThrown!VariantException(v.get!(qual!float));
    }

    int[] ai = [1,2,3];
    v = ai;
    static foreach (qual; AliasSeq!(Alias, ConstOf))
    {
        assert(v.get!(qual!(int[])) == [1,2,3]);
        assert(v.get!(qual!(int)[]) == [1,2,3]);
    }
    static foreach (qual; AliasSeq!(ImmutableOf, SharedOf, SharedConstOf))
    {
        assertThrown!VariantException(v.get!(qual!(int[])));
        assertThrown!VariantException(v.get!(qual!(int)[]));
    }

    const(int[]) cai = [4,5,6];
    v = cai;
    static foreach (qual; AliasSeq!(ConstOf))
    {
        assert(v.get!(qual!(int[])) == [4,5,6]);
        assert(v.get!(qual!(int)[]) == [4,5,6]);
    }
    static foreach (qual; AliasSeq!(Alias, ImmutableOf, SharedOf, SharedConstOf))
    {
        assertThrown!VariantException(v.get!(qual!(int[])));
        assertThrown!VariantException(v.get!(qual!(int)[]));
    }

    immutable(int[]) iai = [7,8,9];
    v = iai;
    //assert(v.get!(immutable(int[])) == [7,8,9]);   // Bug ??? runtime error
    assert(v.get!(immutable(int)[]) == [7,8,9]);
    assert(v.get!(const(int[])) == [7,8,9]);
    assert(v.get!(const(int)[]) == [7,8,9]);
    //assert(v.get!(shared(const(int[]))) == cast(shared const)[7,8,9]);    // Bug ??? runtime error
    //assert(v.get!(shared(const(int))[]) == cast(shared const)[7,8,9]);    // Bug ??? runtime error
    static foreach (qual; AliasSeq!(Alias))
    {
        assertThrown!VariantException(v.get!(qual!(int[])));
        assertThrown!VariantException(v.get!(qual!(int)[]));
    }

    class A {}
    class B : A {}
    B b = new B();
    v = b;
    static foreach (qual; AliasSeq!(Alias, ConstOf))
    {
        assert(v.get!(qual!B) is b);
        assert(v.get!(qual!A) is b);
        assert(v.get!(qual!Object) is b);
    }
    static foreach (qual; AliasSeq!(ImmutableOf, SharedOf, SharedConstOf))
    {
        assertThrown!VariantException(v.get!(qual!B));
        assertThrown!VariantException(v.get!(qual!A));
        assertThrown!VariantException(v.get!(qual!Object));
    }

    const(B) cb = new B();
    v = cb;
    static foreach (qual; AliasSeq!(ConstOf))
    {
        assert(v.get!(qual!B) is cb);
        assert(v.get!(qual!A) is cb);
        assert(v.get!(qual!Object) is cb);
    }
    static foreach (qual; AliasSeq!(Alias, ImmutableOf, SharedOf, SharedConstOf))
    {
        assertThrown!VariantException(v.get!(qual!B));
        assertThrown!VariantException(v.get!(qual!A));
        assertThrown!VariantException(v.get!(qual!Object));
    }

    immutable(B) ib = new immutable(B)();
    v = ib;
    static foreach (qual; AliasSeq!(ImmutableOf, ConstOf, SharedConstOf))
    {
        assert(v.get!(qual!B) is ib);
        assert(v.get!(qual!A) is ib);
        assert(v.get!(qual!Object) is ib);
    }
    static foreach (qual; AliasSeq!(Alias, SharedOf))
    {
        assertThrown!VariantException(v.get!(qual!B));
        assertThrown!VariantException(v.get!(qual!A));
        assertThrown!VariantException(v.get!(qual!Object));
    }

    shared(B) sb = new shared B();
    v = sb;
    static foreach (qual; AliasSeq!(SharedOf, SharedConstOf))
    {
        assert(v.get!(qual!B) is sb);
        assert(v.get!(qual!A) is sb);
        assert(v.get!(qual!Object) is sb);
    }
    static foreach (qual; AliasSeq!(Alias, ImmutableOf, ConstOf))
    {
        assertThrown!VariantException(v.get!(qual!B));
        assertThrown!VariantException(v.get!(qual!A));
        assertThrown!VariantException(v.get!(qual!Object));
    }

    shared(const(B)) scb = new shared const B();
    v = scb;
    static foreach (qual; AliasSeq!(SharedConstOf))
    {
        assert(v.get!(qual!B) is scb);
        assert(v.get!(qual!A) is scb);
        assert(v.get!(qual!Object) is scb);
    }
    static foreach (qual; AliasSeq!(Alias, ConstOf, ImmutableOf, SharedOf))
    {
        assertThrown!VariantException(v.get!(qual!B));
        assertThrown!VariantException(v.get!(qual!A));
        assertThrown!VariantException(v.get!(qual!Object));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=12540
@system unittest
{
    static struct DummyScope
    {
        alias Alias12540 = Algebraic!Class12540;

        static class Class12540
        {
            Alias12540 entity;
        }
    }
}

@system unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=10194
    // Also test for elaborate copying
    static struct S
    {
        @disable this();
        this(int dummy)
        {
            ++cnt;
        }

        this(this)
        {
            ++cnt;
        }

        @disable S opAssign();

        ~this()
        {
            --cnt;
            assert(cnt >= 0);
        }
        static int cnt = 0;
    }

    {
        Variant v;
        {
            v = S(0);
            assert(S.cnt == 1);
        }
        assert(S.cnt == 1);

        // assigning a new value should destroy the existing one
        v = 0;
        assert(S.cnt == 0);

        // destroying the variant should destroy it's current value
        v = S(0);
        assert(S.cnt == 1);
    }
    assert(S.cnt == 0);
}

@system unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=13300
    static struct S
    {
        this(this) {}
        ~this() {}
    }

    static assert( hasElaborateCopyConstructor!(Variant));
    static assert(!hasElaborateCopyConstructor!(Algebraic!bool));
    static assert( hasElaborateCopyConstructor!(Algebraic!S));
    static assert( hasElaborateCopyConstructor!(Algebraic!(bool, S)));

    static assert( hasElaborateDestructor!(Variant));
    static assert(!hasElaborateDestructor!(Algebraic!bool));
    static assert( hasElaborateDestructor!(Algebraic!S));
    static assert( hasElaborateDestructor!(Algebraic!(bool, S)));

    import std.array;
    alias Value = Algebraic!bool;

    static struct T
    {
        Value value;
        @disable this();
    }
    auto a = appender!(T[]);
}

// https://issues.dlang.org/show_bug.cgi?id=13871
@system unittest
{
    alias A = Algebraic!(int, typeof(null));
    static struct B { A value; }
    alias C = std.variant.Algebraic!B;

    C var;
    var = C(B());
}

@system unittest
{
    import std.exception : assertThrown, assertNotThrown;
    // Make sure Variant can handle types with opDispatch but no length field.
    struct SWithNoLength
    {
        void opDispatch(string s)() { }
    }

    struct SWithLength
    {
        @property int opDispatch(string s)()
        {
            // Assume that s == "length"
            return 5; // Any value is OK for test.
        }
    }

    SWithNoLength sWithNoLength;
    Variant v = sWithNoLength;
    assertThrown!VariantException(v.length);

    SWithLength sWithLength;
    v = sWithLength;
    assertNotThrown!VariantException(v.get!SWithLength.length);
    assertThrown!VariantException(v.length);
}

// https://issues.dlang.org/show_bug.cgi?id=13534
@system unittest
{
    static assert(!__traits(compiles, () @safe {
        auto foo() @system { return 3; }
        auto v = Variant(&foo);
        v(); // foo is called in safe code!?
    }));
}

// https://issues.dlang.org/show_bug.cgi?id=15039
@system unittest
{
    import std.typecons;
    import std.variant;

    alias IntTypedef = Typedef!int;
    alias Obj = Algebraic!(int, IntTypedef, This[]);

    Obj obj = 1;

    obj.visit!(
        (int x) {},
        (IntTypedef x) {},
        (Obj[] x) {},
    );
}

// https://issues.dlang.org/show_bug.cgi?id=15791
@system unittest
{
    int n = 3;
    struct NS1 { int foo() { return n + 10; } }
    struct NS2 { int foo() { return n * 10; } }

    Variant v;
    v = NS1();
    assert(v.get!NS1.foo() == 13);
    v = NS2();
    assert(v.get!NS2.foo() == 30);
}

// https://issues.dlang.org/show_bug.cgi?id=15827
@system unittest
{
    static struct Foo15827 { Variant v; this(Foo15827 v) {} }
    Variant v = Foo15827.init;
}

// https://issues.dlang.org/show_bug.cgi?id=18934
@system unittest
{
    static struct S
    {
        const int x;
    }

    auto s = S(42);
    Variant v = s;
    auto s2 = v.get!S;
    assert(s2.x == 42);
    Variant v2 = v; // support copying from one variant to the other
    v2 = S(2);
    v = v2;
    assert(v.get!S.x == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=19200
@system unittest
{
    static struct S
    {
        static int opBinaryRight(string op : "|", T)(T rhs)
        {
            return 3;
        }
    }

    S s;
    Variant v;
    auto b = v | s;
    assert(b == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=11061
@system unittest
{
    int[4] el = [0, 1, 2, 3];
    int[3] nl = [0, 1, 2];
    Variant v1 = el;
    assert(v1 == el); // Compare Var(static) to static
    assert(v1 != nl); // Compare static arrays of different length
    assert(v1 == [0, 1, 2, 3]); // Compare Var(static) to dynamic.
    assert(v1 != [0, 1, 2]);
    int[] dyn = [0, 1, 2, 3];
    v1 = dyn;
    assert(v1 == el); // Compare Var(dynamic) to static.
    assert(v1 == [0, 1] ~ [2, 3]); // Compare Var(dynamic) to dynamic
}

// https://issues.dlang.org/show_bug.cgi?id=15940
@system unittest
{
    class C { }
    struct S
    {
        C a;
        alias a this;
    }
    S s = S(new C());
    auto v = Variant(s); // compile error
}

@system unittest
{
    // Test if we don't have scoping issues.
    Variant createVariant(int[] input)
    {
        int[2] el = [input[0], input[1]];
        Variant v = el;
        return v;
    }
    Variant v = createVariant([0, 1]);
    createVariant([2, 3]);
    assert(v == [0,1]);
}

// https://issues.dlang.org/show_bug.cgi?id=19994
@safe unittest
{
    alias Inner = Algebraic!(This*);
    alias Outer = Algebraic!(Inner, This*);

    static assert(is(Outer.AllowedTypes == AliasSeq!(Inner, Outer*)));
}

// https://issues.dlang.org/show_bug.cgi?id=21296
@system unittest
{
    immutable aa = ["0": 0];
    auto v = Variant(aa); // compile error
}

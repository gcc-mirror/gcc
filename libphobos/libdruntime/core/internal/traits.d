/**
 * Contains traits for runtime internal usage.
 *
 * Copyright: Copyright Digital Mars 2014 -.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 * Source: $(DRUNTIMESRC core/internal/_traits.d)
 */
module core.internal.traits;

alias AliasSeq(TList...) = TList;

template Fields(T)
{
    static if (is(T == struct) || is(T == union))
        alias Fields = typeof(T.tupleof[0 .. $ - __traits(isNested, T)]);
    else static if (is(T == class))
        alias Fields = typeof(T.tupleof);
    else
        alias Fields = AliasSeq!T;
}

T trustedCast(T, U)(auto ref U u) @trusted pure nothrow
{
    return cast(T)u;
}

template Unconst(T)
{
         static if (is(T U ==   immutable U)) alias Unconst = U;
    else static if (is(T U == inout const U)) alias Unconst = U;
    else static if (is(T U == inout       U)) alias Unconst = U;
    else static if (is(T U ==       const U)) alias Unconst = U;
    else                                      alias Unconst = T;
}

/// taken from std.traits.Unqual
template Unqual(T)
{
    version (none) // Error: recursive alias declaration @@@BUG1308@@@
    {
             static if (is(T U ==     const U)) alias Unqual = Unqual!U;
        else static if (is(T U == immutable U)) alias Unqual = Unqual!U;
        else static if (is(T U ==     inout U)) alias Unqual = Unqual!U;
        else static if (is(T U ==    shared U)) alias Unqual = Unqual!U;
        else                                    alias Unqual =        T;
    }
    else // workaround
    {
             static if (is(T U ==          immutable U)) alias Unqual = U;
        else static if (is(T U == shared inout const U)) alias Unqual = U;
        else static if (is(T U == shared inout       U)) alias Unqual = U;
        else static if (is(T U == shared       const U)) alias Unqual = U;
        else static if (is(T U == shared             U)) alias Unqual = U;
        else static if (is(T U ==        inout const U)) alias Unqual = U;
        else static if (is(T U ==        inout       U)) alias Unqual = U;
        else static if (is(T U ==              const U)) alias Unqual = U;
        else                                             alias Unqual = T;
    }
}

// Substitute all `inout` qualifiers that appears in T to `const`
template substInout(T)
{
    static if (is(T == immutable))
    {
        alias substInout = T;
    }
    else static if (is(T : shared const U, U) || is(T : const U, U))
    {
        // U is top-unqualified
        mixin("alias substInout = "
            ~ (is(T == shared) ? "shared " : "")
            ~ (is(T == const) || is(T == inout) ? "const " : "")    // substitute inout to const
            ~ "substInoutForm!U;");
    }
    else
        static assert(0);
}

private template substInoutForm(T)
{
    static if (is(T == struct) || is(T == class) || is(T == union) || is(T == interface))
    {
        alias substInoutForm = T;   // prevent matching to the form of alias-this-ed type
    }
    else static if (is(T : V[K], K, V))        alias substInoutForm = substInout!V[substInout!K];
    else static if (is(T : U[n], U, size_t n)) alias substInoutForm = substInout!U[n];
    else static if (is(T : U[], U))            alias substInoutForm = substInout!U[];
    else static if (is(T : U*, U))             alias substInoutForm = substInout!U*;
    else                                       alias substInoutForm = T;
}

/// used to declare an extern(D) function that is defined in a different module
template externDFunc(string fqn, T:FT*, FT) if (is(FT == function))
{
    static if (is(FT RT == return) && is(FT Args == function))
    {
        import core.demangle : mangleFunc;
        enum decl = {
            string s = "extern(D) RT externDFunc(Args)";
            foreach (attr; __traits(getFunctionAttributes, FT))
                s ~= " " ~ attr;
            return s ~ ";";
        }();
        pragma(mangle, mangleFunc!T(fqn)) mixin(decl);
    }
    else
        static assert(0);
}

template staticIota(int beg, int end)
{
    static if (beg + 1 >= end)
    {
        static if (beg >= end)
        {
            alias staticIota = AliasSeq!();
        }
        else
        {
            alias staticIota = AliasSeq!(+beg);
        }
    }
    else
    {
        enum mid = beg + (end - beg) / 2;
        alias staticIota = AliasSeq!(staticIota!(beg, mid), staticIota!(mid, end));
    }
}

template dtorIsNothrow(T)
{
    enum dtorIsNothrow = is(typeof(function{T t=void;}) : void function() nothrow);
}

/*
Tests whether all given items satisfy a template predicate, i.e. evaluates to
$(D F!(T[0]) && F!(T[1]) && ... && F!(T[$ - 1])).
*/
package(core.internal)
template allSatisfy(alias F, T...)
{
    static if (T.length == 0)
    {
        enum allSatisfy = true;
    }
    else static if (T.length == 1)
    {
        enum allSatisfy = F!(T[0]);
    }
    else
    {
        static if (allSatisfy!(F, T[0  .. $/2]))
            enum allSatisfy = allSatisfy!(F, T[$/2 .. $]);
        else
            enum allSatisfy = false;
    }
}

template anySatisfy(alias F, T...)
{
    static if (T.length == 0)
    {
        enum anySatisfy = false;
    }
    else static if (T.length == 1)
    {
        enum anySatisfy = F!(T[0]);
    }
    else
    {
        enum anySatisfy =
            anySatisfy!(F, T[ 0  .. $/2]) ||
            anySatisfy!(F, T[$/2 ..  $ ]);
    }
}

// simplified from std.traits.maxAlignment
template maxAlignment(U...)
{
    static if (U.length == 0)
        static assert(0);
    else static if (U.length == 1)
        enum maxAlignment = U[0].alignof;
    else static if (U.length == 2)
        enum maxAlignment = U[0].alignof > U[1].alignof ? U[0].alignof : U[1].alignof;
    else
    {
        enum a = maxAlignment!(U[0 .. ($+1)/2]);
        enum b = maxAlignment!(U[($+1)/2 .. $]);
        enum maxAlignment = a > b ? a : b;
    }
}

template classInstanceAlignment(T)
if (is(T == class))
{
    alias classInstanceAlignment = maxAlignment!(void*, typeof(T.tupleof));
}

// Somehow fails for non-static nested structs without support for aliases
template hasElaborateDestructor(T...)
{
    static if (is(T[0]))
        alias S = T[0];
    else
        alias S = typeof(T[0]);

    static if (is(S : E[n], E, size_t n) && S.length)
    {
        enum bool hasElaborateDestructor = hasElaborateDestructor!E;
    }
    else static if (is(S == struct))
    {
        enum hasElaborateDestructor = __traits(hasMember, S, "__dtor")
            || anySatisfy!(.hasElaborateDestructor, S.tupleof);
    }
    else
        enum bool hasElaborateDestructor = false;
}

// Somehow fails for non-static nested structs without support for aliases
template hasElaborateCopyConstructor(T...)
{
    static if (is(T[0]))
        alias S = T[0];
    else
        alias S = typeof(T[0]);

    static if (is(S : E[n], E, size_t n) && S.length)
    {
        enum bool hasElaborateCopyConstructor = hasElaborateCopyConstructor!E;
    }
    else static if (is(S == struct))
    {
        enum hasElaborateCopyConstructor = __traits(hasMember, S, "__postblit")
            || anySatisfy!(.hasElaborateCopyConstructor, S.tupleof);
    }
    else
        enum bool hasElaborateCopyConstructor = false;
}

template hasUnsharedIndirections(T)
{
    static if (is(T == immutable))
        enum hasUnsharedIndirections = false;
    else static if (is(T == struct) || is(T == union))
        enum hasUnsharedIndirections = anySatisfy!(.hasUnsharedIndirections, Fields!T);
    else static if (is(T : E[N], E, size_t N))
        enum hasUnsharedIndirections = is(E == void) ? false : hasUnsharedIndirections!E;
    else static if (isFunctionPointer!T)
        enum hasUnsharedIndirections = false;
    else static if (isPointer!T)
        enum hasUnsharedIndirections = !is(T : shared(U)*, U) && !is(T : immutable(U)*, U);
    else static if (isDynamicArray!T)
        enum hasUnsharedIndirections = !is(T : shared(V)[], V) && !is(T : immutable(V)[], V);
    else static if (is(T == class) || is(T == interface))
        enum hasUnsharedIndirections = !is(T : shared(W), W);
    else
        enum hasUnsharedIndirections = isDelegate!T || __traits(isAssociativeArray, T); // TODO: how to handle these?
}

unittest
{
    static struct Foo { shared(int)* val; }

    static assert(!hasUnsharedIndirections!(immutable(char)*));
    static assert(!hasUnsharedIndirections!(string));

    static assert(!hasUnsharedIndirections!(Foo));
    static assert( hasUnsharedIndirections!(Foo*));
    static assert(!hasUnsharedIndirections!(shared(Foo)*));
    static assert(!hasUnsharedIndirections!(immutable(Foo)*));
}

enum bool isAggregateType(T) = is(T == struct) || is(T == union) ||
                               is(T == class) || is(T == interface);

enum bool isPointer(T) = is(T == U*, U) && !isAggregateType!T;

enum bool isDynamicArray(T) = is(DynamicArrayTypeOf!T) && !isAggregateType!T;

template OriginalType(T)
{
    template Impl(T)
    {
        static if (is(T U == enum)) alias Impl = OriginalType!U;
        else                        alias Impl =              T;
    }

    alias OriginalType = ModifyTypePreservingTQ!(Impl, T);
}

template DynamicArrayTypeOf(T)
{
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = DynamicArrayTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(Unqual!X : E[], E) && !is(typeof({ enum n = X.length; })))
        alias DynamicArrayTypeOf = X;
    else
        static assert(0, T.stringof ~ " is not a dynamic array");
}

private template AliasThisTypeOf(T)
    if (isAggregateType!T)
{
    alias members = __traits(getAliasThis, T);

    static if (members.length == 1)
        alias AliasThisTypeOf = typeof(__traits(getMember, T.init, members[0]));
    else
        static assert(0, T.stringof~" does not have alias this type");
}

template isFunctionPointer(T...)
    if (T.length == 1)
{
    static if (is(T[0] U) || is(typeof(T[0]) U))
    {
        static if (is(U F : F*) && is(F == function))
            enum bool isFunctionPointer = true;
        else
            enum bool isFunctionPointer = false;
    }
    else
        enum bool isFunctionPointer = false;
}

template isDelegate(T...)
    if (T.length == 1)
{
    static if (is(typeof(& T[0]) U : U*) && is(typeof(& T[0]) U == delegate))
    {
        // T is a (nested) function symbol.
        enum bool isDelegate = true;
    }
    else static if (is(T[0] W) || is(typeof(T[0]) W))
    {
        // T is an expression or a type.  Take the type of it and examine.
        enum bool isDelegate = is(W == delegate);
    }
    else
        enum bool isDelegate = false;
}

// std.meta.Filter
template Filter(alias pred, TList...)
{
    static if (TList.length == 0)
    {
        alias Filter = AliasSeq!();
    }
    else static if (TList.length == 1)
    {
        static if (pred!(TList[0]))
            alias Filter = AliasSeq!(TList[0]);
        else
            alias Filter = AliasSeq!();
    }
    /* The next case speeds up compilation by reducing
     * the number of Filter instantiations
     */
    else static if (TList.length == 2)
    {
        static if (pred!(TList[0]))
        {
            static if (pred!(TList[1]))
                alias Filter = AliasSeq!(TList[0], TList[1]);
            else
                alias Filter = AliasSeq!(TList[0]);
        }
        else
        {
            static if (pred!(TList[1]))
                alias Filter = AliasSeq!(TList[1]);
            else
                alias Filter = AliasSeq!();
        }
    }
    else
    {
        alias Filter =
            AliasSeq!(
                Filter!(pred, TList[ 0  .. $/2]),
                Filter!(pred, TList[$/2 ..  $ ]));
    }
}

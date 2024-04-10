/**
 * Contains traits for runtime internal usage.
 *
 * Copyright: Copyright Digital Mars 2014 -.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 * Source: $(DRUNTIMESRC core/internal/_traits.d)
 */
module core.internal.traits;

alias AliasSeq(TList...) = TList;

template Fields(T)
{
    static if (is(T == struct) || is(T == union))
        alias Fields = typeof(T.tupleof[0 .. $ - __traits(isNested, T)]);
    else static if (is(T == class) || is(T == interface))
        alias Fields = typeof(T.tupleof);
    else
        alias Fields = AliasSeq!T;
}

T trustedCast(T, U)(auto ref U u) @trusted pure nothrow
{
    return cast(T)u;
}

alias Unconst(T : const U, U) = U;

/// taken from std.traits.Unqual
template Unqual(T : const U, U)
{
    static if (is(U == shared V, V))
        alias Unqual = V;
    else
        alias Unqual = U;
}

template BaseElemOf(T)
{
    static if (is(OriginalType!T == E[N], E, size_t N))
        alias BaseElemOf = BaseElemOf!E;
    else
        alias BaseElemOf = T;
}

unittest
{
    static assert(is(BaseElemOf!(int) == int));
    static assert(is(BaseElemOf!(int[1]) == int));
    static assert(is(BaseElemOf!(int[1][2]) == int));
    static assert(is(BaseElemOf!(int[1][]) == int[1][]));
    static assert(is(BaseElemOf!(int[][1]) == int[]));
    static enum E : int[2]{ test = [0, 1] }
    static assert(is(BaseElemOf!(E) == int));
}

// [For internal use]
template ModifyTypePreservingTQ(alias Modifier, T)
{
         static if (is(T U ==          immutable U)) alias ModifyTypePreservingTQ =          immutable Modifier!U;
    else static if (is(T U == shared inout const U)) alias ModifyTypePreservingTQ = shared inout const Modifier!U;
    else static if (is(T U == shared inout       U)) alias ModifyTypePreservingTQ = shared inout       Modifier!U;
    else static if (is(T U == shared       const U)) alias ModifyTypePreservingTQ = shared       const Modifier!U;
    else static if (is(T U == shared             U)) alias ModifyTypePreservingTQ = shared             Modifier!U;
    else static if (is(T U ==        inout const U)) alias ModifyTypePreservingTQ =        inout const Modifier!U;
    else static if (is(T U ==        inout       U)) alias ModifyTypePreservingTQ =              inout Modifier!U;
    else static if (is(T U ==              const U)) alias ModifyTypePreservingTQ =              const Modifier!U;
    else                                             alias ModifyTypePreservingTQ =                    Modifier!T;
}
@safe unittest
{
    alias Intify(T) = int;
    static assert(is(ModifyTypePreservingTQ!(Intify,                    real) ==                    int));
    static assert(is(ModifyTypePreservingTQ!(Intify,              const real) ==              const int));
    static assert(is(ModifyTypePreservingTQ!(Intify,        inout       real) ==        inout       int));
    static assert(is(ModifyTypePreservingTQ!(Intify,        inout const real) ==        inout const int));
    static assert(is(ModifyTypePreservingTQ!(Intify, shared             real) == shared             int));
    static assert(is(ModifyTypePreservingTQ!(Intify, shared       const real) == shared       const int));
    static assert(is(ModifyTypePreservingTQ!(Intify, shared inout       real) == shared inout       int));
    static assert(is(ModifyTypePreservingTQ!(Intify, shared inout const real) == shared inout const int));
    static assert(is(ModifyTypePreservingTQ!(Intify,          immutable real) ==          immutable int));
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

private struct __InoutWorkaroundStruct {}
@property T rvalueOf(T)(T val) { return val; }
@property T rvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);
@property ref T lvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);

// taken from std.traits.isAssignable
template isAssignable(Lhs, Rhs = Lhs)
{
    enum isAssignable = __traits(compiles, lvalueOf!Lhs = rvalueOf!Rhs) && __traits(compiles, lvalueOf!Lhs = lvalueOf!Rhs);
}

// taken from std.traits.isInnerClass
template isInnerClass(T) if (is(T == class))
{
    static if (is(typeof(T.outer)))
    {
        template hasOuterMember(T...)
        {
            static if (T.length == 0)
                enum hasOuterMember = false;
            else
                enum hasOuterMember = T[0] == "outer" || hasOuterMember!(T[1 .. $]);
        }
        enum isInnerClass = __traits(isSame, typeof(T.outer), __traits(parent, T)) && !hasOuterMember!(__traits(allMembers, T));
    }
    else
        enum isInnerClass = false;
}

template dtorIsNothrow(T)
{
    enum dtorIsNothrow = is(typeof(function{T t=void;}) : void function() nothrow);
}

// taken from std.meta.allSatisfy
template allSatisfy(alias F, T...)
{
    static foreach (Ti; T)
    {
        static if (!is(typeof(allSatisfy) == bool) && // not yet defined
                   !F!(Ti))
        {
            enum allSatisfy = false;
        }
    }
    static if (!is(typeof(allSatisfy) == bool)) // if not yet defined
    {
        enum allSatisfy = true;
    }
}

// taken from std.meta.anySatisfy
template anySatisfy(alias F, Ts...)
{
    static foreach (T; Ts)
    {
        static if (!is(typeof(anySatisfy) == bool) && // not yet defined
                   F!T)
        {
            enum anySatisfy = true;
        }
    }
    static if (!is(typeof(anySatisfy) == bool)) // if not yet defined
    {
        enum anySatisfy = false;
    }
}

// simplified from std.traits.maxAlignment
template maxAlignment(Ts...)
if (Ts.length > 0)
{
    enum maxAlignment =
    {
        size_t result = 0;
        static foreach (T; Ts)
            if (T.alignof > result) result = T.alignof;
        return result;
    }();
}

template classInstanceAlignment(T)
if (is(T == class))
{
    enum classInstanceAlignment = __traits(classInstanceAlignment, T);
}

/// See $(REF hasElaborateMove, std,traits)
template hasElaborateMove(S)
{
    static if (__traits(isStaticArray, S))
    {
        enum bool hasElaborateMove = S.sizeof && hasElaborateMove!(BaseElemOf!S);
    }
    else static if (is(S == struct))
    {
        enum hasElaborateMove = (is(typeof(S.init.opPostMove(lvalueOf!S))) &&
                                    !is(typeof(S.init.opPostMove(rvalueOf!S)))) ||
                                anySatisfy!(.hasElaborateMove, Fields!S);
    }
    else
    {
        enum bool hasElaborateMove = false;
    }
}

// std.traits.hasElaborateDestructor
template hasElaborateDestructor(S)
{
    static if (__traits(isStaticArray, S))
    {
        enum bool hasElaborateDestructor = S.sizeof && hasElaborateDestructor!(BaseElemOf!S);
    }
    else static if (is(S == struct))
    {
        enum hasElaborateDestructor = __traits(hasMember, S, "__dtor")
            || anySatisfy!(.hasElaborateDestructor, Fields!S);
    }
    else
    {
        enum bool hasElaborateDestructor = false;
    }
}

// std.traits.hasElaborateCopyDestructor
template hasElaborateCopyConstructor(S)
{
    static if (__traits(isStaticArray, S))
    {
        enum bool hasElaborateCopyConstructor = S.sizeof && hasElaborateCopyConstructor!(BaseElemOf!S);
    }
    else static if (is(S == struct))
    {
        enum hasElaborateCopyConstructor = __traits(hasCopyConstructor, S) || __traits(hasPostblit, S);
    }
    else
    {
        enum bool hasElaborateCopyConstructor = false;
    }
}

@safe unittest
{
    static struct S
    {
        int x;
        this(return scope ref typeof(this) rhs) { }
        this(int x, int y) {}
    }

    static assert(hasElaborateCopyConstructor!S);
    static assert(!hasElaborateCopyConstructor!(S[0][1]));

    static struct S2
    {
        int x;
        this(int x, int y) {}
    }

    static assert(!hasElaborateCopyConstructor!S2);

    static struct S3
    {
        int x;
        this(return scope ref typeof(this) rhs, int x = 42) { }
        this(int x, int y) {}
    }

    static assert(hasElaborateCopyConstructor!S3);
}

template hasElaborateAssign(S)
{
    static if (__traits(isStaticArray, S))
    {
        enum bool hasElaborateAssign = S.sizeof && hasElaborateAssign!(BaseElemOf!S);
    }
    else static if (is(S == struct))
    {
        enum hasElaborateAssign = is(typeof(S.init.opAssign(rvalueOf!S))) ||
                                  is(typeof(S.init.opAssign(lvalueOf!S))) ||
                                  anySatisfy!(.hasElaborateAssign, Fields!S);
    }
    else
    {
        enum bool hasElaborateAssign = false;
    }
}

template hasIndirections(T)
{
    static if (is(T == struct) || is(T == union))
        enum hasIndirections = anySatisfy!(.hasIndirections, typeof(T.tupleof));
    else static if (is(T == E[N], E, size_t N))
        enum hasIndirections = T.sizeof && is(E == void) ? true : hasIndirections!(BaseElemOf!E);
    else static if (isFunctionPointer!T)
        enum hasIndirections = false;
    else
        enum hasIndirections = isPointer!T || isDelegate!T || isDynamicArray!T ||
            __traits(isAssociativeArray, T) || is (T == class) || is(T == interface);
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

    int local;
    struct HasContextPointer { int opCall() { return ++local; } }
    static assert(hasIndirections!HasContextPointer);
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

// std.meta.staticMap
template staticMap(alias F, T...)
{
    static if (T.length == 0)
    {
        alias staticMap = AliasSeq!();
    }
    else static if (T.length == 1)
    {
        alias staticMap = AliasSeq!(F!(T[0]));
    }
    /* Cases 2 to 8 improve compile performance by reducing
     * the number of recursive instantiations of staticMap
     */
    else static if (T.length == 2)
    {
        alias staticMap = AliasSeq!(F!(T[0]), F!(T[1]));
    }
    else static if (T.length == 3)
    {
        alias staticMap = AliasSeq!(F!(T[0]), F!(T[1]), F!(T[2]));
    }
    else static if (T.length == 4)
    {
        alias staticMap = AliasSeq!(F!(T[0]), F!(T[1]), F!(T[2]), F!(T[3]));
    }
    else static if (T.length == 5)
    {
        alias staticMap = AliasSeq!(F!(T[0]), F!(T[1]), F!(T[2]), F!(T[3]), F!(T[4]));
    }
    else static if (T.length == 6)
    {
        alias staticMap = AliasSeq!(F!(T[0]), F!(T[1]), F!(T[2]), F!(T[3]), F!(T[4]), F!(T[5]));
    }
    else static if (T.length == 7)
    {
        alias staticMap = AliasSeq!(F!(T[0]), F!(T[1]), F!(T[2]), F!(T[3]), F!(T[4]), F!(T[5]), F!(T[6]));
    }
    else static if (T.length == 8)
    {
        alias staticMap = AliasSeq!(F!(T[0]), F!(T[1]), F!(T[2]), F!(T[3]), F!(T[4]), F!(T[5]), F!(T[6]), F!(T[7]));
    }
    else
    {
        alias staticMap =
            AliasSeq!(
                staticMap!(F, T[ 0  .. $/2]),
                staticMap!(F, T[$/2 ..  $ ]));
    }
}

// std.exception.assertCTFEable
version (CoreUnittest) package(core)
void assertCTFEable(alias dg)()
{
    static assert({ cast(void) dg(); return true; }());
    cast(void) dg();
}

// std.traits.FunctionTypeOf
/*
Get the function type from a callable object `func`.

Using builtin `typeof` on a property function yields the types of the
property value, not of the property function itself.  Still,
`FunctionTypeOf` is able to obtain function types of properties.

Note:
Do not confuse function types with function pointer types; function types are
usually used for compile-time reflection purposes.
 */
template FunctionTypeOf(func...)
if (func.length == 1 /*&& isCallable!func*/)
{
    static if (is(typeof(& func[0]) Fsym : Fsym*) && is(Fsym == function) || is(typeof(& func[0]) Fsym == delegate))
    {
        alias FunctionTypeOf = Fsym; // HIT: (nested) function symbol
    }
    else static if (is(typeof(& func[0].opCall) Fobj == delegate))
    {
        alias FunctionTypeOf = Fobj; // HIT: callable object
    }
    else static if (is(typeof(& func[0].opCall) Ftyp : Ftyp*) && is(Ftyp == function))
    {
        alias FunctionTypeOf = Ftyp; // HIT: callable type
    }
    else static if (is(func[0] T) || is(typeof(func[0]) T))
    {
        static if (is(T == function))
            alias FunctionTypeOf = T;    // HIT: function
        else static if (is(T Fptr : Fptr*) && is(Fptr == function))
            alias FunctionTypeOf = Fptr; // HIT: function pointer
        else static if (is(T Fdlg == delegate))
            alias FunctionTypeOf = Fdlg; // HIT: delegate
        else
            static assert(0);
    }
    else
        static assert(0);
}

@safe unittest
{
    class C
    {
        int value() @property { return 0; }
    }
    static assert(is( typeof(C.value) == int ));
    static assert(is( FunctionTypeOf!(C.value) == function ));
}

@system unittest
{
    int test(int a);
    int propGet() @property;
    int propSet(int a) @property;
    int function(int) test_fp;
    int delegate(int) test_dg;
    static assert(is( typeof(test) == FunctionTypeOf!(typeof(test)) ));
    static assert(is( typeof(test) == FunctionTypeOf!test ));
    static assert(is( typeof(test) == FunctionTypeOf!test_fp ));
    static assert(is( typeof(test) == FunctionTypeOf!test_dg ));
    alias int GetterType() @property;
    alias int SetterType(int) @property;
    static assert(is( FunctionTypeOf!propGet == GetterType ));
    static assert(is( FunctionTypeOf!propSet == SetterType ));

    interface Prop { int prop() @property; }
    Prop prop;
    static assert(is( FunctionTypeOf!(Prop.prop) == GetterType ));
    static assert(is( FunctionTypeOf!(prop.prop) == GetterType ));

    class Callable { int opCall(int) { return 0; } }
    auto call = new Callable;
    static assert(is( FunctionTypeOf!call == typeof(test) ));

    struct StaticCallable { static int opCall(int) { return 0; } }
    StaticCallable stcall_val;
    StaticCallable* stcall_ptr;
    static assert(is( FunctionTypeOf!stcall_val == typeof(test) ));
    static assert(is( FunctionTypeOf!stcall_ptr == typeof(test) ));

    interface Overloads
    {
        void test(string);
        real test(real);
        int  test(int);
        int  test() @property;
    }
    alias ov = __traits(getVirtualMethods, Overloads, "test");
    alias F_ov0 = FunctionTypeOf!(ov[0]);
    alias F_ov1 = FunctionTypeOf!(ov[1]);
    alias F_ov2 = FunctionTypeOf!(ov[2]);
    alias F_ov3 = FunctionTypeOf!(ov[3]);
    static assert(is(F_ov0* == void function(string)));
    static assert(is(F_ov1* == real function(real)));
    static assert(is(F_ov2* == int function(int)));
    static assert(is(F_ov3* == int function() @property));

    alias F_dglit = FunctionTypeOf!((int a){ return a; });
    static assert(is(F_dglit* : int function(int)));
}

// std.traits.ReturnType
/*
Get the type of the return value from a function,
a pointer to function, a delegate, a struct
with an opCall, a pointer to a struct with an opCall,
or a class with an `opCall`. Please note that $(D_KEYWORD ref)
is not part of a type, but the attribute of the function
(see template $(LREF functionAttributes)).
*/
template ReturnType(func...)
if (func.length == 1 /*&& isCallable!func*/)
{
    static if (is(FunctionTypeOf!func R == return))
        alias ReturnType = R;
    else
        static assert(0, "argument has no return type");
}

//
@safe unittest
{
    int foo();
    ReturnType!foo x;   // x is declared as int
}

@safe unittest
{
    struct G
    {
        int opCall (int i) { return 1;}
    }

    alias ShouldBeInt = ReturnType!G;
    static assert(is(ShouldBeInt == int));

    G g;
    static assert(is(ReturnType!g == int));

    G* p;
    alias pg = ReturnType!p;
    static assert(is(pg == int));

    class C
    {
        int opCall (int i) { return 1;}
    }

    static assert(is(ReturnType!C == int));

    C c;
    static assert(is(ReturnType!c == int));

    class Test
    {
        int prop() @property { return 0; }
    }
    alias R_Test_prop = ReturnType!(Test.prop);
    static assert(is(R_Test_prop == int));

    alias R_dglit = ReturnType!((int a) { return a; });
    static assert(is(R_dglit == int));
}

// std.traits.Parameters
/*
Get, as a tuple, the types of the parameters to a function, a pointer
to function, a delegate, a struct with an `opCall`, a pointer to a
struct with an `opCall`, or a class with an `opCall`.
*/
template Parameters(func...)
if (func.length == 1 /*&& isCallable!func*/)
{
    static if (is(FunctionTypeOf!func P == function))
        alias Parameters = P;
    else
        static assert(0, "argument has no parameters");
}

//
@safe unittest
{
    int foo(int, long);
    void bar(Parameters!foo);      // declares void bar(int, long);
    void abc(Parameters!foo[1]);   // declares void abc(long);
}

@safe unittest
{
    int foo(int i, bool b) { return 0; }
    static assert(is(Parameters!foo == AliasSeq!(int, bool)));
    static assert(is(Parameters!(typeof(&foo)) == AliasSeq!(int, bool)));

    struct S { real opCall(real r, int i) { return 0.0; } }
    S s;
    static assert(is(Parameters!S == AliasSeq!(real, int)));
    static assert(is(Parameters!(S*) == AliasSeq!(real, int)));
    static assert(is(Parameters!s == AliasSeq!(real, int)));

    class Test
    {
        int prop() @property { return 0; }
    }
    alias P_Test_prop = Parameters!(Test.prop);
    static assert(P_Test_prop.length == 0);

    alias P_dglit = Parameters!((int a){});
    static assert(P_dglit.length == 1);
    static assert(is(P_dglit[0] == int));
}

// Return `true` if `Type` has `member` that evaluates to `true` in a static if condition
enum isTrue(Type, string member) = __traits(compiles, { static if (__traits(getMember, Type, member)) {} else static assert(0); });

unittest
{
    static struct T
    {
        enum a = true;
        enum b = false;
        enum c = 1;
        enum d = 45;
        enum e = "true";
        enum f = "";
        enum g = null;
        alias h = bool;
    }

    static assert( isTrue!(T, "a"));
    static assert(!isTrue!(T, "b"));
    static assert( isTrue!(T, "c"));
    static assert( isTrue!(T, "d"));
    static assert( isTrue!(T, "e"));
    static assert( isTrue!(T, "f"));
    static assert(!isTrue!(T, "g"));
    static assert(!isTrue!(T, "h"));
}

template hasUDA(alias symbol, alias attribute)
{
    alias attrs = __traits(getAttributes, symbol);

    static foreach (a; attrs)
    {
        static if (is(a == attribute))
        {
            enum hasUDA = true;
        }
    }

    static if (!__traits(compiles, (hasUDA == true)))
        enum hasUDA = false;
}

unittest
{
    struct SomeUDA{}

    struct Test
    {
        int woUDA;
        @SomeUDA int withUDA;
    }

    static assert(hasUDA!(Test.withUDA, SomeUDA));
    static assert(!hasUDA!(Test.woUDA, SomeUDA));
}

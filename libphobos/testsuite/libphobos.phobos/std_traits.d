@safe unittest
{
    import std.traits;

    static assert(is(InoutOf!(int) == inout int));
    static assert(is(InoutOf!(inout int) == inout int));
    static assert(is(InoutOf!(const int) == inout const int));
    static assert(is(InoutOf!(shared int) == inout shared int));
}

@safe unittest
{
    import std.traits;

    static assert(is(ConstOf!(int) == const int));
    static assert(is(ConstOf!(const int) == const int));
    static assert(is(ConstOf!(inout int) == const inout int));
    static assert(is(ConstOf!(shared int) == const shared int));
}

@safe unittest
{
    import std.traits;

    static assert(is(SharedOf!(int) == shared int));
    static assert(is(SharedOf!(shared int) == shared int));
    static assert(is(SharedOf!(inout int) == shared inout int));
    static assert(is(SharedOf!(immutable int) == shared immutable int));
}

@safe unittest
{
    import std.traits;

    static assert(is(SharedInoutOf!(int) == shared inout int));
    static assert(is(SharedInoutOf!(int) == inout shared int));

    static assert(is(SharedInoutOf!(const int) == shared inout const int));
    static assert(is(SharedInoutOf!(immutable int) == shared inout immutable int));
}

@safe unittest
{
    import std.traits;

    static assert(is(SharedConstOf!(int) == shared const int));
    static assert(is(SharedConstOf!(int) == const shared int));

    static assert(is(SharedConstOf!(inout int) == shared inout const int));
    // immutable variables are implicitly shared and const
    static assert(is(SharedConstOf!(immutable int) == immutable int));
}

@safe unittest
{
    import std.traits;

    static assert(is(SharedConstInoutOf!(int) == shared const inout int));
    static assert(is(SharedConstInoutOf!(int) == const shared inout int));
    static assert(is(SharedConstInoutOf!(inout int) == shared inout const int));
    // immutable variables are implicitly shared and const
    static assert(is(SharedConstInoutOf!(immutable int) == immutable int));
}

@safe unittest
{
    import std.traits;

    static assert(is(ImmutableOf!(int) == immutable int));
    static assert(is(ImmutableOf!(const int) == immutable int));
    static assert(is(ImmutableOf!(inout int) == immutable int));
    static assert(is(ImmutableOf!(shared int) == immutable int));
}

@safe unittest
{
    import std.traits;

    static assert(__traits(isSame, QualifierOf!(shared const inout int), SharedConstInoutOf));
    static assert(__traits(isSame, QualifierOf!(immutable int), ImmutableOf));
    static assert(__traits(isSame, QualifierOf!(shared int), SharedOf));
    static assert(__traits(isSame, QualifierOf!(shared inout int), SharedInoutOf));
    import std.meta : Alias;
    static assert(__traits(isSame, QualifierOf!(int), Alias));
}

@safe unittest
{
    import std.traits;

    static assert(packageName!packageName == "std");
}

@safe unittest
{
    import std.traits;

    static assert(packageName!moduleName == "std");
}

@safe unittest
{
    import std.traits;

    static assert(moduleName!moduleName == "std.traits");
}

@safe unittest
{
    import std.traits;

    static assert(fullyQualifiedName!fullyQualifiedName == "std.traits.fullyQualifiedName");
}

@safe unittest
{
    import std.traits;

    int foo();
    ReturnType!foo x;   // x is declared as int
}

@safe unittest
{
    import std.traits;

    int foo(int, long);
    void bar(Parameters!foo);      // declares void bar(int, long);
    void abc(Parameters!foo[1]);   // declares void abc(long);
}

@safe unittest
{
    import std.traits;

    void foo(){}
    static assert(arity!foo == 0);
    void bar(uint){}
    static assert(arity!bar == 1);
    void variadicFoo(uint...){}
    static assert(!__traits(compiles, arity!variadicFoo));
}

@safe unittest
{
    import std.traits;

    alias STC = ParameterStorageClass; // shorten the enum name

    void func(ref int ctx, out real result, in real param, void* ptr)
    {
    }
    alias pstc = ParameterStorageClassTuple!func;
    static assert(pstc.length == 4); // number of parameters
    static assert(pstc[0] == STC.ref_);
    static assert(pstc[1] == STC.out_);
    version (none)
    {
        // TODO: When the DMD PR (dlang/dmd#11474) gets merged,
        // remove the versioning and the second test
        static assert(pstc[2] == STC.in_);
        // This is the current behavior, before `in` is fixed to not be an alias
        static assert(pstc[2] == STC.scope_);
    }
    static assert(pstc[3] == STC.none);
}

@safe unittest
{
    import std.traits;

    static void func(ref int ctx, out real result);

    enum param1 = extractParameterStorageClassFlags!(
        __traits(getParameterStorageClasses, func, 0)
    );
    static assert(param1 == ParameterStorageClass.ref_);

    enum param2 = extractParameterStorageClassFlags!(
        __traits(getParameterStorageClasses, func, 1)
    );
    static assert(param2 == ParameterStorageClass.out_);

    enum param3 = extractParameterStorageClassFlags!(
        __traits(getParameterStorageClasses, func, 0),
        __traits(getParameterStorageClasses, func, 1)
    );
    static assert(param3 == (ParameterStorageClass.ref_ | ParameterStorageClass.out_));
}

@safe unittest
{
    import std.traits;

    int foo(int num, string name, int);
    static assert([ParameterIdentifierTuple!foo] == ["num", "name", ""]);
}

@safe unittest
{
    import std.traits;

    int foo(int num, string name = "hello", int[] = [1,2,3], lazy int x = 0);
    static assert(is(ParameterDefaults!foo[0] == void));
    static assert(   ParameterDefaults!foo[1] == "hello");
    static assert(   ParameterDefaults!foo[2] == [1,2,3]);
    static assert(   ParameterDefaults!foo[3] == 0);
}

@safe unittest
{
    import std.traits;

    alias FA = FunctionAttribute; // shorten the enum name

    real func(real x) pure nothrow @safe
    {
        return x;
    }
    static assert(functionAttributes!func & FA.pure_);
    static assert(functionAttributes!func & FA.safe);
    static assert(!(functionAttributes!func & FA.trusted)); // not @trusted
}

@safe unittest
{
    import std.traits;

    real func(real x) pure nothrow @safe;
    static assert(hasFunctionAttributes!(func, "@safe", "pure"));
    static assert(!hasFunctionAttributes!(func, "@trusted"));

    // for templates attributes are automatically inferred
    bool myFunc(T)(T b)
    {
        return !b;
    }
    static assert(hasFunctionAttributes!(myFunc!bool, "@safe", "pure", "@nogc", "nothrow"));
    static assert(!hasFunctionAttributes!(myFunc!bool, "shared"));
}

@safe unittest
{
    import std.traits;

    @safe    int add(int a, int b) {return a+b;}
    @trusted int sub(int a, int b) {return a-b;}
    @system  int mul(int a, int b) {return a*b;}

    static assert( isSafe!add);
    static assert( isSafe!sub);
    static assert(!isSafe!mul);
}

@safe unittest
{
    import std.traits;

    @safe    int add(int a, int b) {return a+b;}
    @trusted int sub(int a, int b) {return a-b;}
    @system  int mul(int a, int b) {return a*b;}

    static assert(!isUnsafe!add);
    static assert(!isUnsafe!sub);
    static assert( isUnsafe!mul);
}

@safe unittest
{
    import std.traits;

    extern(D) void Dfunc() {}
    extern(C) void Cfunc() {}
    static assert(functionLinkage!Dfunc == "D");
    static assert(functionLinkage!Cfunc == "C");

    string a = functionLinkage!Dfunc;
    assert(a == "D");

    auto fp = &Cfunc;
    string b = functionLinkage!fp;
    assert(b == "C");
}

@safe unittest
{
    import std.traits;

    void func() {}
    static assert(variadicFunctionStyle!func == Variadic.no);

    extern(C) int printf(const char*, ...);
    static assert(variadicFunctionStyle!printf == Variadic.c);
}

@safe unittest
{
    import std.traits;

    class C
    {
        int value() @property => 0;
        static string opCall() => "hi";
    }
    static assert(is( typeof(C.value) == int ));
    static assert(is( FunctionTypeOf!(C.value) == function ));
    static assert(is( FunctionTypeOf!C == typeof(C.opCall) ));

    int function() fp;
    alias IntFn = int();
    static assert(is( typeof(fp) == IntFn* ));
    static assert(is( FunctionTypeOf!fp == IntFn ));
}

@safe unittest
{
    import std.traits;

    alias ExternC(T) = SetFunctionAttributes!(T, "C", functionAttributes!T);

    auto assumePure(T)(T t)
    if (isFunctionPointer!T || isDelegate!T)
    {
        enum attrs = functionAttributes!T | FunctionAttribute.pure_;
        return cast(SetFunctionAttributes!(T, functionLinkage!T, attrs)) t;
    }

    int f()
    {
        import core.thread : getpid;
        return getpid();
    }

    int g() pure @trusted
    {
        auto pureF = assumePure(&f);
        return pureF();
    }
    assert(g() > 0);
}

@safe unittest
{
    import std.traits;

    class C
    {
        int outer;
    }
    static assert(!isInnerClass!C);

    class Outer1
    {
        class Inner1 { }
        class Inner2
        {
            int outer;
        }
    }
    static assert(isInnerClass!(Outer1.Inner1));
    static assert(!isInnerClass!(Outer1.Inner2));

    static class Outer2
    {
        static class Inner
        {
            int outer;
        }
    }
    static assert(!isInnerClass!(Outer2.Inner));
}

@safe unittest
{
    import std.traits;

    static struct S { }
    static assert(!isNested!S);

    int i;
    struct NestedStruct { void f() { ++i; } }
    static assert(isNested!NestedStruct);
}

@safe unittest
{
    import std.traits;

    static struct S { }

    int i;
    struct NS { void f() { ++i; } }

    static assert(!hasNested!(S[2]));
    static assert(hasNested!(NS[2]));
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;
    struct S { int x; float y; }
    static assert(is(Fields!S == AliasSeq!(int, float)));
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;
    struct S { int x; float y; }
    static assert(FieldNameTuple!S == AliasSeq!("x", "y"));
    static assert(FieldNameTuple!int == AliasSeq!"");
}

@safe unittest
{
    import std.traits;

    struct S1 { int a; float b; }
    struct S2 { char[] a; union { S1 b; S1 * c; } }
    alias R = RepresentationTypeTuple!S2;
    assert(R.length == 4
        && is(R[0] == char[]) && is(R[1] == int)
        && is(R[2] == float) && is(R[3] == S1*));
}

@safe unittest
{
    import std.traits;

    struct S1 { int a; Object b; }
    struct S2 { string a; }
    struct S3 { int a; immutable Object b; }
    struct S4 { float[3] vals; }
    static assert( hasAliasing!S1);
    static assert(!hasAliasing!S2);
    static assert(!hasAliasing!S3);
    static assert(!hasAliasing!S4);
}

@safe unittest
{
    import std.traits;

    static assert( hasIndirections!(int[string]));
    static assert( hasIndirections!(void delegate()));
    static assert( hasIndirections!(void delegate() immutable));
    static assert( hasIndirections!(immutable(void delegate())));
    static assert( hasIndirections!(immutable(void delegate() immutable)));

    static assert(!hasIndirections!(void function()));
    static assert( hasIndirections!(void*[1]));
    static assert(!hasIndirections!(byte[1]));
}

@safe unittest
{
    import std.traits;

    struct S1 { int a; Object b; }
    struct S2 { string a; }
    struct S3 { int a; immutable Object b; }
    static assert( hasUnsharedAliasing!S1);
    static assert(!hasUnsharedAliasing!S2);
    static assert(!hasUnsharedAliasing!S3);

    struct S4 { int a; shared Object b; }
    struct S5 { char[] a; }
    struct S6 { shared char[] b; }
    struct S7 { float[3] vals; }
    static assert(!hasUnsharedAliasing!S4);
    static assert( hasUnsharedAliasing!S5);
    static assert(!hasUnsharedAliasing!S6);
    static assert(!hasUnsharedAliasing!S7);
}

@safe unittest
{
    import std.traits;

    static assert(!hasElaborateCopyConstructor!int);

    static struct S1 { }
    static struct S2 { this(this) {} }
    static struct S3 { S2 field; }
    static struct S4 { S3[1] field; }
    static struct S5 { S3[] field; }
    static struct S6 { S3[0] field; }
    static struct S7 { @disable this(); S3 field; }
    static assert(!hasElaborateCopyConstructor!S1);
    static assert( hasElaborateCopyConstructor!S2);
    static assert( hasElaborateCopyConstructor!(immutable S2));
    static assert( hasElaborateCopyConstructor!S3);
    static assert( hasElaborateCopyConstructor!(S3[1]));
    static assert(!hasElaborateCopyConstructor!(S3[0]));
    static assert( hasElaborateCopyConstructor!S4);
    static assert(!hasElaborateCopyConstructor!S5);
    static assert(!hasElaborateCopyConstructor!S6);
    static assert( hasElaborateCopyConstructor!S7);
}

@safe unittest
{
    import std.traits;

    static assert(!hasElaborateAssign!int);

    static struct S  { void opAssign(S) {} }
    static assert( hasElaborateAssign!S);
    static assert(!hasElaborateAssign!(const(S)));

    static struct S1 { void opAssign(ref S1) {} }
    static struct S2 { void opAssign(int) {} }
    static struct S3 { S s; }
    static assert( hasElaborateAssign!S1);
    static assert(!hasElaborateAssign!S2);
    static assert( hasElaborateAssign!S3);
    static assert( hasElaborateAssign!(S3[1]));
    static assert(!hasElaborateAssign!(S3[0]));
}

@safe unittest
{
    import std.traits;

    static assert(!hasElaborateDestructor!int);

    static struct S1 { }
    static struct S2 { ~this() {} }
    static struct S3 { S2 field; }
    static struct S4 { S3[1] field; }
    static struct S5 { S3[] field; }
    static struct S6 { S3[0] field; }
    static struct S7 { @disable this(); S3 field; }
    static assert(!hasElaborateDestructor!S1);
    static assert( hasElaborateDestructor!S2);
    static assert( hasElaborateDestructor!(immutable S2));
    static assert( hasElaborateDestructor!S3);
    static assert( hasElaborateDestructor!(S3[1]));
    static assert(!hasElaborateDestructor!(S3[0]));
    static assert( hasElaborateDestructor!S4);
    static assert(!hasElaborateDestructor!S5);
    static assert(!hasElaborateDestructor!S6);
    static assert( hasElaborateDestructor!S7);
}

@safe unittest
{
    import std.traits;

    static assert(!hasElaborateMove!int);

    static struct S1 { }
    static struct S2 { void opPostMove(ref S2) {} }
    static struct S3 { void opPostMove(inout ref S3) inout {} }
    static struct S4 { void opPostMove(const ref S4) {} }
    static struct S5 { void opPostMove(S5) {} }
    static struct S6 { void opPostMove(int) {} }
    static struct S7 { S3[1] field; }
    static struct S8 { S3[] field; }
    static struct S9 { S3[0] field; }
    static struct S10 { @disable this(); S3 field; }
    static assert(!hasElaborateMove!S1);
    static assert( hasElaborateMove!S2);
    static assert( hasElaborateMove!S3);
    static assert( hasElaborateMove!(immutable S3));
    static assert( hasElaborateMove!S4);
    static assert(!hasElaborateMove!S5);
    static assert(!hasElaborateMove!S6);
    static assert( hasElaborateMove!S7);
    static assert(!hasElaborateMove!S8);
    static assert(!hasElaborateMove!S9);
    static assert( hasElaborateMove!S10);
}

@safe unittest
{
    import std.traits;

    static assert(!hasMember!(int, "blah"));
    struct S1 { int blah; }
    struct S2 { int blah(){ return 0; } }
    class C1 { int blah; }
    class C2 { int blah(){ return 0; } }
    static assert(hasMember!(S1, "blah"));
    static assert(hasMember!(S2, "blah"));
    static assert(hasMember!(C1, "blah"));
    static assert(hasMember!(C2, "blah"));
}

@safe unittest
{
    import std.traits;

    static struct S
    {
        static void sf() {}
        void f() {}

        static int si;
        int i;
    }

    static assert( hasStaticMember!(S, "sf"));
    static assert(!hasStaticMember!(S, "f"));

    static assert( hasStaticMember!(S, "si"));
    static assert(!hasStaticMember!(S, "i"));

    static assert(!hasStaticMember!(S, "hello"));
}

@safe unittest
{
    import std.traits;

    enum Sqrts : real
    {
        one = 1,
        two = 1.41421,
        three = 1.73205
    }
    auto sqrts = [EnumMembers!Sqrts];
    assert(sqrts == [Sqrts.one, Sqrts.two, Sqrts.three]);
}

@safe unittest
{
    import std.traits;

    // Returns i if e is the i-th enumerator of E.
    static size_t rank(E)(E e)
    if (is(E == enum))
    {
        static foreach (i, member; EnumMembers!E)
        {
            if (e == member)
                return i;
        }
        assert(0, "Not an enum member");
    }

    enum Mode
    {
        read = 1,
        write = 2,
        map = 4
    }
    assert(rank(Mode.read) == 0);
    assert(rank(Mode.write) == 1);
    assert(rank(Mode.map) == 2);
}

@safe unittest
{
    import std.traits;

    import std.conv : to;
    class FooClass
    {
        string calledMethod;
        void foo() @safe { calledMethod = "foo"; }
        void bar() @safe { calledMethod = "bar"; }
        void baz() @safe { calledMethod = "baz"; }
    }

    enum FooEnum { foo, bar, baz }

    auto var = FooEnum.bar;
    auto fooObj = new FooClass();
    s: final switch (var)
    {
        static foreach (member; EnumMembers!FooEnum)
        {
            case member: // Generate a case for each enum value.
                // Call fooObj.{name of enum value}().
                __traits(getMember, fooObj, to!string(member))();
                break s;
        }
    }
    // As we pass in FooEnum.bar, the bar() method gets called.
    assert(fooObj.calledMethod == "bar");
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;

    interface I1 { }
    interface I2 { }
    interface I12 : I1, I2 { }
    static assert(is(BaseTypeTuple!I12 == AliasSeq!(I1, I2)));

    interface I3 : I1 { }
    interface I123 : I1, I2, I3 { }
    static assert(is(BaseTypeTuple!I123 == AliasSeq!(I1, I2, I3)));
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;

    class C1 { }
    class C2 : C1 { }
    class C3 : C2 { }
    static assert(!BaseClassesTuple!Object.length);
    static assert(is(BaseClassesTuple!C1 == AliasSeq!(Object)));
    static assert(is(BaseClassesTuple!C2 == AliasSeq!(C1, Object)));
    static assert(is(BaseClassesTuple!C3 == AliasSeq!(C2, C1, Object)));
}

@safe unittest
{
    import std.traits;

    interface I1 {}
    interface I2 {}
    class A : I1, I2 {}
    class B : A, I1 {}
    class C : B {}

    alias TL = InterfacesTuple!C;
    static assert(is(TL[0] == I1) && is(TL[1] == I2));
}

@safe unittest
{
    import std.traits;

    interface J1 {}
    interface J2 {}
    class B1 {}
    class B2 : B1, J1, J2 {}
    class B3 : B2, J1 {}
    alias TL = TransitiveBaseTypeTuple!B3;
    assert(TL.length == 5);
    assert(is (TL[0] == B2));
    assert(is (TL[1] == B1));
    assert(is (TL[2] == Object));
    assert(is (TL[3] == J1));
    assert(is (TL[4] == J2));

    assert(TransitiveBaseTypeTuple!Object.length == 0);
}

@safe unittest
{
    import std.traits;

    interface I { I foo(); }
    class B
    {
        real foo(real v) { return v; }
    }
    class C : B, I
    {
        override C foo() { return this; } // covariant overriding of I.foo()
    }
    alias foos = MemberFunctionsTuple!(C, "foo");
    static assert(foos.length == 2);
    static assert(__traits(isSame, foos[0], C.foo));
    static assert(__traits(isSame, foos[1], B.foo));
}

@safe unittest
{
    import std.traits;

    struct Foo(T, U) {}
    static assert(__traits(isSame, TemplateOf!(Foo!(int, real)), Foo));
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;

    struct Foo(T, U) {}
    static assert(is(TemplateArgsOf!(Foo!(int, real)) == AliasSeq!(int, real)));
}

@safe unittest
{
    import std.traits;

    class A { byte b; }
    class B { long l; }

    // As class instance always has a hidden pointer
    static assert(classInstanceAlignment!A == (void*).alignof);
    static assert(classInstanceAlignment!B == long.alignof);
}

@safe unittest
{
    import std.traits;

    alias X = CommonType!(int, long, short);
    assert(is(X == long));
    alias Y = CommonType!(int, char[], short);
    assert(is(Y == void));
}

@safe unittest
{
    import std.traits;

    static assert(is(CommonType!(3) == int));
    static assert(is(CommonType!(double, 4, float) == double));
    static assert(is(CommonType!(string, char[]) == const(char)[]));
    static assert(is(CommonType!(3, 3U) == uint));
    static assert(is(CommonType!(double, int) == double));
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;

    static assert(is(AllImplicitConversionTargets!(ulong) == AliasSeq!(long, float, double, real)));
    static assert(is(AllImplicitConversionTargets!(int) == AliasSeq!(dchar, uint, long, ulong, float, double, real)));
    static assert(is(AllImplicitConversionTargets!(float) == AliasSeq!(double, real)));
    static assert(is(AllImplicitConversionTargets!(double) == AliasSeq!(float, real)));

    static assert(is(AllImplicitConversionTargets!(char) ==
        AliasSeq!(byte, ubyte, short, ushort, wchar, int, dchar, uint, long,
            ulong, float, double, real)
    ));
    static assert(is(AllImplicitConversionTargets!(wchar) == AliasSeq!(
        short, ushort, dchar, int, uint, long, ulong, float, double, real
    )));
    static assert(is(AllImplicitConversionTargets!(dchar) == AliasSeq!(
        int, uint, long, ulong, float, double, real
    )));

    static assert(is(AllImplicitConversionTargets!(string) == AliasSeq!(const(char)[])));
    static assert(is(AllImplicitConversionTargets!(int*) == AliasSeq!(void*)));

    interface A {}
    interface B {}
    class C : A, B {}

    static assert(is(AllImplicitConversionTargets!(C) == AliasSeq!(Object, A, B)));
    static assert(is(AllImplicitConversionTargets!(const C) == AliasSeq!(const Object, const A, const B)));
    static assert(is(AllImplicitConversionTargets!(immutable C) == AliasSeq!(
        immutable Object, immutable A, immutable B
    )));

    interface I : A, B {}

    static assert(is(AllImplicitConversionTargets!(I) == AliasSeq!(A, B)));
    static assert(is(AllImplicitConversionTargets!(const I) == AliasSeq!(const A, const B)));
    static assert(is(AllImplicitConversionTargets!(immutable I) == AliasSeq!(
        immutable A, immutable B
    )));
}

@safe unittest
{
    import std.traits;

    static assert( isImplicitlyConvertible!(immutable(char), char));
    static assert( isImplicitlyConvertible!(const(char), char));
    static assert( isImplicitlyConvertible!(char, wchar));
    static assert(!isImplicitlyConvertible!(wchar, char));

    static assert(!isImplicitlyConvertible!(const(ushort), ubyte));
    static assert(!isImplicitlyConvertible!(const(uint), ubyte));
    static assert(!isImplicitlyConvertible!(const(ulong), ubyte));

    static assert(!isImplicitlyConvertible!(const(char)[], string));
    static assert( isImplicitlyConvertible!(string, const(char)[]));
}

@safe unittest
{
    import std.traits;

    // Mutable and immmutable both convert to const...
    static assert( isQualifierConvertible!(char, const(char)));
    static assert( isQualifierConvertible!(immutable(char), const(char)));
    // ...but const does not convert back to mutable or immutable
    static assert(!isQualifierConvertible!(const(char), char));
    static assert(!isQualifierConvertible!(const(char), immutable(char)));
}

@safe unittest
{
    import std.traits;

    static assert( isAssignable!(long, int));
    static assert(!isAssignable!(int, long));
    static assert( isAssignable!(const(char)[], string));
    static assert(!isAssignable!(string, char[]));

    // int is assignable to int
    static assert( isAssignable!int);

    // immutable int is not assignable to immutable int
    static assert(!isAssignable!(immutable int));
}

@safe unittest
{
    import std.traits;

    struct S1
    {
        void opAssign(S1);
    }

    struct S2
    {
        void opAssign(ref S2);
    }

    static assert( isRvalueAssignable!(long, int));
    static assert(!isRvalueAssignable!(int, long));
    static assert( isRvalueAssignable!S1);
    static assert(!isRvalueAssignable!S2);
}

@safe unittest
{
    import std.traits;

    struct S1
    {
        void opAssign(S1);
    }

    struct S2
    {
        void opAssign(ref S2);
    }

    static assert( isLvalueAssignable!(long, int));
    static assert(!isLvalueAssignable!(int, long));
    static assert( isLvalueAssignable!S1);
    static assert( isLvalueAssignable!S2);
}

@safe unittest
{
    import std.traits;

    interface I { I clone(); }
    interface J { J clone(); }
    class C : I
    {
        override C clone()   // covariant overriding of I.clone()
        {
            return new C;
        }
    }

    // C.clone() can override I.clone(), indeed.
    static assert(isCovariantWith!(typeof(C.clone), typeof(I.clone)));

    // C.clone() can't override J.clone(); the return type C is not implicitly
    // convertible to J.
    static assert(!isCovariantWith!(typeof(C.clone), typeof(J.clone)));
}

@system unittest
{
    import std.traits;

    static int f(int);
    static assert(is(typeof(f(rvalueOf!int)) == int));
}

@system unittest
{
    import std.traits;

    static bool f(ref int);
    static assert(is(typeof(f(lvalueOf!int)) == bool));
}

@safe unittest
{
    import std.traits;

    static assert( isBoolean!bool);
    enum EB : bool { a = true }
    static assert( isBoolean!EB);

    struct SubTypeOfBool
    {
        bool val;
        alias val this;
    }
    static assert(!isBoolean!(SubTypeOfBool));
}

@safe unittest
{
    import std.traits;

    static assert(
        isIntegral!byte &&
        isIntegral!short &&
        isIntegral!int &&
        isIntegral!long &&
        isIntegral!(const(long)) &&
        isIntegral!(immutable(long))
    );

    static assert(
        !isIntegral!bool &&
        !isIntegral!char &&
        !isIntegral!double
    );

    // types which act as integral values do not pass
    struct S
    {
        int val;
        alias val this;
    }

    static assert(!isIntegral!S);
}

@safe unittest
{
    import std.traits;

    static assert(
        isFloatingPoint!float &&
        isFloatingPoint!double &&
        isFloatingPoint!real &&
        isFloatingPoint!(const(real)) &&
        isFloatingPoint!(immutable(real))
    );

    static assert(!isFloatingPoint!int);

    // types which act as floating point values do not pass
    struct S
    {
        float val;
        alias val this;
    }

    static assert(!isFloatingPoint!S);
}

@safe unittest
{
    import std.traits;

    static assert(
        isNumeric!byte &&
        isNumeric!short &&
        isNumeric!int &&
        isNumeric!long &&
        isNumeric!float &&
        isNumeric!double &&
        isNumeric!real &&
        isNumeric!(const(real)) &&
        isNumeric!(immutable(real))
    );

    static assert(
        !isNumeric!void &&
        !isNumeric!bool &&
        !isNumeric!char &&
        !isNumeric!wchar &&
        !isNumeric!dchar
    );

    // types which act as numeric values do not pass
    struct S
    {
        int val;
        alias val this;
    }

    static assert(!isNumeric!S);
}

@safe unittest
{
    import std.traits;

    static assert(!isScalarType!void);
    static assert( isScalarType!(immutable(byte)));
    static assert( isScalarType!(immutable(ushort)));
    static assert( isScalarType!(immutable(int)));
    static assert( isScalarType!(ulong));
    static assert( isScalarType!(shared(float)));
    static assert( isScalarType!(shared(const bool)));
    static assert( isScalarType!(const(char)));
    static assert( isScalarType!(wchar));
    static assert( isScalarType!(const(dchar)));
    static assert( isScalarType!(const(double)));
    static assert( isScalarType!(const(real)));
}

@safe unittest
{
    import std.traits;

    static assert(isBasicType!void);
    static assert(isBasicType!(const(void)));
    static assert(isBasicType!(shared(void)));
    static assert(isBasicType!(immutable(void)));
    static assert(isBasicType!(shared const(void)));
    static assert(isBasicType!(shared inout(void)));
    static assert(isBasicType!(shared inout const(void)));
    static assert(isBasicType!(inout(void)));
    static assert(isBasicType!(inout const(void)));
    static assert(isBasicType!(immutable(int)));
    static assert(isBasicType!(shared(float)));
    static assert(isBasicType!(shared(const bool)));
    static assert(isBasicType!(const(dchar)));
}

@safe unittest
{
    import std.traits;

    static assert(
        isUnsigned!uint &&
        isUnsigned!ulong
    );

    static assert(
        !isUnsigned!char &&
        !isUnsigned!int &&
        !isUnsigned!long &&
        !isUnsigned!char &&
        !isUnsigned!wchar &&
        !isUnsigned!dchar
    );
}

@safe unittest
{
    import std.traits;

    static assert(
        isSigned!int &&
        isSigned!long
    );

    static assert(
        !isSigned!uint &&
        !isSigned!ulong
    );
}

@safe unittest
{
    import std.traits;

    //Char types
    static assert( isSomeChar!char);
    static assert( isSomeChar!wchar);
    static assert( isSomeChar!dchar);
    static assert( isSomeChar!(typeof('c')));
    static assert( isSomeChar!(immutable char));
    static assert( isSomeChar!(const dchar));

    //Non char types
    static assert(!isSomeChar!int);
    static assert(!isSomeChar!byte);
    static assert(!isSomeChar!string);
    static assert(!isSomeChar!wstring);
    static assert(!isSomeChar!dstring);
    static assert(!isSomeChar!(char[4]));
}

@safe unittest
{
    import std.traits;

    //String types
    static assert( isSomeString!string);
    static assert( isSomeString!(wchar[]));
    static assert( isSomeString!(dchar[]));
    static assert( isSomeString!(typeof("aaa")));
    static assert( isSomeString!(const(char)[]));

    //Non string types
    static assert(!isSomeString!int);
    static assert(!isSomeString!(int[]));
    static assert(!isSomeString!(byte[]));
    static assert(!isSomeString!(typeof(null)));
    static assert(!isSomeString!(char[4]));

    enum ES : string { a = "aaa", b = "bbb" }
    static assert(!isSomeString!ES);

    static struct Stringish
    {
        string str;
        alias str this;
    }
    static assert(!isSomeString!Stringish);
}

@safe unittest
{
    import std.traits;

    static assert(isNarrowString!string);
    static assert(isNarrowString!wstring);
    static assert(isNarrowString!(char[]));
    static assert(isNarrowString!(wchar[]));

    static assert(!isNarrowString!dstring);
    static assert(!isNarrowString!(dchar[]));

    static assert(!isNarrowString!(typeof(null)));
    static assert(!isNarrowString!(char[4]));

    enum ES : string { a = "aaa", b = "bbb" }
    static assert(!isNarrowString!ES);

    static struct Stringish
    {
        string str;
        alias str this;
    }
    static assert(!isNarrowString!Stringish);
}

@safe unittest
{
    import std.traits;

    static assert(isOrderingComparable!int);
    static assert(isOrderingComparable!string);

    static struct Foo {}
    static assert(!isOrderingComparable!Foo);

    static struct Bar
    {
        int a;
        auto opCmp(Bar b1) const { return a - b1.a; }
    }

    Bar b1 = Bar(5);
    Bar b2 = Bar(7);
    assert(isOrderingComparable!Bar && b2 > b1);
}

@safe unittest
{
    import std.traits;

    static struct AliasedString
    {
        string s;
        alias s this;
    }

    enum StringEnum { a = "foo" }

    assert(!isConvertibleToString!string);
    assert(isConvertibleToString!AliasedString);
    assert(isConvertibleToString!StringEnum);
    assert(isConvertibleToString!(char[25]));
    assert(!isConvertibleToString!(char[]));
}

@safe unittest
{
    import std.traits;

    static struct Stringish
    {
        string s;
        alias s this;
    }
    static assert(isAutodecodableString!wstring);
    static assert(isAutodecodableString!Stringish);
    static assert(!isAutodecodableString!dstring);

    enum E : const(char)[3] { X = "abc" }
    enum F : const(char)[] { X = "abc" }
    enum G : F { X = F.init }

    static assert(isAutodecodableString!(char[]));
    static assert(!isAutodecodableString!(E));
    static assert(isAutodecodableString!(F));
    static assert(isAutodecodableString!(G));

    struct Stringish2
    {
        Stringish s;
        alias s this;
    }

    enum H : Stringish { X = Stringish() }
    enum I : Stringish2 { X = Stringish2() }

    static assert(isAutodecodableString!(H));
    static assert(isAutodecodableString!(I));

    static assert(!isAutodecodableString!(noreturn[]));
    static assert(!isAutodecodableString!(immutable(noreturn)[]));
}

@safe unittest
{
    import std.traits;

    static assert( isStaticArray!(int[3]));
    static assert( isStaticArray!(const(int)[5]));
    static assert( isStaticArray!(const(int)[][5]));

    static assert(!isStaticArray!(const(int)[]));
    static assert(!isStaticArray!(immutable(int)[]));
    static assert(!isStaticArray!(const(int)[4][]));
    static assert(!isStaticArray!(int[]));
    static assert(!isStaticArray!(int[char]));
    static assert(!isStaticArray!(int[1][]));
    static assert(!isStaticArray!(int[int]));
    static assert(!isStaticArray!int);
}

@safe unittest
{
    import std.traits;

    static assert( isDynamicArray!(int[]));
    static assert( isDynamicArray!(string));
    static assert( isDynamicArray!(long[3][]));

    static assert(!isDynamicArray!(int[5]));
    static assert(!isDynamicArray!(typeof(null)));
}

@safe unittest
{
    import std.traits;

    static assert( isArray!(int[]));
    static assert( isArray!(int[5]));
    static assert( isArray!(string));

    static assert(!isArray!uint);
    static assert(!isArray!(uint[uint]));
    static assert(!isArray!(typeof(null)));
}

@safe unittest
{
    import std.traits;

    struct S;

    static assert( isAssociativeArray!(int[string]));
    static assert( isAssociativeArray!(S[S]));
    static assert(!isAssociativeArray!(string[]));
    static assert(!isAssociativeArray!S);
    static assert(!isAssociativeArray!(int[4]));
}

@safe unittest
{
    import std.traits;

    class C;
    union U;
    struct S;
    interface I;

    static assert( isBuiltinType!void);
    static assert( isBuiltinType!string);
    static assert( isBuiltinType!(int[]));
    static assert( isBuiltinType!(C[string]));
    static assert( isBuiltinType!(typeof(null)));
    static assert(!isBuiltinType!C);
    static assert(!isBuiltinType!U);
    static assert(!isBuiltinType!S);
    static assert(!isBuiltinType!I);
    static assert(!isBuiltinType!(void delegate(int)));
}

@safe unittest
{
    import std.traits;

    static if (is(__vector(float[4])))
    {
        alias SimdVec = __vector(float[4]);
        static assert(isSIMDVector!(__vector(float[4])));
        static assert(isSIMDVector!SimdVec);
    }
    static assert(!isSIMDVector!uint);
    static assert(!isSIMDVector!(float[4]));
}

@safe unittest
{
    import std.traits;

    void fun();

    static assert( isPointer!(int*));
    static assert( isPointer!(int function()));
    static assert(!isPointer!int);
    static assert(!isPointer!string);
    static assert(!isPointer!(typeof(null)));
    static assert(!isPointer!(typeof(fun)));
    static assert(!isPointer!(int delegate()));
}

@safe unittest
{
    import std.traits;

    static assert(is(PointerTarget!(int*) == int));
    static assert(is(PointerTarget!(void*) == void));
}

@safe unittest
{
    import std.traits;

    class C {}
    union U {}
    struct S {}
    interface I {}

    static assert( isAggregateType!C);
    static assert( isAggregateType!U);
    static assert( isAggregateType!S);
    static assert( isAggregateType!I);
    static assert(!isAggregateType!void);
    static assert(!isAggregateType!string);
    static assert(!isAggregateType!(int[]));
    static assert(!isAggregateType!(C[string]));
    static assert(!isAggregateType!(void delegate(int)));

    enum ES : S { a = S.init }
    enum EC : C { a = C.init }
    enum EI : I { a = I.init }
    enum EU : U { a = U.init }

    static assert( isAggregateType!ES);
    static assert( isAggregateType!EC);
    static assert( isAggregateType!EI);
    static assert( isAggregateType!EU);
}

@safe unittest
{
    import std.traits;

    struct OpApply
    {
        int opApply(scope int delegate(ref uint) dg) { assert(0); }
    }

    struct Range
    {
        @property uint front() { assert(0); }
        void popFront() { assert(0); }
        enum bool empty = false;
    }

    static assert( isIterable!(uint[]));
    static assert( isIterable!OpApply);
    static assert( isIterable!(uint[string]));
    static assert( isIterable!Range);

    static assert(!isIterable!uint);
}

@safe unittest
{
    import std.traits;

    static assert( isMutable!int);
    static assert( isMutable!string);
    static assert( isMutable!(shared int));
    static assert( isMutable!(shared const(int)[]));

    static assert(!isMutable!(const int));
    static assert(!isMutable!(inout int));
    static assert(!isMutable!(shared(const int)));
    static assert(!isMutable!(shared(inout int)));
    static assert(!isMutable!(immutable string));
}

@safe unittest
{
    import std.traits;

    static struct Foo(T...) { }
    static struct Bar(T...) { }
    static struct Doo(T) { }
    static struct ABC(int x) { }
    static void fun(T)() { }
    template templ(T) { }

    static assert(isInstanceOf!(Foo, Foo!int));
    static assert(!isInstanceOf!(Foo, Bar!int));
    static assert(!isInstanceOf!(Foo, int));
    static assert(isInstanceOf!(Doo, Doo!int));
    static assert(isInstanceOf!(ABC, ABC!1));
    static assert(!isInstanceOf!(Foo, Foo));
    static assert(isInstanceOf!(fun, fun!int));
    static assert(isInstanceOf!(templ, templ!int));
}

@safe unittest
{
    import std.traits;

    static struct A(T = void)
    {
        // doesn't work as expected, only accepts A when T = void
        void func(B)(B b)
        if (isInstanceOf!(A, B)) {}

        // correct behavior
        void method(B)(B b)
        if (isInstanceOf!(TemplateOf!(A), B)) {}
    }

    A!(void) a1;
    A!(void) a2;
    A!(int) a3;

    static assert(!__traits(compiles, a1.func(a3)));
    static assert( __traits(compiles, a1.method(a2)));
    static assert( __traits(compiles, a1.method(a3)));
}

@safe unittest
{
    import std.traits;

    static assert(isExpressions!(1, 2.0, "a"));
    static assert(!isExpressions!(int, double, string));
    static assert(!isExpressions!(int, 2.0, "a"));
}

@safe unittest
{
    import std.traits;

    static assert(isTypeTuple!(int, float, string));
    static assert(!isTypeTuple!(1, 2.0, "a"));
    static assert(!isTypeTuple!(1, double, string));
}

@safe unittest
{
    import std.traits;

    static void foo() {}
    void bar() {}

    auto fpfoo = &foo;
    static assert( isFunctionPointer!fpfoo);
    static assert( isFunctionPointer!(void function()));

    auto dgbar = &bar;
    static assert(!isFunctionPointer!dgbar);
    static assert(!isFunctionPointer!(void delegate()));
    static assert(!isFunctionPointer!foo);
    static assert(!isFunctionPointer!bar);

    static assert( isFunctionPointer!((int a) {}));
}

@safe unittest
{
    import std.traits;

    static void sfunc() { }
    int x;
    void func() { x++; }

    int delegate() dg;
    assert(isDelegate!dg);
    assert(isDelegate!(int delegate()));
    assert(isDelegate!(typeof(&func)));

    int function() fp;
    assert(!isDelegate!fp);
    assert(!isDelegate!(int function()));
    assert(!isDelegate!(typeof(&sfunc)));
}

@safe unittest
{
    import std.traits;

    static real func(ref int) { return 0; }
    static void prop() @property { }
    class C
    {
        real method(ref int) { return 0; }
        real prop() @property { return 0; }
    }
    auto c = new C;
    auto fp = &func;
    auto dg = &c.method;

    static assert( isSomeFunction!func);
    static assert( isSomeFunction!prop);
    static assert( isSomeFunction!(C.method));
    static assert( isSomeFunction!(C.prop));
    static assert( isSomeFunction!(c.prop));
    static assert( isSomeFunction!fp);
    static assert( isSomeFunction!dg);

    real val;
    static assert(!isSomeFunction!int);
    static assert(!isSomeFunction!val);
}

@safe unittest
{
    import std.traits;

    void f() { }
    int g(int x) { return x; }

    static assert( isCallable!f);
    static assert( isCallable!g);

    class C { int opCall(int) { return 0; } }
    auto c = new C;
    struct S { static int opCall(int) { return 0; } }
    interface I { real value() @property; }

    static assert( isCallable!c);
    static assert( isCallable!(c.opCall));
    static assert( isCallable!S);
    static assert( isCallable!(I.value));
    static assert( isCallable!((int a) { return a; }));

    static assert(!isCallable!I);
}

@safe unittest
{
    import std.traits;

    void f()() { }
    T g(T = int)(T x) { return x; }
    struct S1 { static void opCall()() { } }
    struct S2 { static T opCall(T = int)(T x) {return x; } }

    static assert( isCallable!f);
    static assert( isCallable!g);
    static assert( isCallable!S1);
    static assert( isCallable!S2);
}

@safe unittest
{
    import std.traits;

    static struct Wrapper
    {
        void f() { }
        int f(int x) { return x; }

        void g()() { }
        T g(T = int)(T x) { return x; }
    }

    static assert(isCallable!(Wrapper.f));
    static assert(isCallable!(Wrapper.g));
}

@safe unittest
{
    import std.traits;

    struct S { void foo() { } }
    class C { void foo() { } }
    class AC { abstract void foo(); }
    static assert(!isAbstractFunction!(int));
    static assert(!isAbstractFunction!(S.foo));
    static assert(!isAbstractFunction!(C.foo));
    static assert( isAbstractFunction!(AC.foo));
}

@safe unittest
{
    import std.traits;

    struct S { void bar() { } }
    final class FC { void foo(); }
    class C
    {
        void bar() { }
        final void foo();
    }
    static assert(!isFinalFunction!(int));
    static assert(!isFinalFunction!(S.bar));
    static assert( isFinalFunction!(FC.foo));
    static assert(!isFinalFunction!(C.bar));
    static assert( isFinalFunction!(C.foo));
}

@safe unittest
{
    import std.traits;

    static void f() {}
    static void fun()
    {
        int i;
        int f() { return i; }

        static assert(isNestedFunction!(f));
    }

    static assert(!isNestedFunction!f);
}

@safe unittest
{
    import std.traits;

    struct S { }
    class C { }
    abstract class AC { }
    static assert(!isAbstractClass!S);
    static assert(!isAbstractClass!C);
    static assert( isAbstractClass!AC);
    C c;
    static assert(!isAbstractClass!c);
    AC ac;
    static assert( isAbstractClass!ac);
}

@safe unittest
{
    import std.traits;

    class C { }
    abstract class AC { }
    final class FC1 : C { }
    final class FC2 { }
    static assert(!isFinalClass!C);
    static assert(!isFinalClass!AC);
    static assert( isFinalClass!FC1);
    static assert( isFinalClass!FC2);
    C c;
    static assert(!isFinalClass!c);
    FC1 fc1;
    static assert( isFinalClass!fc1);
}

@safe unittest
{
    import std.traits;

    static assert(is(Unconst!int == int));
    static assert(is(Unconst!(const int) == int));
    static assert(is(Unconst!(immutable int) == int));
    static assert(is(Unconst!(shared int) == shared int));
    static assert(is(Unconst!(shared(const int)) == shared int));
}

@safe unittest
{
    import std.traits;

    static assert(is(Unshared!int == int));
    static assert(is(Unshared!(const int) == const int));
    static assert(is(Unshared!(immutable int) == immutable int));

    static assert(is(Unshared!(shared int) == int));
    static assert(is(Unshared!(shared(const int)) == const int));

    static assert(is(Unshared!(shared(int[])) == shared(int)[]));
}

@safe unittest
{
    import std.traits;

    static assert(is(Unqual!int == int));
    static assert(is(Unqual!(const int) == int));
    static assert(is(Unqual!(immutable int) == int));
    static assert(is(Unqual!(shared int) == int));
    static assert(is(Unqual!(shared(const int)) == int));
}

@safe unittest
{
    import std.traits;

    static assert(is(CopyTypeQualifiers!(inout const real, int) == inout const int));
}

@safe unittest
{
    import std.traits;

    const(int) i;
    CopyConstness!(typeof(i), float) f;
    assert( is(typeof(f) == const float));

    CopyConstness!(char, uint) u;
    assert( is(typeof(u) == uint));

    //The 'shared' qualifier will not be copied
    assert(!is(CopyConstness!(shared bool, int) == shared int));

    //But the constness will be
    assert( is(CopyConstness!(shared const real, double) == const double));

    //Careful, const(int)[] is a mutable array of const(int)
    alias MutT = CopyConstness!(const(int)[], int);
    assert(!is(MutT == const(int)));

    //Okay, const(int[]) applies to array and contained ints
    alias CstT = CopyConstness!(const(int[]), int);
    assert( is(CstT == const(int)));
}

@safe unittest
{
    import std.traits;

    static assert(is(ForeachType!(uint[]) == uint));
    static assert(is(ForeachType!string == immutable(char)));
    static assert(is(ForeachType!(string[string]) == string));
    static assert(is(ForeachType!(inout(int)[]) == inout(int)));
}

@safe unittest
{
    import std.traits;

    enum E : real { a = 0 } // NOTE: explicit initialization to 0 required during Enum init deprecation cycle
    enum F : E    { a = E.a }
    alias G = const(F);
    static assert(is(OriginalType!E == real));
    static assert(is(OriginalType!F == real));
    static assert(is(OriginalType!G == const real));
}

@safe unittest
{
    import std.traits;

    alias Hash = int[string];
    static assert(is(KeyType!Hash == string));
    static assert(is(ValueType!Hash == int));
    KeyType!Hash str = "a"; // str is declared as string
    ValueType!Hash num = 1; // num is declared as int
}

@safe unittest
{
    import std.traits;

    alias Hash = int[string];
    static assert(is(KeyType!Hash == string));
    static assert(is(ValueType!Hash == int));
    KeyType!Hash str = "a"; // str is declared as string
    ValueType!Hash num = 1; // num is declared as int
}

@safe unittest
{
    import std.traits;

    static assert(is(Unsigned!(int) == uint));
    static assert(is(Unsigned!(long) == ulong));
    static assert(is(Unsigned!(const short) == const ushort));
    static assert(is(Unsigned!(immutable byte) == immutable ubyte));
    static assert(is(Unsigned!(inout int) == inout uint));
}

@safe unittest
{
    import std.traits;

    static assert(is(Unsigned!(uint) == uint));
    static assert(is(Unsigned!(const uint) == const uint));

    static assert(is(Unsigned!(ubyte) == ubyte));
    static assert(is(Unsigned!(immutable uint) == immutable uint));
}

@safe unittest
{
    import std.traits;

    static assert(is(Largest!(uint, ubyte, ushort, real) == real));
    static assert(is(Largest!(ulong, double) == ulong));
    static assert(is(Largest!(double, ulong) == double));
    static assert(is(Largest!(uint, byte, double, short) == double));
    static if (is(ucent))
        static assert(is(Largest!(uint, ubyte, ucent, ushort) == ucent));
}

@safe unittest
{
    import std.traits;

    alias S1 = Signed!uint;
    static assert(is(S1 == int));
    alias S2 = Signed!(const(uint));
    static assert(is(S2 == const(int)));
    alias S3 = Signed!(immutable(uint));
    static assert(is(S3 == immutable(int)));
    static if (is(ucent))
    {
        alias S4 = Signed!ucent;
        static assert(is(S4 == cent));
    }
}

@safe unittest
{
    import std.traits;

    static assert(mostNegative!float == -float.max);
    static assert(mostNegative!double == -double.max);
    static assert(mostNegative!real == -real.max);
    static assert(mostNegative!bool == false);
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(bool, byte, short, int, long))
        static assert(mostNegative!T == T.min);

    static foreach (T; AliasSeq!(ubyte, ushort, uint, ulong, char, wchar, dchar))
        static assert(mostNegative!T == 0);
}

@safe unittest
{
    import std.traits;

    ubyte a = 3, b = 5;
    static assert(is(typeof(a * b) == Promoted!ubyte));
    static assert(is(Promoted!ubyte == int));

    static assert(is(Promoted!(shared(bool)) == shared(int)));
    static assert(is(Promoted!(const(int)) == const(int)));
    static assert(is(Promoted!double == double));
}

@safe unittest
{
    import std.traits;

    import std.meta : AliasSeq;
    alias TL = staticMap!(mangledName, int, const int, immutable int);
    static assert(TL == AliasSeq!("i", "xi", "yi"));
}

@safe unittest
{
    import std.traits;

    // can select types
    static assert(is(Select!(true, int, long) == int));
    static assert(is(Select!(false, int, long) == long));
    static struct Foo {}
    static assert(is(Select!(false, const(int), const(Foo)) == const(Foo)));

    // can select symbols
    int a = 1;
    int b = 2;
    alias selA = Select!(true, a, b);
    alias selB = Select!(false, a, b);
    assert(selA == 1);
    assert(selB == 2);

    // can select (compile-time) expressions
    enum val = Select!(false, -4, 9 - 6);
    static assert(val == 3);
}

@safe unittest
{
    import std.traits;

    real run() { return 0; }
    int fail() { assert(0); }
    auto a = select!true(run(), fail());
    auto b = select!false(fail(), run());
    static assert(is(typeof(a) == real));
    static assert(is(typeof(b) == real));
}

@safe unittest
{
    import std.traits;

    enum E;
    struct S {}

    @("alpha") int a;
    static assert(hasUDA!(a, "alpha"));
    static assert(!hasUDA!(a, S));
    static assert(!hasUDA!(a, E));

    @(E) int b;
    static assert(!hasUDA!(b, "alpha"));
    static assert(!hasUDA!(b, S));
    static assert(hasUDA!(b, E));

    @E int c;
    static assert(!hasUDA!(c, "alpha"));
    static assert(!hasUDA!(c, S));
    static assert(hasUDA!(c, E));

    @(S, E) int d;
    static assert(!hasUDA!(d, "alpha"));
    static assert(hasUDA!(d, S));
    static assert(hasUDA!(d, E));

    @S int e;
    static assert(!hasUDA!(e, "alpha"));
    static assert(hasUDA!(e, S));
    static assert(!hasUDA!(e, S()));
    static assert(!hasUDA!(e, E));

    @S() int f;
    static assert(!hasUDA!(f, "alpha"));
    static assert(hasUDA!(f, S));
    static assert(hasUDA!(f, S()));
    static assert(!hasUDA!(f, E));

    @(S, E, "alpha") int g;
    static assert(hasUDA!(g, "alpha"));
    static assert(hasUDA!(g, S));
    static assert(hasUDA!(g, E));

    @(100) int h;
    static assert(hasUDA!(h, 100));

    struct Named { string name; }

    @Named("abc") int i;
    static assert(hasUDA!(i, Named));
    static assert(hasUDA!(i, Named("abc")));
    static assert(!hasUDA!(i, Named("def")));

    struct AttrT(T)
    {
        string name;
        T value;
    }

    @AttrT!int("answer", 42) int j;
    static assert(hasUDA!(j, AttrT));
    static assert(hasUDA!(j, AttrT!int));
    static assert(!hasUDA!(j, AttrT!string));

    @AttrT!string("hello", "world") int k;
    static assert(hasUDA!(k, AttrT));
    static assert(!hasUDA!(k, AttrT!int));
    static assert(hasUDA!(k, AttrT!string));

    struct FuncAttr(alias f) { alias func = f; }
    static int fourtyTwo() { return 42; }
    static size_t getLen(string s) { return s.length; }

    @FuncAttr!getLen int l;
    static assert(hasUDA!(l, FuncAttr));
    static assert(!hasUDA!(l, FuncAttr!fourtyTwo));
    static assert(hasUDA!(l, FuncAttr!getLen));
    static assert(!hasUDA!(l, FuncAttr!fourtyTwo()));
    static assert(!hasUDA!(l, FuncAttr!getLen()));

    @FuncAttr!getLen() int m;
    static assert(hasUDA!(m, FuncAttr));
    static assert(!hasUDA!(m, FuncAttr!fourtyTwo));
    static assert(hasUDA!(m, FuncAttr!getLen));
    static assert(!hasUDA!(m, FuncAttr!fourtyTwo()));
    static assert(hasUDA!(m, FuncAttr!getLen()));
}

@safe unittest
{
    import std.traits;

    struct Attr
    {
        string name;
        int value;
    }

    @Attr("Answer", 42) int a;
    static assert(getUDAs!(a, Attr).length == 1);
    static assert(getUDAs!(a, Attr)[0].name == "Answer");
    static assert(getUDAs!(a, Attr)[0].value == 42);

    @(Attr("Answer", 42), "string", 9999) int b;
    static assert(getUDAs!(b, Attr).length == 1);
    static assert(getUDAs!(b, Attr)[0].name == "Answer");
    static assert(getUDAs!(b, Attr)[0].value == 42);

    @Attr("Answer", 42) @Attr("Pi", 3) int c;
    static assert(getUDAs!(c, Attr).length == 2);
    static assert(getUDAs!(c, Attr)[0].name == "Answer");
    static assert(getUDAs!(c, Attr)[0].value == 42);
    static assert(getUDAs!(c, Attr)[1].name == "Pi");
    static assert(getUDAs!(c, Attr)[1].value == 3);

    static assert(getUDAs!(c, Attr("Answer", 42)).length == 1);
    static assert(getUDAs!(c, Attr("Answer", 42))[0].name == "Answer");
    static assert(getUDAs!(c, Attr("Answer", 42))[0].value == 42);

    static assert(getUDAs!(c, Attr("Answer", 99)).length == 0);

    struct AttrT(T)
    {
        string name;
        T value;
    }

    @AttrT!uint("Answer", 42) @AttrT!int("Pi", 3) @AttrT int d;
    static assert(getUDAs!(d, AttrT).length == 2);
    static assert(getUDAs!(d, AttrT)[0].name == "Answer");
    static assert(getUDAs!(d, AttrT)[0].value == 42);
    static assert(getUDAs!(d, AttrT)[1].name == "Pi");
    static assert(getUDAs!(d, AttrT)[1].value == 3);

    static assert(getUDAs!(d, AttrT!uint).length == 1);
    static assert(getUDAs!(d, AttrT!uint)[0].name == "Answer");
    static assert(getUDAs!(d, AttrT!uint)[0].value == 42);

    static assert(getUDAs!(d, AttrT!int).length == 1);
    static assert(getUDAs!(d, AttrT!int)[0].name == "Pi");
    static assert(getUDAs!(d, AttrT!int)[0].value == 3);

    struct SimpleAttr {}

    @SimpleAttr int e;
    static assert(getUDAs!(e, SimpleAttr).length == 1);
    static assert(is(getUDAs!(e, SimpleAttr)[0] == SimpleAttr));

    @SimpleAttr() int f;
    static assert(getUDAs!(f, SimpleAttr).length == 1);
    static assert(is(typeof(getUDAs!(f, SimpleAttr)[0]) == SimpleAttr));

    struct FuncAttr(alias f) { alias func = f; }
    static int add42(int v) { return v + 42; }
    static string concat(string l, string r) { return l ~ r; }

    @FuncAttr!add42 int g;
    static assert(getUDAs!(g, FuncAttr).length == 1);
    static assert(getUDAs!(g, FuncAttr)[0].func(5) == 47);

    static assert(getUDAs!(g, FuncAttr!add42).length == 1);
    static assert(getUDAs!(g, FuncAttr!add42)[0].func(5) == 47);

    static assert(getUDAs!(g, FuncAttr!add42()).length == 0);

    static assert(getUDAs!(g, FuncAttr!concat).length == 0);
    static assert(getUDAs!(g, FuncAttr!concat()).length == 0);

    @FuncAttr!add42() int h;
    static assert(getUDAs!(h, FuncAttr).length == 1);
    static assert(getUDAs!(h, FuncAttr)[0].func(5) == 47);

    static assert(getUDAs!(h, FuncAttr!add42).length == 1);
    static assert(getUDAs!(h, FuncAttr!add42)[0].func(5) == 47);

    static assert(getUDAs!(h, FuncAttr!add42()).length == 1);
    static assert(getUDAs!(h, FuncAttr!add42())[0].func(5) == 47);

    static assert(getUDAs!(h, FuncAttr!concat).length == 0);
    static assert(getUDAs!(h, FuncAttr!concat()).length == 0);

    @("alpha") @(42) int i;
    static assert(getUDAs!(i, "alpha").length == 1);
    static assert(getUDAs!(i, "alpha")[0] == "alpha");

    static assert(getUDAs!(i, 42).length == 1);
    static assert(getUDAs!(i, 42)[0] == 42);

    static assert(getUDAs!(i, 'c').length == 0);
}

@safe unittest
{
    import std.traits;

    enum Attr;
    struct A
    {
        @Attr int a;
        int b;
    }

    static assert(getSymbolsByUDA!(A, Attr).length == 1);
    static assert(hasUDA!(getSymbolsByUDA!(A, Attr)[0], Attr));
}

@safe unittest
{
    import std.traits;

    enum Attr;

    static struct A
    {
        @Attr int a;
        int b;
        @Attr void doStuff() {}
        void doOtherStuff() {}
        static struct Inner
        {
            // Not found by getSymbolsByUDA
            @Attr int c;
        }
    }

    // Finds both variables and functions with the attribute, but
    // doesn't include the variables and functions without it.
    static assert(getSymbolsByUDA!(A, Attr).length == 2);
    // Can access attributes on the symbols returned by getSymbolsByUDA.
    static assert(hasUDA!(getSymbolsByUDA!(A, Attr)[0], Attr));
    static assert(hasUDA!(getSymbolsByUDA!(A, Attr)[1], Attr));
}

@safe unittest
{
    import std.traits;

    static struct UDA { string name; }

    static struct B
    {
        @UDA("X")
        int x;
        @UDA("Y")
        int y;
        @(100)
        int z;
    }

    // Finds both UDA attributes.
    static assert(getSymbolsByUDA!(B, UDA).length == 2);
    // Finds one `100` attribute.
    static assert(getSymbolsByUDA!(B, 100).length == 1);
    // Can get the value of the UDA from the return value
    static assert(getUDAs!(getSymbolsByUDA!(B, UDA)[0], UDA)[0].name == "X");
}

@safe unittest
{
    import std.traits;

    static struct UDA { string name; }

    @UDA("A")
    static struct C
    {
        @UDA("B")
        int d;
    }

    static assert(getSymbolsByUDA!(C, UDA).length == 2);
    static assert(getSymbolsByUDA!(C, UDA)[0].stringof == "C");
    static assert(getSymbolsByUDA!(C, UDA)[1].stringof == "d");
}

@safe unittest
{
    import std.traits;

    static struct UDA { string name; }

    static struct D
    {
        int x;
    }

    static assert(getSymbolsByUDA!(D, UDA).length == 0);
}

@safe unittest
{
    import std.traits;

    static assert(allSameType!());
    static assert(allSameType!(int));
    static assert(allSameType!(int, int));
    static assert(allSameType!(int, int, int));
    static assert(allSameType!(float, float, float));
    static assert(!allSameType!(int, double));
    static assert(!allSameType!(int, float, double));
    static assert(!allSameType!(int, float, double, real));
    static assert(!allSameType!(short, int, float, double, real));
}

@safe unittest
{
    import std.traits;

    class C;
    struct S1;
    struct S2
    {
        T opCast(T)() const;
    }

    static assert( ifTestable!bool);
    static assert( ifTestable!int);
    static assert( ifTestable!(S1*));
    static assert( ifTestable!(typeof(null)));
    static assert( ifTestable!(int[]));
    static assert( ifTestable!(int[string]));
    static assert( ifTestable!S2);
    static assert( ifTestable!C);
    static assert(!ifTestable!S1);
}

@safe unittest
{
    import std.traits;

    struct S {
        template Test() {}
    }
    class C {}
    interface I {}
    union U {}
    static assert(isType!int);
    static assert(isType!string);
    static assert(isType!(int[int]));
    static assert(isType!S);
    static assert(isType!C);
    static assert(isType!I);
    static assert(isType!U);

    int n;
    void func(){}
    static assert(!isType!n);
    static assert(!isType!func);
    static assert(!isType!(S.Test));
    static assert(!isType!(S.Test!()));
}

@safe unittest
{
    import std.traits;

    static void func(){}
    static assert(isFunction!func);

    struct S
    {
        void func(){}
    }
    static assert(isFunction!(S.func));
}

@safe unittest
{
    import std.traits;

    class C
    {
        void nf() {}
        static void sf() {}
        final void ff() {}
    }
    final class FC { }

    static assert(!isFinal!(C));
    static assert( isFinal!(FC));

    static assert(!isFinal!(C.nf));
    static assert(!isFinal!(C.sf));
    static assert( isFinal!(C.ff));
}

@safe unittest
{
    import std.traits;

    struct S1 {}                        // Fine. Can be copied
    struct S2 {         this(this) {}}  // Fine. Can be copied
    struct S3 {@disable this(this);  }  // Not fine. Copying is disabled.
    struct S4 {S3 s;}                   // Not fine. A field has copying disabled.

    class C1 {}

    static assert( isCopyable!S1);
    static assert( isCopyable!S2);
    static assert(!isCopyable!S3);
    static assert(!isCopyable!S4);

    static assert(isCopyable!C1);
    static assert(isCopyable!int);
    static assert(isCopyable!(int[]));
}


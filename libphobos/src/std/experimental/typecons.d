// Written in the D programming language.

/**
This module implements experimental additions/modifications to $(MREF std, _typecons).

Use this module to test out new functionality for $(REF wrap, std, _typecons)
which allows for a struct to be wrapped against an interface; the
implementation in $(MREF std, _typecons) only allows for classes to use the wrap
functionality.

Source:    $(PHOBOSSRC std/experimental/_typecons.d)

Copyright: Copyright the respective authors, 2008-
License:   $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP erdani.org, Andrei Alexandrescu),
           $(HTTP bartoszmilewski.wordpress.com, Bartosz Milewski),
           Don Clugston,
           Shin Fujishiro,
           Kenji Hara
 */
module std.experimental.typecons;

import std.meta; // : AliasSeq, allSatisfy;
import std.traits;

import std.typecons : Tuple, tuple, Bind, DerivedFunctionType,
       isImplicitlyConvertible, mixinAll, staticIota,
       GetOverloadedMethods;

private
{
    pragma(mangle, "_d_toObject")
    extern(C) pure nothrow Object typecons_d_toObject(void* p);
}

/*
 * Avoids opCast operator overloading.
 */
private template dynamicCast(T)
if (is(T == class) || is(T == interface))
{
    @trusted
    T dynamicCast(S)(inout S source)
    if (is(S == class) || is(S == interface))
    {
        static if (is(Unqual!S : Unqual!T))
        {
            import std.traits : QualifierOf;
            alias Qual = QualifierOf!S; // SharedOf or MutableOf
            alias TmpT = Qual!(Unqual!T);
            inout(TmpT) tmp = source;   // bypass opCast by implicit conversion
            return *cast(T*)(&tmp);     // + variable pointer cast + dereference
        }
        else
        {
            return cast(T) typecons_d_toObject(*cast(void**)(&source));
        }
    }
}

@system unittest
{
    class C { @disable opCast(T)() {} }
    auto c = new C;
    static assert(!__traits(compiles, cast(Object) c));
    auto o = dynamicCast!Object(c);
    assert(c is o);

    interface I { @disable opCast(T)() {} Object instance(); }
    interface J { @disable opCast(T)() {} Object instance(); }
    class D : I, J { Object instance() { return this; } }
    I i = new D();
    static assert(!__traits(compiles, cast(J) i));
    J j = dynamicCast!J(i);
    assert(i.instance() is j.instance());
}

/*
 * Determines if the `Source` type satisfies all interface requirements of
 * `Targets`.
 */
private template implementsInterface(Source, Targets...)
if (Targets.length >= 1 && allSatisfy!(isMutable, Targets))
{
    import std.meta : staticMap;

    // strict upcast
    bool implementsInterface()()
    if (Targets.length == 1 && is(Source : Targets[0]))
    {
        return true;
    }
    // structural upcast
    template implementsInterface()
    if (!allSatisfy!(Bind!(isImplicitlyConvertible, Source), Targets))
    {
        auto implementsInterface()
        {
            return hasRequiredMethods!();
        }

        // list of FuncInfo
        alias TargetMembers = UniqMembers!(ConcatInterfaceMembers!Targets);
        // list of function symbols
        alias SourceMembers = GetOverloadedMethods!Source;

        // Check whether all of SourceMembers satisfy covariance target in
        // TargetMembers
        template hasRequiredMethods(size_t i = 0)
        {
            static if (i >= TargetMembers.length)
                enum hasRequiredMethods = true;
            else
            {
                enum foundFunc = findCovariantFunction!(TargetMembers[i], Source, SourceMembers);
                debug
                {
                    static if (foundFunc == -1)
                        pragma(msg, "Could not locate matching function for: ",
                               TargetMembers[i].stringof);
                }
                enum hasRequiredMethods =
                    foundFunc != -1 &&
                    hasRequiredMethods!(i + 1);
            }
        }
    }
}
// ditto
private template implementsInterface(Source, Targets...)
if (Targets.length >= 1 && !allSatisfy!(isMutable, Targets))
{
    import std.meta : staticMap;

    alias implementsInterface = .implementsInterface!(Source, staticMap!(Unqual, Targets));
}

@safe unittest
{
    interface Foo {
        void foo();
    }
    interface Bar {
        void bar();
    }
    interface FooBar : Foo, Bar {
        void foobar();
    }

    struct A {
        void foo() {}
    }
    struct B {
        void bar() {}
        void foobar() {}
    }
    class C {
        void foo() {}
        void bar() {}
    }
    struct D {
        void foo() {}
        void bar() {}
        void foobar() {}
    }
    // Implements interface
    static assert(implementsInterface!(A, Foo));
    static assert(implementsInterface!(A, const(Foo)));
    static assert(implementsInterface!(A, immutable(Foo)));
    // Doesn't implement interface
    static assert(!implementsInterface!(B, Foo));
    static assert(implementsInterface!(B, Bar));
    // Implements both interfaces
    static assert(implementsInterface!(C, Foo));
    static assert(implementsInterface!(C, Bar));
    static assert(implementsInterface!(C, Foo, Bar));
    static assert(implementsInterface!(C, Foo, const(Bar)));
    static assert(!implementsInterface!(A, Foo, Bar));
    static assert(!implementsInterface!(A, Foo, immutable(Bar)));
    // Implements inherited
    static assert(implementsInterface!(D, FooBar));
    static assert(!implementsInterface!(B, FooBar));
}

private enum isInterface(ConceptType) = is(ConceptType == interface);

///
template wrap(Targets...)
if (Targets.length >= 1 && allSatisfy!(isInterface, Targets))
{
    import std.meta : ApplyLeft, staticMap;

    version (StdDdoc)
    {
        /**
         * Wrap src in an anonymous class implementing $(D_PARAM Targets).
         *
         * wrap creates an internal wrapper class which implements the
         * interfaces in `Targets` using the methods of `src`, then returns a
         * GC-allocated instance of it.
         *
         * $(D_PARAM Source) can be either a `class` or a `struct`, but it must
         * $(I structurally conform) with all the $(D_PARAM Targets)
         * interfaces; i.e. it must provide concrete methods with compatible
         * signatures of those in $(D_PARAM Targets).
         *
         * If $(D_PARAM Source) is a `struct` then wrapping/unwrapping will
         * create a copy; it is not possible to affect the original `struct`
         * through the wrapper.
         *
         * The returned object additionally supports $(LREF unwrap).
         *
         * Note:
         * If $(D_PARAM Targets) has only one entry and $(D_PARAM Source) is a
         * class which explicitly implements it, wrap simply returns src
         * upcasted to `Targets[0]`.
         *
         * Bugs:
         * wrap does not support interfaces which take their own type as either
         * a parameter type or return type in any of its methods.
         *
         * See_Also: $(LREF unwrap) for examples
         */
        auto wrap(Source)(inout Source src)
            if (implementsInterface!(Source, Targets));
    }

    static if (!allSatisfy!(isMutable, Targets))
        alias wrap = .wrap!(staticMap!(Unqual, Targets));
    else
    {
        // strict upcast
        auto wrap(Source)(inout Source src)
        if (Targets.length == 1 && is(Source : Targets[0]))
        {
            alias T = Select!(is(Source == shared), shared Targets[0], Targets[0]);
            return dynamicCast!(inout T)(src);
        }

        // structural upcast
        template wrap(Source)
        if (!allSatisfy!(ApplyLeft!(isImplicitlyConvertible, Source), Targets))
        {
            auto wrap(inout Source src)
            {
                static assert(implementsInterface!(Source, Targets),
                              "Source "~Source.stringof~
                              " does not have structural conformance to "~
                              Targets.stringof);

                alias T = Select!(is(Source == shared), shared Impl, Impl);
                return new inout T(src);
            }

            // list of FuncInfo
            alias TargetMembers = UniqMembers!(ConcatInterfaceMembers!(Targets));
            // list of function symbols
            alias SourceMembers = GetOverloadedMethods!Source;

            static if (is(Source == class) || is(Source == interface))
                alias StructuralType = Object;
            else static if (is(Source == struct))
                alias StructuralType = Source;

            // Check whether all of SourceMembers satisfy covariance target in TargetMembers
            // Internal wrapper class
            final class Impl : Structural!StructuralType, Targets
            {
            private:
                Source _wrap_source;

                this(       inout Source s)        inout @safe pure nothrow { _wrap_source = s; }
                this(shared inout Source s) shared inout @safe pure nothrow { _wrap_source = s; }

                static if (is(Source == class) || is(Source == interface))
                {
                    // BUG: making private should work with NVI.
                    protected inout(Object) _wrap_getSource() inout @safe
                    {
                        return dynamicCast!(inout Object)(_wrap_source);
                    }
                }
                else
                {
                    // BUG: making private should work with NVI.
                    protected inout(Source) _wrap_getSource() inout @safe
                    {
                        return _wrap_source;
                    }
                }

                import std.conv : to;
                import std.functional : forward;
                template generateFun(size_t i)
                {
                    enum name = TargetMembers[i].name;
                    enum fa = functionAttributes!(TargetMembers[i].type);
                    static args(int num)()
                    {
                        string r;
                        bool first = true;
                        foreach (i; staticIota!(0, num))
                        {
                            import std.conv : to;
                            r ~= (first ? "" : ", ") ~ " a" ~ (i+1).to!string;
                            first = false;
                        }
                        return r;
                    }
                    static if (fa & FunctionAttribute.property)
                    {
                        static if (Parameters!(TargetMembers[i].type).length == 0)
                            enum fbody = "_wrap_source."~name;
                        else
                            enum fbody = "_wrap_source."~name~" = a1";
                    }
                    else
                    {
                            enum fbody = "_wrap_source."~name~"("~args!(Parameters!(TargetMembers[i].type).length)~")";
                    }
                    enum generateFun =
                        "override "~wrapperSignature!(TargetMembers[i]) ~
                        "{ return "~fbody~"; }";
                }

            public:
                mixin mixinAll!(
                    staticMap!(generateFun, staticIota!(0, TargetMembers.length)));
            }
        }
    }
}

// Build a signature that matches the provided function
// Each argument will be provided a name in the form a#
private template wrapperSignature(alias fun)
{
    enum name = fun.name;
    enum fa = functionAttributes!(fun.type);
    static @property stc()
    {
        string r;
        if (fa & FunctionAttribute.property)    r ~= "@property ";
        if (fa & FunctionAttribute.ref_)        r ~= "ref ";
        if (fa & FunctionAttribute.pure_)       r ~= "pure ";
        if (fa & FunctionAttribute.nothrow_)    r ~= "nothrow ";
        if (fa & FunctionAttribute.trusted)     r ~= "@trusted ";
        if (fa & FunctionAttribute.safe)        r ~= "@safe ";
        return r;
    }
    static @property mod()
    {
        alias type = AliasSeq!(fun.type)[0];
        string r;
        static if (is(type == immutable))       r ~= " immutable";
        else
        {
            static if (is(type == shared))      r ~= " shared";
            static if (is(type == const))       r ~= " const";
            else static if (is(type == inout))  r ~= " inout";
            //else  --> mutable
        }
        return r;
    }
    alias param = Parameters!(fun.type);
    static @property wrapperParameters()
    {
        string r;
        bool first = true;
        foreach (i, p; param)
        {
            import std.conv : to;
            r ~= (first ? "" : ", ") ~ p.stringof ~ " a" ~ (i+1).to!string;
            first = false;
        }
        return r;
    }

    enum wrapperSignature =
        stc~ReturnType!(fun.type).stringof ~ " "
        ~ name~"("~wrapperParameters~")"~mod;
}

@safe unittest
{
    interface M
    {
        void f1();
        void f2(string[] args, int count);
        void f3(string[] args, int count) pure const;
    }

    alias TargetMembers = UniqMembers!(ConcatInterfaceMembers!M);
    static assert(wrapperSignature!(TargetMembers[0]) == "void f1()"
                  , wrapperSignature!(TargetMembers[0]));

    static assert(wrapperSignature!(TargetMembers[1]) == "void f2(string[] a1, int a2)"
                  , wrapperSignature!(TargetMembers[1]));

    static assert(wrapperSignature!(TargetMembers[2]) == "pure void f3(string[] a1, int a2) const"
                  , wrapperSignature!(TargetMembers[2]));
}

// Internal class to support dynamic cross-casting
private interface Structural(T)
{
    inout(T) _wrap_getSource() inout @safe pure nothrow;
}

private string unwrapExceptionText(Source, Target)()
{
    return Target.stringof~ " not wrapped into "~ Source.stringof;
}

version (StdDdoc)
{
    /**
     * Extract object previously wrapped by $(LREF wrap).
     *
     * Params:
     *     Target = type of wrapped object
     *     src = wrapper object returned by $(LREF wrap)
     *
     * Returns: the wrapped object, or null if src is not a wrapper created
     * by $(LREF wrap) and $(D_PARAM Target) is a class
     *
     * Throws: $(REF ConvException, std, conv) when attempting to extract a
     * struct which is not the wrapped type
     *
     * See_also: $(LREF wrap)
     */
    public inout(Target) unwrap(Target, Source)(inout Source src);
}

///
@system unittest
{
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
    struct HumanStructure
    {
        int quack() { return 3; }
        @property int height() { return 30; }
    }

    Duck d1 = new Duck();
    Human h1 = new Human();
    HumanStructure hs1;

    interface Refreshable
    {
        int refresh();
    }
    // does not have structural conformance
    static assert(!__traits(compiles, d1.wrap!Refreshable));
    static assert(!__traits(compiles, h1.wrap!Refreshable));
    static assert(!__traits(compiles, hs1.wrap!Refreshable));

    // strict upcast
    Quack qd = d1.wrap!Quack;
    assert(qd is d1);
    assert(qd.quack() == 1);    // calls Duck.quack
    // strict downcast
    Duck d2 = qd.unwrap!Duck;
    assert(d2 is d1);

    // structural upcast
    Quack qh = h1.wrap!Quack;
    Quack qhs = hs1.wrap!Quack;
    assert(qh.quack() == 2);    // calls Human.quack
    assert(qhs.quack() == 3);    // calls HumanStructure.quack
    // structural downcast
    Human h2 = qh.unwrap!Human;
    HumanStructure hs2 = qhs.unwrap!HumanStructure;
    assert(h2 is h1);
    assert(hs2 is hs1);

    // structural upcast (two steps)
    Quack qx = h1.wrap!Quack;   // Human -> Quack
    Quack qxs = hs1.wrap!Quack;   // HumanStructure -> Quack
    Flyer fx = qx.wrap!Flyer;   // Quack -> Flyer
    Flyer fxs = qxs.wrap!Flyer;   // Quack -> Flyer
    assert(fx.height == 20);    // calls Human.height
    assert(fxs.height == 30);    // calls HumanStructure.height
    // strucural downcast (two steps)
    Quack qy = fx.unwrap!Quack; // Flyer -> Quack
    Quack qys = fxs.unwrap!Quack; // Flyer -> Quack
    Human hy = qy.unwrap!Human; // Quack -> Human
    HumanStructure hys = qys.unwrap!HumanStructure; // Quack -> HumanStructure
    assert(hy is h1);
    assert(hys is hs1);
    // strucural downcast (one step)
    Human hz = fx.unwrap!Human; // Flyer -> Human
    HumanStructure hzs = fxs.unwrap!HumanStructure; // Flyer -> HumanStructure
    assert(hz is h1);
    assert(hzs is hs1);
}

///
@system unittest
{
    import std.traits : functionAttributes, FunctionAttribute;
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

template unwrap(Target)
{
    static if (!isMutable!Target)
        alias unwrap = .unwrap!(Unqual!Target);
    else
    {
        // strict downcast
        auto unwrap(Source)(inout Source src)
        if (is(Target : Source))
        {
            alias T = Select!(is(Source == shared), shared Target, Target);
            return dynamicCast!(inout T)(src);
        }

        // structural downcast for struct target
        auto unwrap(Source)(inout Source src)
        if (is(Target == struct))
        {
            alias T = Select!(is(Source == shared), shared Target, Target);
            auto upCastSource = dynamicCast!Object(src);   // remove qualifier
            do
            {
                if (auto a = dynamicCast!(Structural!Object)(upCastSource))
                {
                    upCastSource = a._wrap_getSource();
                }
                else if (auto a = dynamicCast!(Structural!T)(upCastSource))
                {
                    return a._wrap_getSource();
                }
                else
                {
                    static if (hasMember!(Source, "_wrap_getSource"))
                        return unwrap!Target(src._wrap_getSource());
                    else
                        break;
                }
            } while (upCastSource);
            import std.conv : ConvException;
            throw new ConvException(unwrapExceptionText!(Source,Target));
        }
        // structural downcast for class target
        auto unwrap(Source)(inout Source src)
        if (!is(Target : Source) && !is(Target == struct))
        {
            alias T = Select!(is(Source == shared), shared Target, Target);
            Object upCastSource = dynamicCast!(Object)(src);   // remove qualifier
            do
            {
                // Unwrap classes
                if (auto a = dynamicCast!(Structural!Object)(upCastSource))
                {
                    if (auto d = dynamicCast!(inout T)(upCastSource = a._wrap_getSource()))
                        return d;
                }
                // Unwrap a structure of type T
                else if (auto a = dynamicCast!(Structural!T)(upCastSource))
                {
                    return a._wrap_getSource();
                }
                // Unwrap class that already inherited from interface
                else if (auto d = dynamicCast!(inout T)(upCastSource))
                {
                    return d;
                }
                // Recurse to find the struct Target within a wrapped tree
                else
                {
                    static if (hasMember!(Source, "_wrap_getSource"))
                        return unwrap!Target(src._wrap_getSource());
                    else
                        break;
                }
            } while (upCastSource);
            return null;
        }
    }
}

@system unittest
{
    // Validate const/immutable
    class A
    {
        int draw()              { return 1; }
        int draw(int v)         { return v; }

        int draw() const        { return 2; }
        int draw() shared       { return 3; }
        int draw() shared const { return 4; }
        int draw() immutable    { return 5; }
    }
    interface Drawable
    {
        int draw();
        int draw() const;
        int draw() shared;
        int draw() shared const;
        int draw() immutable;
    }
    interface Drawable2
    {
        int draw(int v);
    }

    auto ma = new A();
    auto sa = new shared A();
    auto ia = new immutable A();
    {
                     Drawable  md = ma.wrap!Drawable;
               const Drawable  cd = ma.wrap!Drawable;
              shared Drawable  sd = sa.wrap!Drawable;
        shared const Drawable scd = sa.wrap!Drawable;
           immutable Drawable  id = ia.wrap!Drawable;
        assert( md.draw() == 1);
        assert( cd.draw() == 2);
        assert( sd.draw() == 3);
        assert(scd.draw() == 4);
        assert( id.draw() == 5);
    }
    {
        Drawable2 d = ma.wrap!Drawable2;
        static assert(!__traits(compiles, d.draw()));
        assert(d.draw(10) == 10);
    }
}
@system unittest
{
    // Bugzilla 10377
    import std.algorithm, std.range;

    interface MyInputRange(T)
    {
        @property T front();
        void popFront();
        @property bool empty();
    }

    //auto o = iota(0,10,1).inputRangeObject();
    //pragma(msg, __traits(allMembers, typeof(o)));
    auto r = iota(0,10,1).inputRangeObject().wrap!(MyInputRange!int)();
    assert(equal(r, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
}
@system unittest
{
    // Bugzilla 10536
    interface Interface
    {
        int foo();
    }
    class Pluggable
    {
        int foo() { return 1; }
        @disable void opCast(T, this X)();  // !
    }

    Interface i = new Pluggable().wrap!Interface;
    assert(i.foo() == 1);
}
@system unittest
{
    // Enhancement 10538
    interface Interface
    {
        int foo();
        int bar(int);
    }
    class Pluggable
    {
        int opDispatch(string name, A...)(A args) { return 100; }
    }

    Interface i = wrap!Interface(new Pluggable());
    assert(i.foo() == 100);
    assert(i.bar(10) == 100);
}

// Concat all Targets function members into one tuple
private template ConcatInterfaceMembers(Targets...)
{
    static if (Targets.length == 0)
        alias ConcatInterfaceMembers = AliasSeq!();
    else static if (Targets.length == 1)
        alias ConcatInterfaceMembers
          = AliasSeq!(GetOverloadedMethods!(Targets[0]));
    else
        alias ConcatInterfaceMembers = AliasSeq!(
                GetOverloadedMethods!(Targets[0]),
                ConcatInterfaceMembers!(Targets[1..$]));
}
// Remove duplicated functions based on the identifier name and function type covariance
private template UniqMembers(members...)
{
    template FuncInfo(string s, F)
    {
        enum name = s;
        alias type = F;
    }

    static if (members.length == 0)
        alias UniqMembers = AliasSeq!();
    else
    {
        alias func = members[0];
        enum  name = __traits(identifier, func);
        alias type = FunctionTypeOf!func;
        template check(size_t i, mem...)
        {
            static if (i >= mem.length)
                enum ptrdiff_t check = -1;
            else static if
              (__traits(identifier, func) == __traits(identifier, mem[i]) &&
              !is(DerivedFunctionType!(type, FunctionTypeOf!(mem[i])) == void))
            {
                enum ptrdiff_t check = i;
            }
            else
                enum ptrdiff_t check = check!(i + 1, mem);
        }
        enum ptrdiff_t x = 1 + check!(0, members[1 .. $]);
        static if (x >= 1)
        {
            alias typex = DerivedFunctionType!(type, FunctionTypeOf!(members[x]));
            alias remain = UniqMembers!(members[1 .. x], members[x + 1 .. $]);

            static if (remain.length >= 1 && remain[0].name == name &&
                       !is(DerivedFunctionType!(typex, remain[0].type) == void))
            {
                alias F = DerivedFunctionType!(typex, remain[0].type);
                alias UniqMembers = AliasSeq!(FuncInfo!(name, F), remain[1 .. $]);
            }
            else
                alias UniqMembers = AliasSeq!(FuncInfo!(name, typex), remain);
        }
        else
        {
            alias UniqMembers = AliasSeq!(FuncInfo!(name, type), UniqMembers!(members[1 .. $]));
        }
    }
}

// find a function from Fs that has same identifier and covariant type with f
private template findCovariantFunction(alias finfo, Source, Fs...)
{
    template check(size_t i = 0)
    {
        static if (i >= Fs.length)
            enum ptrdiff_t check = -1;
        else
        {
            enum ptrdiff_t check =
                (finfo.name == __traits(identifier, Fs[i])) &&
                isCovariantWith!(FunctionTypeOf!(Fs[i]), finfo.type)
              ? i : check!(i + 1);
        }
    }
    enum x = check!();
    static if (x == -1 && is(typeof(Source.opDispatch)))
    {
        alias Params = Parameters!(finfo.type);
        enum ptrdiff_t findCovariantFunction =
            is(typeof((             Source).init.opDispatch!(finfo.name)(Params.init))) ||
            is(typeof((       const Source).init.opDispatch!(finfo.name)(Params.init))) ||
            is(typeof((   immutable Source).init.opDispatch!(finfo.name)(Params.init))) ||
            is(typeof((      shared Source).init.opDispatch!(finfo.name)(Params.init))) ||
            is(typeof((shared const Source).init.opDispatch!(finfo.name)(Params.init)))
          ? ptrdiff_t.max : -1;
    }
    else
        enum ptrdiff_t findCovariantFunction = x;
}

/**
Type constructor for final (aka head-const) variables.

Final variables cannot be directly mutated or rebound, but references
reached through the variable are typed with their original mutability.
It is equivalent to `final` variables in D1 and Java, as well as
`readonly` variables in C#.

When `T` is a `const` or `immutable` type, `Final` aliases
to `T`.
*/
template Final(T)
{
static if (is(T == const) || is(T == immutable))
    alias Final = T;
else
{
    struct Final
    {
        import std.typecons : Proxy;

        private T final_value;
        mixin Proxy!final_value;

        /**
         * Construction is forwarded to the underlying type.
         */
        this(T other)
        {
            this.final_value = other;
        }

        /// Ditto
        this(Args...)(auto ref Args args)
            if (__traits(compiles, T(args)))
        {
            static assert((!is(T == struct) && !is(T == union)) || !isNested!T,
                "Non-static nested type " ~ fullyQualifiedName!T ~ " must be " ~
                "constructed explicitly at the call-site (e.g. auto s = " ~
                "makeFinal(" ~ T.stringof ~ "(...));)");
            this.final_value = T(args);
        }

        // Attaching function attributes gives less noisy error messages
        pure nothrow @safe @nogc
        {
            /++
             + All operators, including member access, are forwarded to the
             + underlying value of type `T` except for these mutating operators,
             + which are disabled.
             +/
            void opAssign(Other)(Other other)
            {
                static assert(0, typeof(this).stringof ~
                                 " cannot be reassigned.");
            }

            /// Ditto
            void opOpAssign(string op, Other)(Other other)
            {
                static assert(0, typeof(this).stringof ~
                                 " cannot be reassigned.");
            }

            /// Ditto
            void opUnary(string op : "--")()
            {
                static assert(0, typeof(this).stringof ~
                                 " cannot be mutated.");
            }

            /// Ditto
            void opUnary(string op : "++")()
            {
                static assert(0, typeof(this).stringof ~
                                 " cannot be mutated.");
            }
        }

        /**
         *
         * `Final!T` implicitly converts to an rvalue of type `T` through
         * `AliasThis`.
         */
        inout(T) final_get() inout
        {
            return final_value;
        }

        /// Ditto
        alias final_get this;

        /// Ditto
        auto ref opUnary(string op)()
            if (__traits(compiles, mixin(op ~ "T.init")))
        {
            return mixin(op ~ "this.final_value");
        }
    }
}
}

/// Ditto
Final!T makeFinal(T)(T t)
{
    return Final!T(t);
}

/// `Final` can be used to create class references which cannot be rebound:
pure nothrow @safe unittest
{
    static class A
    {
        int i;

        this(int i) pure nothrow @nogc @safe
        {
            this.i = i;
        }
    }

    auto a = makeFinal(new A(42));
    assert(a.i == 42);

    //a = new A(24); // Reassignment is illegal,
    a.i = 24; // But fields are still mutable.

    assert(a.i == 24);
}

/// `Final` can also be used to create read-only data fields without using transitive immutability:
pure nothrow @safe unittest
{
    static class A
    {
        int i;

        this(int i) pure nothrow @nogc @safe
        {
            this.i = i;
        }
    }

    static class B
    {
        Final!A a;

        this(A a) pure nothrow @nogc @safe
        {
            this.a = a; // Construction, thus allowed.
        }
    }

    auto b = new B(new A(42));
    assert(b.a.i == 42);

    // b.a = new A(24); // Reassignment is illegal,
    b.a.i = 24; // but `a` is still mutable.

    assert(b.a.i == 24);
}

pure nothrow @safe unittest
{
    static class A { int i; }
    static assert(!is(Final!A == A));
    static assert(is(Final!(const A) == const A));
    static assert(is(Final!(immutable A) == immutable A));

    Final!A a = new A;
    static assert(!__traits(compiles, a = new A));

    static void foo(ref A a) pure nothrow @safe @nogc {}
    static assert(!__traits(compiles, foo(a)));

    assert(a.i == 0);
    a.i = 42;
    assert(a.i == 42);

    Final!int i = 42;
    static assert(!__traits(compiles, i = 24));
    static assert(!__traits(compiles, --i));
    static assert(!__traits(compiles, ++i));
    assert(i == 42);
    int iCopy = i;
    assert(iCopy == 42);
    iCopy = -i; // non-mutating unary operators must work
    assert(iCopy == -42);

    static struct S
    {
        int i;

        pure nothrow @safe @nogc:
        this(int i){}
        this(string s){}
        this(int i, string s, float f){ this.i = i; }
    }

    Final!S sint = 42;
    Final!S sstr = "foo";
    static assert(!__traits(compiles, sint = sstr));

    auto sboth = Final!S(42, "foo", 3.14);
    assert(sboth.i == 42);

    sboth.i = 24;
    assert(sboth.i == 24);

    struct NestedS
    {
        int i;
        int get() pure nothrow @safe @nogc { return sboth.i + i; }
    }

    // Nested structs must be constructed at the call-site
    static assert(!__traits(compiles, Final!NestedS(6)));
    auto s = makeFinal(NestedS(6));
    assert(s.i == 6);
    assert(s.get == 30);

    class NestedC
    {
        int i;

        pure nothrow @safe @nogc:
        this(int i) { this.i = i; }
        int get() { return sboth.i + i; }
    }

    auto c = makeFinal(new NestedC(6));
    assert(c.i == 6);
    assert(c.get == 30);
}

pure nothrow @safe unittest
{
    auto arr = makeFinal([1, 2, 3]);
    static assert(!__traits(compiles, arr = null));
    static assert(!__traits(compiles, arr ~= 4));
    assert((arr ~ 4) == [1, 2, 3, 4]);
}

// issue 17270
pure nothrow @nogc @system unittest
{
    int i = 1;
    Final!(int*) fp = &i;
    assert(*fp == 1);
    static assert(!__traits(compiles,
        fp = &i // direct assignment
    ));
    static assert(is(typeof(*fp) == int));
    *fp = 2; // indirect assignment
    assert(*fp == 2);
    int* p = fp;
    assert(*p == 2);
}

pure nothrow @system unittest
{
    Final!(int[]) arr;
    // static assert(!__traits(compiles,
        // arr.length = 10; // bug!
    // ));
    static assert(!__traits(compiles,
        arr.ptr = null
    ));
    static assert(!__traits(compiles,
        arr.ptr++
    ));
}

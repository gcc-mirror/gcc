// Written in the D programming language.

/**
 * Templates which extract information about types and symbols at compile time.
 *
 * $(SCRIPT inhibitQuickIndex = 1;)
 *
 * $(DIVC quickindex,
 * $(BOOKTABLE ,
 * $(TR $(TH Category) $(TH Templates))
 * $(TR $(TD Symbol Name traits) $(TD
 *           $(LREF fullyQualifiedName)
 *           $(LREF mangledName)
 *           $(LREF moduleName)
 *           $(LREF packageName)
 * ))
 * $(TR $(TD Function traits) $(TD
 *           $(LREF isFunction)
 *           $(LREF arity)
 *           $(LREF functionAttributes)
 *           $(LREF hasFunctionAttributes)
 *           $(LREF functionLinkage)
 *           $(LREF FunctionTypeOf)
 *           $(LREF isSafe)
 *           $(LREF isUnsafe)
 *           $(LREF isFinal)
 *           $(LREF ParameterDefaults)
 *           $(LREF ParameterIdentifierTuple)
 *           $(LREF ParameterStorageClassTuple)
 *           $(LREF Parameters)
 *           $(LREF ReturnType)
 *           $(LREF SetFunctionAttributes)
 *           $(LREF variadicFunctionStyle)
 * ))
 * $(TR $(TD Aggregate Type traits) $(TD
 *           $(LREF BaseClassesTuple)
 *           $(LREF BaseTypeTuple)
 *           $(LREF classInstanceAlignment)
 *           $(LREF EnumMembers)
 *           $(LREF FieldNameTuple)
 *           $(LREF Fields)
 *           $(LREF hasAliasing)
 *           $(LREF hasElaborateAssign)
 *           $(LREF hasElaborateCopyConstructor)
 *           $(LREF hasElaborateDestructor)
 *           $(LREF hasElaborateMove)
 *           $(LREF hasIndirections)
 *           $(LREF hasMember)
 *           $(LREF hasStaticMember)
 *           $(LREF hasNested)
 *           $(LREF hasUnsharedAliasing)
 *           $(LREF InterfacesTuple)
 *           $(LREF isInnerClass)
 *           $(LREF isNested)
 *           $(LREF MemberFunctionsTuple)
 *           $(LREF RepresentationTypeTuple)
 *           $(LREF TemplateArgsOf)
 *           $(LREF TemplateOf)
 *           $(LREF TransitiveBaseTypeTuple)
 * ))
 * $(TR $(TD Type Conversion) $(TD
 *           $(LREF CommonType)
 *           $(LREF AllImplicitConversionTargets)
 *           $(LREF ImplicitConversionTargets)
 *           $(LREF CopyTypeQualifiers)
 *           $(LREF CopyConstness)
 *           $(LREF isAssignable)
 *           $(LREF isCovariantWith)
 *           $(LREF isImplicitlyConvertible)
 * ))
 * $(TR $(TD Type Constructors) $(TD
 *           $(LREF InoutOf)
 *           $(LREF ConstOf)
 *           $(LREF SharedOf)
 *           $(LREF SharedInoutOf)
 *           $(LREF SharedConstOf)
 *           $(LREF SharedConstInoutOf)
 *           $(LREF ImmutableOf)
 *           $(LREF QualifierOf)
 * ))
 * $(TR $(TD Categories of types) $(TD
 *           $(LREF allSameType)
 *           $(LREF ifTestable)
 *           $(LREF isType)
 *           $(LREF isAggregateType)
 *           $(LREF isArray)
 *           $(LREF isAssociativeArray)
 *           $(LREF isAutodecodableString)
 *           $(LREF isBasicType)
 *           $(LREF isBoolean)
 *           $(LREF isBuiltinType)
 *           $(LREF isCopyable)
 *           $(LREF isDynamicArray)
 *           $(LREF isEqualityComparable)
 *           $(LREF isFloatingPoint)
 *           $(LREF isIntegral)
 *           $(LREF isNarrowString)
 *           $(LREF isConvertibleToString)
 *           $(LREF isNumeric)
 *           $(LREF isOrderingComparable)
 *           $(LREF isPointer)
 *           $(LREF isScalarType)
 *           $(LREF isSigned)
 *           $(LREF isSIMDVector)
 *           $(LREF isSomeChar)
 *           $(LREF isSomeString)
 *           $(LREF isStaticArray)
 *           $(LREF isUnsigned)
 * ))
 * $(TR $(TD Type behaviours) $(TD
 *           $(LREF isAbstractClass)
 *           $(LREF isAbstractFunction)
 *           $(LREF isCallable)
 *           $(LREF isDelegate)
 *           $(LREF isExpressions)
 *           $(LREF isFinalClass)
 *           $(LREF isFinalFunction)
 *           $(LREF isFunctionPointer)
 *           $(LREF isInstanceOf)
 *           $(LREF isIterable)
 *           $(LREF isMutable)
 *           $(LREF isSomeFunction)
 *           $(LREF isTypeTuple)
 * ))
 * $(TR $(TD General Types) $(TD
 *           $(LREF ForeachType)
 *           $(LREF KeyType)
 *           $(LREF Largest)
 *           $(LREF mostNegative)
 *           $(LREF OriginalType)
 *           $(LREF PointerTarget)
 *           $(LREF Signed)
 *           $(LREF Unconst)
 *           $(LREF Unqual)
 *           $(LREF Unsigned)
 *           $(LREF ValueType)
 *           $(LREF Promoted)
 * ))
 * $(TR $(TD Misc) $(TD
 *           $(LREF lvalueOf)
 *           $(LREF rvalueOf)
 *           $(LREF Select)
 *           $(LREF select)
 * ))
 * $(TR $(TD User-Defined Attributes) $(TD
 *           $(LREF hasUDA)
 *           $(LREF getUDAs)
 *           $(LREF getSymbolsByUDA)
 * ))
 * )
 * )
 *
 * Copyright: Copyright The D Language Foundation 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright),
 *            Tomasz Stachowiak (`isExpressions`),
 *            $(HTTP erdani.org, Andrei Alexandrescu),
 *            Shin Fujishiro,
 *            $(HTTP octarineparrot.com, Robert Clipsham),
 *            $(HTTP klickverbot.at, David Nadlinger),
 *            Kenji Hara,
 *            Shoichi Kato
 * Source:    $(PHOBOSSRC std/traits.d)
 */
/*          Copyright The D Language Foundation 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.traits;

import std.meta : AliasSeq, allSatisfy, anySatisfy, ApplyLeft;

// Legacy inheritance from std.typetuple
// See also: https://github.com/dlang/phobos/pull/5484#discussion_r122602797
import std.meta : staticMapMeta = staticMap;
// TODO: find a way to trigger deprecation warnings
//deprecated("staticMap is part of std.meta: Please import std.meta")
alias staticMap = staticMapMeta;

///////////////////////////////////////////////////////////////////////////////
// Type lists
///////////////////////////////////////////////////////////////////////////////

private
{
    static if (is(ucent))
    {
        alias CentTypeList         = AliasSeq!(cent, ucent);
        alias SignedCentTypeList   = AliasSeq!(cent);
        alias UnsignedCentTypeList = AliasSeq!(ucent);
    }
    else
    {
        alias CentTypeList         = AliasSeq!();
        alias SignedCentTypeList   = AliasSeq!();
        alias UnsignedCentTypeList = AliasSeq!();
    }

    alias IntegralTypeList      = AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong, CentTypeList);
    alias SignedIntTypeList     = AliasSeq!(byte, short, int, long, SignedCentTypeList);
    alias UnsignedIntTypeList   = AliasSeq!(ubyte, ushort, uint, ulong, UnsignedCentTypeList);
    alias FloatingPointTypeList = AliasSeq!(float, double, real);
    alias ImaginaryTypeList     = AliasSeq!(ifloat, idouble, ireal);
    alias ComplexTypeList       = AliasSeq!(cfloat, cdouble, creal);
    alias NumericTypeList       = AliasSeq!(IntegralTypeList, FloatingPointTypeList);
    alias CharTypeList          = AliasSeq!(char, wchar, dchar);
}

/**
 * Params:
 *     T = The type to qualify
 * Returns:
 *     `T` with the `inout` qualifier added.
 */
alias InoutOf(T) = inout(T);

///
@safe unittest
{
    static assert(is(InoutOf!(int) == inout int));
    static assert(is(InoutOf!(inout int) == inout int));
    static assert(is(InoutOf!(const int) == inout const int));
    static assert(is(InoutOf!(shared int) == inout shared int));
}

/**
 * Params:
 *     T = The type to qualify
 * Returns:
 *     `T` with the `const` qualifier added.
 */
alias ConstOf(T) = const(T);

///
@safe unittest
{
    static assert(is(ConstOf!(int) == const int));
    static assert(is(ConstOf!(const int) == const int));
    static assert(is(ConstOf!(inout int) == const inout int));
    static assert(is(ConstOf!(shared int) == const shared int));
}

/**
 * Params:
 *     T = The type to qualify
 * Returns:
 *     `T` with the `shared` qualifier added.
 */
alias SharedOf(T) = shared(T);

///
@safe unittest
{
    static assert(is(SharedOf!(int) == shared int));
    static assert(is(SharedOf!(shared int) == shared int));
    static assert(is(SharedOf!(inout int) == shared inout int));
    static assert(is(SharedOf!(immutable int) == shared immutable int));
}

/**
 * Params:
 *     T = The type to qualify
 * Returns:
 *     `T` with the `inout` and `shared` qualifiers added.
 */
alias SharedInoutOf(T) = shared(inout(T));

///
@safe unittest
{
    static assert(is(SharedInoutOf!(int) == shared inout int));
    static assert(is(SharedInoutOf!(int) == inout shared int));

    static assert(is(SharedInoutOf!(const int) == shared inout const int));
    static assert(is(SharedInoutOf!(immutable int) == shared inout immutable int));
}

/**
 * Params:
 *     T = The type to qualify
 * Returns:
 *     `T` with the `const` and `shared` qualifiers added.
 */
alias SharedConstOf(T) = shared(const(T));

///
@safe unittest
{
    static assert(is(SharedConstOf!(int) == shared const int));
    static assert(is(SharedConstOf!(int) == const shared int));

    static assert(is(SharedConstOf!(inout int) == shared inout const int));
    // immutable variables are implicitly shared and const
    static assert(is(SharedConstOf!(immutable int) == immutable int));
}

/**
 * Params:
 *     T = The type to qualify
 * Returns:
 *     `T` with the `const`, `shared`, and `inout` qualifiers added.
 */
alias SharedConstInoutOf(T) = shared(const(inout(T)));

///
@safe unittest
{
    static assert(is(SharedConstInoutOf!(int) == shared const inout int));
    static assert(is(SharedConstInoutOf!(int) == const shared inout int));
    static assert(is(SharedConstInoutOf!(inout int) == shared inout const int));
    // immutable variables are implicitly shared and const
    static assert(is(SharedConstInoutOf!(immutable int) == immutable int));
}

/**
 * Params:
 *     T = The type to qualify
 * Returns:
 *     `T` with the `immutable` qualifier added.
 */
alias ImmutableOf(T) = immutable(T);

///
@safe unittest
{
    static assert(is(ImmutableOf!(int) == immutable int));
    static assert(is(ImmutableOf!(const int) == immutable int));
    static assert(is(ImmutableOf!(inout int) == immutable int));
    static assert(is(ImmutableOf!(shared int) == immutable int));
}

@safe unittest
{
    static assert(is(      InoutOf!int ==        inout int));
    static assert(is(      ConstOf!int ==        const int));
    static assert(is(     SharedOf!int == shared       int));
    static assert(is(SharedInoutOf!int == shared inout int));
    static assert(is(SharedConstOf!int == shared const int));
    static assert(is(  ImmutableOf!int ==    immutable int));
}

/**
 * Gives a template that can be used to apply the same
 * attributes that are on the given type `T`. E.g. passing
 * `inout shared int` will return `SharedInoutOf`.
 *
 * Params:
 *     T = the type to check qualifiers from
 * Returns:
 *     The qualifier template from the given type `T`
 */
template QualifierOf(T)
{
    static if (is(immutable T == T))
    {
        alias QualifierOf = ImmutableOf;
    }
    else
    {
        private enum quals = is(const T == T) | (is(inout T == T) << 1) | (is(shared T == T) << 2);
        static if (quals == 0)      { import std.meta : Alias; alias QualifierOf = Alias; }
        else static if (quals == 1) alias QualifierOf = ConstOf;
        else static if (quals == 2) alias QualifierOf = InoutOf;
        else static if (quals == 3) alias QualifierOf = ConstInoutOf;
        else static if (quals == 4) alias QualifierOf = SharedOf;
        else static if (quals == 5) alias QualifierOf = SharedConstOf;
        else static if (quals == 6) alias QualifierOf = SharedInoutOf;
        else                        alias QualifierOf = SharedConstInoutOf;
    }
}

///
@safe unittest
{
    static assert(__traits(isSame, QualifierOf!(shared const inout int), SharedConstInoutOf));
    static assert(__traits(isSame, QualifierOf!(immutable int), ImmutableOf));
    static assert(__traits(isSame, QualifierOf!(shared int), SharedOf));
    static assert(__traits(isSame, QualifierOf!(shared inout int), SharedInoutOf));
    import std.meta : Alias;
    static assert(__traits(isSame, QualifierOf!(int), Alias));
}

@safe unittest
{
    alias Qual1 = QualifierOf!(             int);   static assert(is(Qual1!long ==              long));
    alias Qual2 = QualifierOf!(       inout int);   static assert(is(Qual2!long ==        inout long));
    alias Qual3 = QualifierOf!(       const int);   static assert(is(Qual3!long ==        const long));
    alias Qual4 = QualifierOf!(shared       int);   static assert(is(Qual4!long == shared       long));
    alias Qual5 = QualifierOf!(shared inout int);   static assert(is(Qual5!long == shared inout long));
    alias Qual6 = QualifierOf!(shared const int);   static assert(is(Qual6!long == shared const long));
    alias Qual7 = QualifierOf!(   immutable int);   static assert(is(Qual7!long ==    immutable long));
}

version (StdUnittest)
{
    import std.meta : Alias;
    alias TypeQualifierList = AliasSeq!(Alias, ConstOf, SharedOf, SharedConstOf, ImmutableOf);

    struct SubTypeOf(T)
    {
        T val;
        alias val this;
    }
}

private alias parentOf(alias sym) = Identity!(__traits(parent, sym));
private alias parentOf(alias sym : T!Args, alias T, Args...) = Identity!(__traits(parent, T));

/**
 * Get the full package name for the given symbol.
 */
template packageName(alias T)
{
    import std.algorithm.searching : startsWith;

    enum bool isNotFunc = !isSomeFunction!(T);

    static if (__traits(compiles, parentOf!T))
        enum parent = packageName!(parentOf!T);
    else
        enum string parent = null;

    static if (isNotFunc && T.stringof.startsWith("package "))
        enum packageName = (parent.length ? parent ~ '.' : "") ~ T.stringof[8 .. $];
    else static if (parent)
        enum packageName = parent;
    else
        static assert(false, T.stringof ~ " has no parent");
}

///
@safe unittest
{
    static assert(packageName!packageName == "std");
}

@safe unittest
{
    import std.array;

    static assert(packageName!std == "std");
    static assert(packageName!(std.traits) == "std");     // this module
    static assert(packageName!packageName == "std");      // symbol in this module
    static assert(packageName!(std.array) == "std");  // other module from same package

    import core.sync.barrier;  // local import
    static assert(packageName!core == "core");
    static assert(packageName!(core.sync) == "core.sync");
    static assert(packageName!Barrier == "core.sync");

    struct X12287(T) { T i; }
    static assert(packageName!(X12287!int.i) == "std");
}

version (none) @safe unittest //Please uncomment me when changing packageName to test global imports
{
    import core.sync.barrier;  // global import
    static assert(packageName!core == "core");
    static assert(packageName!(core.sync) == "core.sync");
    static assert(packageName!Barrier == "core.sync");
}

///
@safe unittest
{
    static assert(packageName!moduleName == "std");
}

// https://issues.dlang.org/show_bug.cgi?id=13741
@safe unittest
{
    import std.ascii : isWhite;
    static assert(packageName!(isWhite) == "std");

    struct Foo{void opCall(int){}}
    static assert(packageName!(Foo.opCall) == "std");

    @property void function(int) vf;
    static assert(packageName!(vf) == "std");
}

/**
 * Get the module name (including package) for the given symbol.
 */
template moduleName(alias T)
{
    import std.algorithm.searching : startsWith;

    enum bool isNotFunc = !isSomeFunction!(T);

    static if (isNotFunc)
        static assert(!T.stringof.startsWith("package "),
            "cannot get the module name for a package");

    static if (isNotFunc && T.stringof.startsWith("module "))
    {
        static if (__traits(compiles, packageName!T))
            enum packagePrefix = packageName!T ~ '.';
        else
            enum packagePrefix = "";

        enum moduleName = packagePrefix ~ T.stringof[7..$];
    }
    else
        alias moduleName = moduleName!(parentOf!T); // If you use enum, it will cause compiler ICE
}

///
@safe unittest
{
    static assert(moduleName!moduleName == "std.traits");
}

@safe unittest
{
    import std.array;

    static assert(!__traits(compiles, moduleName!std));
    static assert(moduleName!(std.traits) == "std.traits");            // this module
    static assert(moduleName!moduleName == "std.traits");              // symbol in this module
    static assert(moduleName!(std.array) == "std.array");      // other module
    static assert(moduleName!(std.array.array) == "std.array");  // symbol in other module

    import core.sync.barrier;  // local import
    static assert(!__traits(compiles, moduleName!(core.sync)));
    static assert(moduleName!(core.sync.barrier) == "core.sync.barrier");
    static assert(moduleName!Barrier == "core.sync.barrier");

    struct X12287(T) { T i; }
    static assert(moduleName!(X12287!int.i) == "std.traits");
}

// https://issues.dlang.org/show_bug.cgi?id=13741
@safe unittest
{
    import std.ascii : isWhite;
    static assert(moduleName!(isWhite) == "std.ascii");

    struct Foo{void opCall(int){}}
    static assert(moduleName!(Foo.opCall) == "std.traits");

    @property void function(int) vf;
    static assert(moduleName!(vf) == "std.traits");
}

version (none) @safe unittest //Please uncomment me when changing moduleName to test global imports
{
    import core.sync.barrier;  // global import
    static assert(!__traits(compiles, moduleName!(core.sync)));
    static assert(moduleName!(core.sync.barrier) == "core.sync.barrier");
    static assert(moduleName!Barrier == "core.sync.barrier");
}

/***
 * Get the fully qualified name of a type or a symbol. Can act as an intelligent type/symbol to string  converter.

Example:
-----------------
module myModule;
struct MyStruct {}
static assert(fullyQualifiedName!(const MyStruct[]) == "const(myModule.MyStruct[])");
-----------------
*/
enum fullyQualifiedName(T) = fqnType!(T, false, false, false, false);

/// ditto
enum fullyQualifiedName(alias T) = fqnSym!(T);

///
@safe unittest
{
    static assert(fullyQualifiedName!fullyQualifiedName == "std.traits.fullyQualifiedName");
}

version (StdUnittest)
{
    // Used for both fqnType and fqnSym unittests
    private struct QualifiedNameTests
    {
        struct Inner
        {
            bool value;
        }

        ref const(Inner[string]) func( ref Inner var1, lazy scope string var2 );
        ref const(Inner[string]) retfunc( return ref Inner var1 );
        Inner inoutFunc(inout Inner) inout;
        shared(const(Inner[string])[]) data;
        const Inner delegate(double, string) @safe nothrow deleg;
        inout(int) delegate(inout int) inout inoutDeleg;
        Inner function(out double, string) funcPtr;
        extern(C) Inner function(double, string) cFuncPtr;

        extern(C) void cVarArg(int, ...);
        void dVarArg(...);
        void dVarArg2(int, ...);
        void typesafeVarArg(int[] ...);

        Inner[] array;
        Inner[16] sarray;
        Inner[Inner] aarray;
        const(Inner[const(Inner)]) qualAarray;

        shared(immutable(Inner) delegate(ref double, scope string) const shared @trusted nothrow) attrDeleg;

        struct Data(T) { int x; }
        void tfunc(T...)(T args) {}

        template Inst(alias A) { int x; }

        class Test12309(T, int x, string s) {}
    }

    private enum QualifiedEnum
    {
        a = 42
    }
}

private template fqnSym(alias T : X!A, alias X, A...)
{
    template fqnTuple(T...)
    {
        static if (T.length == 0)
            enum fqnTuple = "";
        else static if (T.length == 1)
        {
            static if (isExpressionTuple!T)
                enum fqnTuple = T[0].stringof;
            else
                enum fqnTuple = fullyQualifiedName!(T[0]);
        }
        else
            enum fqnTuple = fqnTuple!(T[0]) ~ ", " ~ fqnTuple!(T[1 .. $]);
    }

    enum fqnSym =
        fqnSym!(__traits(parent, X)) ~
        '.' ~ __traits(identifier, X) ~ "!(" ~ fqnTuple!A ~ ")";
}

private template fqnSym(alias T)
{
    static if (__traits(compiles, __traits(parent, T)) && !__traits(isSame, T, __traits(parent, T)))
        enum parentPrefix = fqnSym!(__traits(parent, T)) ~ ".";
    else
        enum parentPrefix = null;

    static string adjustIdent(string s)
    {
        import std.algorithm.searching : findSplit, skipOver;

        if (s.skipOver("package ") || s.skipOver("module "))
            return s;
        return s.findSplit("(")[0];
    }
    enum fqnSym = parentPrefix ~ adjustIdent(__traits(identifier, T));
}

@safe unittest
{
    alias fqn = fullyQualifiedName;

    // Make sure those 2 are the same
    static assert(fqnSym!fqn == fqn!fqn);

    static assert(fqn!fqn == "std.traits.fullyQualifiedName");

    alias qnTests = QualifiedNameTests;
    enum prefix = "std.traits.QualifiedNameTests.";
    static assert(fqn!(qnTests.Inner)           == prefix ~ "Inner");
    static assert(fqn!(qnTests.func)            == prefix ~ "func");
    static assert(fqn!(qnTests.Data!int)        == prefix ~ "Data!(int)");
    static assert(fqn!(qnTests.Data!int.x)      == prefix ~ "Data!(int).x");
    static assert(fqn!(qnTests.tfunc!(int[]))   == prefix ~ "tfunc!(int[])");
    static assert(fqn!(qnTests.Inst!(Object))   == prefix ~ "Inst!(object.Object)");
    static assert(fqn!(qnTests.Inst!(Object).x) == prefix ~ "Inst!(object.Object).x");

    static assert(fqn!(qnTests.Test12309!(int, 10, "str"))
                                                == prefix ~ "Test12309!(int, 10, \"str\")");

    import core.sync.barrier;
    static assert(fqn!Barrier == "core.sync.barrier.Barrier");
}

@safe unittest
{
    struct TemplatedStruct()
    {
        enum foo = 0;
    }
    alias TemplatedStructAlias = TemplatedStruct;
    assert("TemplatedStruct.foo" == fullyQualifiedName!(TemplatedStructAlias!().foo));
}

private template fqnType(T,
    bool alreadyConst, bool alreadyImmutable, bool alreadyShared, bool alreadyInout)
{
    // Convenience tags
    enum {
        _const = 0,
        _immutable = 1,
        _shared = 2,
        _inout = 3
    }

    alias qualifiers   = AliasSeq!(is(T == const), is(T == immutable), is(T == shared), is(T == inout));
    alias noQualifiers = AliasSeq!(false, false, false, false);

    string storageClassesString(uint psc)() @property
    {
        import std.conv : text;

        alias PSC = ParameterStorageClass;

        return text(
            psc & PSC.scope_ ? "scope " : "",
            psc & PSC.return_ ? "return " : "",
            psc & PSC.in_ ? "in " : "",
            psc & PSC.out_ ? "out " : "",
            psc & PSC.ref_ ? "ref " : "",
            psc & PSC.lazy_ ? "lazy " : "",
        );
    }

    string parametersTypeString(T)() @property
    {
        alias parameters   = Parameters!(T);
        alias parameterStC = ParameterStorageClassTuple!(T);

        enum variadic = variadicFunctionStyle!T;
        static if (variadic == Variadic.no)
            enum variadicStr = "";
        else static if (variadic == Variadic.c)
            enum variadicStr = ", ...";
        else static if (variadic == Variadic.d)
            enum variadicStr = parameters.length ? ", ..." : "...";
        else static if (variadic == Variadic.typesafe)
            enum variadicStr = " ...";
        else
            static assert(0, "New variadic style has been added, please update fullyQualifiedName implementation");

        static if (parameters.length)
        {
            import std.algorithm.iteration : map;
            import std.array : join;
            import std.meta : staticMap;
            import std.range : zip;

            string result = join(
                map!(a => (a[0] ~ a[1]))(
                    zip([staticMap!(storageClassesString, parameterStC)],
                        [staticMap!(fullyQualifiedName, parameters)])
                ),
                ", "
            );

            return result ~= variadicStr;
        }
        else
            return variadicStr;
    }

    string linkageString(T)() @property
    {
        enum linkage = functionLinkage!T;

        if (linkage != "D")
            return "extern(" ~ linkage ~ ") ";
        else
            return "";
    }

    string functionAttributeString(T)() @property
    {
        alias FA = FunctionAttribute;
        enum attrs = functionAttributes!T;

        static if (attrs == FA.none)
            return "";
        else
            return
                (attrs & FA.pure_ ? " pure" : "")
                ~ (attrs & FA.nothrow_ ? " nothrow" : "")
                ~ (attrs & FA.ref_ ? " ref" : "")
                ~ (attrs & FA.property ? " @property" : "")
                ~ (attrs & FA.trusted ? " @trusted" : "")
                ~ (attrs & FA.safe ? " @safe" : "")
                ~ (attrs & FA.nogc ? " @nogc" : "")
                ~ (attrs & FA.return_ ? " return" : "")
                ~ (attrs & FA.live ? " @live" : "");
    }

    string addQualifiers(string typeString,
        bool addConst, bool addImmutable, bool addShared, bool addInout)
    {
        auto result = typeString;
        if (addShared)
        {
            result = "shared(" ~ result ~")";
        }
        if (addConst || addImmutable || addInout)
        {
            result = (addConst ? "const" : addImmutable ? "immutable" : "inout")
                ~ "(" ~ result ~ ")";
        }
        return result;
    }

    // Convenience template to avoid copy-paste
    template chain(string current)
    {
        enum chain = addQualifiers(current,
            qualifiers[_const]     && !alreadyConst,
            qualifiers[_immutable] && !alreadyImmutable,
            qualifiers[_shared]    && !alreadyShared,
            qualifiers[_inout]     && !alreadyInout);
    }

    static if (is(T == string))
    {
        enum fqnType = "string";
    }
    else static if (is(T == wstring))
    {
        enum fqnType = "wstring";
    }
    else static if (is(T == dstring))
    {
        enum fqnType = "dstring";
    }
    else static if (is(T == typeof(null)))
    {
        enum fqnType = "typeof(null)";
    }
    else static if (isBasicType!T && !is(T == enum))
    {
        enum fqnType = chain!((Unqual!T).stringof);
    }
    else static if (isAggregateType!T || is(T == enum))
    {
        enum fqnType = chain!(fqnSym!T);
    }
    else static if (isStaticArray!T)
    {
        import std.conv : to;
        enum fqnType = chain!(
            fqnType!(typeof(T.init[0]), qualifiers) ~ "[" ~ to!string(T.length) ~ "]"
        );
    }
    else static if (isArray!T)
    {
        enum fqnType = chain!(
            fqnType!(typeof(T.init[0]), qualifiers) ~ "[]"
        );
    }
    else static if (isAssociativeArray!T)
    {
        enum fqnType = chain!(
            fqnType!(ValueType!T, qualifiers) ~ '[' ~ fqnType!(KeyType!T, noQualifiers) ~ ']'
        );
    }
    else static if (isSomeFunction!T)
    {
        static if (is(T F == delegate))
        {
            enum qualifierString =
                (is(F == shared) ? " shared" : "")
                ~ (is(F == inout) ? " inout" :
                    is(F == immutable) ? " immutable" :
                    is(F == const) ? " const" : "");
            enum fqnType = chain!(
                linkageString!T
                ~ fqnType!(ReturnType!T, noQualifiers)
                ~ " delegate(" ~ parametersTypeString!(T) ~ ")"
                ~ functionAttributeString!T
                ~ qualifierString
            );
        }
        else
        {
            enum fqnType = chain!(
                linkageString!T
                ~ fqnType!(ReturnType!T, noQualifiers)
                ~ (isFunctionPointer!T ? " function(" : "(")
                ~ parametersTypeString!(T) ~ ")"
                ~ functionAttributeString!T
            );
        }
    }
    else static if (is(T == U*, U))
    {
        enum fqnType = chain!(
            fqnType!(U, qualifiers) ~ "*"
        );
    }
    else static if (is(T : __vector(V[N]), V, size_t N))
    {
        import std.conv : to;
        enum fqnType = chain!(
            "__vector(" ~ fqnType!(V, qualifiers) ~ "[" ~ N.to!string ~ "])"
        );
    }
    else
        // In case something is forgotten
        static assert(0, "Unrecognized type " ~ T.stringof ~ ", can't convert to fully qualified string");
}

@safe unittest
{
    import std.format : format;
    alias fqn = fullyQualifiedName;

    // Verify those 2 are the same for simple case
    alias Ambiguous = const(QualifiedNameTests.Inner);
    static assert(fqn!Ambiguous == fqnType!(Ambiguous, false, false, false, false));

    // Main tests
    enum inner_name = "std.traits.QualifiedNameTests.Inner";
    with (QualifiedNameTests)
    {
        // Special cases
        static assert(fqn!(string) == "string");
        static assert(fqn!(wstring) == "wstring");
        static assert(fqn!(dstring) == "dstring");
        static assert(fqn!(typeof(null)) == "typeof(null)");
        static assert(fqn!(void) == "void");
        static assert(fqn!(const(void)) == "const(void)");
        static assert(fqn!(shared(void)) == "shared(void)");
        static assert(fqn!(shared const(void)) == "const(shared(void))");
        static assert(fqn!(shared inout(void)) == "inout(shared(void))");
        static assert(fqn!(shared inout const(void)) == "const(shared(void))");
        static assert(fqn!(inout(void)) == "inout(void)");
        static assert(fqn!(inout const(void)) == "const(void)");
        static assert(fqn!(immutable(void)) == "immutable(void)");

        // Basic qualified name
        static assert(fqn!(Inner) == inner_name);
        static assert(fqn!(QualifiedEnum) == "std.traits.QualifiedEnum"); // type
        static assert(fqn!(QualifiedEnum.a) == "std.traits.QualifiedEnum.a"); // symbol

        // Array types
        static assert(fqn!(typeof(array)) == format("%s[]", inner_name));
        static assert(fqn!(typeof(sarray)) == format("%s[16]", inner_name));
        static assert(fqn!(typeof(aarray)) == format("%s[%s]", inner_name, inner_name));

        // qualified key for AA
        static assert(fqn!(typeof(qualAarray)) == format("const(%s[const(%s)])", inner_name, inner_name));

        // Qualified composed data types
        static assert(fqn!(typeof(data)) == format("shared(const(%s[string])[])", inner_name));

        // Function types + function attributes
        static assert(fqn!(typeof(func)) == format("const(%s[string])(ref %s, scope lazy string) ref",
                    inner_name, inner_name));
        static assert(fqn!(typeof(retfunc)) == format("const(%s[string])(return %s) ref", inner_name, inner_name));
        static assert(fqn!(typeof(inoutFunc)) == format("inout(%s(inout(%s)))", inner_name, inner_name));
        static assert(fqn!(typeof(deleg)) == format("const(%s delegate(double, string) nothrow @safe)", inner_name));
        static assert(fqn!(typeof(inoutDeleg)) == "inout(int) delegate(inout(int)) inout");
        static assert(fqn!(typeof(funcPtr)) == format("%s function(out double, string)", inner_name));
        static assert(fqn!(typeof(cFuncPtr)) == format("extern(C) %s function(double, string)", inner_name));

        // Delegate type with qualified function type
        static assert(fqn!(typeof(attrDeleg)) == format("shared(immutable(%s) "~
            "delegate(ref double, scope string) nothrow @trusted shared const)", inner_name));

        // Variable argument function types
        static assert(fqn!(typeof(cVarArg)) == "extern(C) void(int, ...)");
        static assert(fqn!(typeof(dVarArg)) == "void(...)");
        static assert(fqn!(typeof(dVarArg2)) == "void(int, ...)");
        static assert(fqn!(typeof(typesafeVarArg)) == "void(int[] ...)");

        // SIMD vector
        static if (is(__vector(float[4])))
        {
            static assert(fqn!(__vector(float[4])) == "__vector(float[4])");
        }
    }
}

/***
 * Get the type of the return value from a function,
 * a pointer to function, a delegate, a struct
 * with an opCall, a pointer to a struct with an opCall,
 * or a class with an `opCall`. Please note that $(D_KEYWORD ref)
 * is not part of a type, but the attribute of the function
 * (see template $(LREF functionAttributes)).
 *
 * $(NOTE To reduce template instantiations, consider instead using
 * $(D typeof(() { return func(args); } ())) if the argument types are known or
 * $(D static if (is(typeof(func) Ret == return))) if only that basic test is needed.)
 */
template ReturnType(alias func)
if (isCallable!func)
{
    static if (is(FunctionTypeOf!func R == return))
        alias ReturnType = R;
    else
        static assert(0, "argument has no return type");
}

///
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

/***
Get, as a tuple, the types of the parameters to a function, a pointer
to function, a delegate, a struct with an `opCall`, a pointer to a
struct with an `opCall`, or a class with an `opCall`.
*/
template Parameters(alias func)
if (isCallable!func)
{
    static if (is(FunctionTypeOf!func P == function))
        alias Parameters = P;
    else
        static assert(0, "argument has no parameters");
}

///
@safe unittest
{
    int foo(int, long);
    void bar(Parameters!foo);      // declares void bar(int, long);
    void abc(Parameters!foo[1]);   // declares void abc(long);
}

/**
 * Alternate name for $(LREF Parameters), kept for legacy compatibility.
 */
alias ParameterTypeTuple = Parameters;

@safe unittest
{
    int foo(int i, bool b) { return 0; }
    static assert(is(ParameterTypeTuple!foo == AliasSeq!(int, bool)));
    static assert(is(ParameterTypeTuple!(typeof(&foo)) == AliasSeq!(int, bool)));

    struct S { real opCall(real r, int i) { return 0.0; } }
    S s;
    static assert(is(ParameterTypeTuple!S == AliasSeq!(real, int)));
    static assert(is(ParameterTypeTuple!(S*) == AliasSeq!(real, int)));
    static assert(is(ParameterTypeTuple!s == AliasSeq!(real, int)));

    class Test
    {
        int prop() @property { return 0; }
    }
    alias P_Test_prop = ParameterTypeTuple!(Test.prop);
    static assert(P_Test_prop.length == 0);

    alias P_dglit = ParameterTypeTuple!((int a){});
    static assert(P_dglit.length == 1);
    static assert(is(P_dglit[0] == int));
}

/**
Returns the number of arguments of function `func`.
arity is undefined for variadic functions.
*/
template arity(alias func)
if (isCallable!func && variadicFunctionStyle!func == Variadic.no)
{
    enum size_t arity = Parameters!func.length;
}

///
@safe unittest
{
    void foo(){}
    static assert(arity!foo == 0);
    void bar(uint){}
    static assert(arity!bar == 1);
    void variadicFoo(uint...){}
    static assert(!__traits(compiles, arity!variadicFoo));
}

// https://issues.dlang.org/show_bug.cgi?id=11389
@safe unittest
{
    alias TheType = size_t function( string[] );
    static assert(arity!TheType == 1);
}

/**
Get tuple, one per function parameter, of the storage classes of the parameters.
Params:
    func = function symbol or type of function, delegate, or pointer to function
Returns:
    A tuple of ParameterStorageClass bits
 */
enum ParameterStorageClass : uint
{
    /**
     * These flags can be bitwise OR-ed together to represent complex storage
     * class.
     */
    none    = 0x00,
    in_     = 0x01, /// ditto
    ref_    = 0x02, /// ditto
    out_    = 0x04, /// ditto
    lazy_   = 0x08, /// ditto
    scope_  = 0x10, /// ditto
    return_ = 0x20, /// ditto
}

/// ditto
template ParameterStorageClassTuple(alias func)
if (isCallable!func)
{
    alias Func = FunctionTypeOf!func;

    static if (is(Func PT == __parameters))
    {
        template StorageClass(size_t i)
        {
            static if (i < PT.length)
            {
                alias StorageClass = AliasSeq!(
                        extractParameterStorageClassFlags!(__traits(getParameterStorageClasses, Func, i)),
                        StorageClass!(i + 1));
            }
            else
                alias StorageClass = AliasSeq!();
        }
        alias ParameterStorageClassTuple = StorageClass!0;
    }
    else
    {
        static assert(0, func[0].stringof ~ " is not a function");
        alias ParameterStorageClassTuple = AliasSeq!();
    }
}

///
@safe unittest
{
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

/**
Convert the result of $(DDSUBLINK spec/traits, getParameterStorageClasses, `__traits(getParameterStorageClasses)`)
to $(LREF ParameterStorageClass) `enum`s.

Params:
    Attribs = The return value of `__traits(getParameterStorageClasses)`
Returns:
    The bitwise OR of the equivalent $(LREF ParameterStorageClass) `enum`s.
 */
template extractParameterStorageClassFlags(Attribs...)
{
    enum ParameterStorageClass extractParameterStorageClassFlags = ()
    {
        auto result = ParameterStorageClass.none;
        static if (Attribs.length > 0)
        {
            static foreach (attrib; Attribs)
            {
                final switch (attrib) with (ParameterStorageClass)
                {
                    case "scope":  result |= scope_;  break;
                    case "in":     result |= in_;    break;
                    case "out":    result |= out_;    break;
                    case "ref":    result |= ref_;    break;
                    case "lazy":   result |= lazy_;   break;
                    case "return": result |= return_; break;
                }
            }
            /* Mimic behavor of original version of ParameterStorageClassTuple()
             * to avoid breaking existing code.
             */
            if (result == (ParameterStorageClass.ref_ | ParameterStorageClass.return_))
                result = ParameterStorageClass.return_;
        }
        return result;
    }();
}

///
@safe unittest
{
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
    alias STC = ParameterStorageClass;

    void noparam() {}
    static assert(ParameterStorageClassTuple!noparam.length == 0);

    ref int test(scope int*, ref int, out int, lazy int, int, return ref int i) { return i; }
    alias test_pstc = ParameterStorageClassTuple!test;
    static assert(test_pstc.length == 6);
    static assert(test_pstc[0] == STC.scope_);
    static assert(test_pstc[1] == STC.ref_);
    static assert(test_pstc[2] == STC.out_);
    static assert(test_pstc[3] == STC.lazy_);
    static assert(test_pstc[4] == STC.none);
    static assert(test_pstc[5] == STC.return_);

    interface Test
    {
        void test_const(int) const;
        void test_sharedconst(int) shared const;
    }
    Test testi;

    alias test_const_pstc = ParameterStorageClassTuple!(Test.test_const);
    static assert(test_const_pstc.length == 1);
    static assert(test_const_pstc[0] == STC.none);

    alias test_sharedconst_pstc = ParameterStorageClassTuple!(testi.test_sharedconst);
    static assert(test_sharedconst_pstc.length == 1);
    static assert(test_sharedconst_pstc[0] == STC.none);

    alias dglit_pstc = ParameterStorageClassTuple!((ref int a) {});
    static assert(dglit_pstc.length == 1);
    static assert(dglit_pstc[0] == STC.ref_);

    // https://issues.dlang.org/show_bug.cgi?id=9317
    static inout(int) func(inout int param) { return param; }
    static assert(ParameterStorageClassTuple!(typeof(func))[0] == STC.none);
}

@safe unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=14253
    static struct Foo {
        ref Foo opAssign(ref Foo rhs) return { return this; }
    }

    alias tup = ParameterStorageClassTuple!(__traits(getOverloads, Foo, "opAssign")[0]);
}


/**
Get, as a tuple, the identifiers of the parameters to a function symbol.
 */
template ParameterIdentifierTuple(alias func)
if (isCallable!func)
{
    static if (is(FunctionTypeOf!func PT == __parameters))
    {
        template Get(size_t i)
        {
            static if (!isFunctionPointer!func && !isDelegate!func
                       // Unnamed parameters yield CT error.
                       && is(typeof(__traits(identifier, PT[i .. i+1])))
                       // Filter out unnamed args, which look like (Type) instead of (Type name).
                       && PT[i].stringof != PT[i .. i+1].stringof[1..$-1])
            {
                enum Get = __traits(identifier, PT[i .. i+1]);
            }
            else
            {
                enum Get = "";
            }
        }
    }
    else
    {
        static assert(0, func.stringof ~ " is not a function");

        // Define dummy entities to avoid pointless errors
        template Get(size_t i) { enum Get = ""; }
        alias PT = AliasSeq!();
    }

    template Impl(size_t i = 0)
    {
        static if (i == PT.length)
            alias Impl = AliasSeq!();
        else
            alias Impl = AliasSeq!(Get!i, Impl!(i+1));
    }

    alias ParameterIdentifierTuple = Impl!();
}

///
@safe unittest
{
    int foo(int num, string name, int);
    static assert([ParameterIdentifierTuple!foo] == ["num", "name", ""]);
}

// https://issues.dlang.org/show_bug.cgi?id=19456
@safe unittest
{
    struct SomeType {}
    void foo(SomeType);
    void bar(int);
    static assert([ParameterIdentifierTuple!foo] == [""]);
    static assert([ParameterIdentifierTuple!bar] == [""]);
}

@safe unittest
{
    alias PIT = ParameterIdentifierTuple;

    void bar(int num, string name, int[] array){}
    static assert([PIT!bar] == ["num", "name", "array"]);

    // might be changed in the future?
    void function(int num, string name) fp;
    static assert([PIT!fp] == ["", ""]);

    // might be changed in the future?
    void delegate(int num, string name, int[long] aa) dg;
    static assert([PIT!dg] == ["", "", ""]);

    interface Test
    {
        @property string getter();
        @property void setter(int a);
        Test method(int a, long b, string c);
    }
    static assert([PIT!(Test.getter)] == []);
    static assert([PIT!(Test.setter)] == ["a"]);
    static assert([PIT!(Test.method)] == ["a", "b", "c"]);

/+
    // depends on internal
    void baw(int, string, int[]){}
    static assert([PIT!baw] == ["_param_0", "_param_1", "_param_2"]);

    // depends on internal
    void baz(AliasSeq!(int, string, int[]) args){}
    static assert([PIT!baz] == ["_param_0", "_param_1", "_param_2"]);
+/
}


/**
Get, as a tuple, the default value of the parameters to a function symbol.
If a parameter doesn't have the default value, `void` is returned instead.
 */
template ParameterDefaults(alias func)
if (isCallable!func)
{
    alias param_names = ParameterIdentifierTuple!func;
    static if (is(FunctionTypeOf!(func) PT == __parameters))
    {
        template Get(size_t i)
        {
            // `PT[i .. i+1]` declares a parameter with an arbitrary name.
            // To avoid a name clash, generate local names that are distinct
            // from the parameter name, and mix them in.
            enum name = param_names[i];
            enum args = "args" ~ (name == "args" ? "_" : "");
            enum val = "val" ~ (name == "val" ? "_" : "");
            enum ptr = "ptr" ~ (name == "ptr" ? "_" : "");
            mixin("
                enum hasDefaultArg = (PT[i .. i+1] " ~ args ~ ") { return true; };
            ");
            static if (is(typeof(hasDefaultArg())))
            {
                mixin("
                // workaround scope escape check, see
                // https://issues.dlang.org/show_bug.cgi?id=16582
                // should use return scope once available
                enum get = (PT[i .. i+1] " ~ args ~ ") @trusted
                {
                    // If the parameter is lazy, we force it to be evaluated
                    // like this.
                    auto " ~ val ~ " = " ~ args ~ "[0];
                    auto " ~ ptr ~ " = &" ~ val ~ ";
                    return *" ~ ptr ~ ";
                };");
                enum Get = get();
            }
            else
                alias Get = void;
                // If default arg doesn't exist, returns void instead.
        }
    }
    else
    {
        static assert(0, func.stringof ~ " is not a function");

        // Define dummy entities to avoid pointless errors
        template Get(size_t i) { enum Get = ""; }
        alias PT = AliasSeq!();
    }

    template Impl(size_t i = 0)
    {
        static if (i == PT.length)
            alias Impl = AliasSeq!();
        else
            alias Impl = AliasSeq!(Get!i, Impl!(i+1));
    }

    alias ParameterDefaults = Impl!();
}

///
@safe unittest
{
    int foo(int num, string name = "hello", int[] = [1,2,3], lazy int x = 0);
    static assert(is(ParameterDefaults!foo[0] == void));
    static assert(   ParameterDefaults!foo[1] == "hello");
    static assert(   ParameterDefaults!foo[2] == [1,2,3]);
    static assert(   ParameterDefaults!foo[3] == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=17192
@safe unittest
{
    static void func(int i, int PT, int __pd_value, int __pd_val, int __args,
        int name, int args, int val, int ptr, int args_, int val_, int ptr_)
    {
    }
    alias Voids = ParameterDefaults!func;
    static assert(Voids.length == 12);
    static foreach (V; Voids) static assert(is(V == void));
}

// https://issues.dlang.org/show_bug.cgi?id=20182
@safe pure nothrow @nogc unittest
{
    struct S
    {
        this(ref S) {}
    }

    static assert(__traits(compiles, ParameterDefaults!(S.__ctor)));
}

/**
 * Alternate name for $(LREF ParameterDefaults), kept for legacy compatibility.
 */
alias ParameterDefaultValueTuple = ParameterDefaults;

@safe unittest
{
    alias PDVT = ParameterDefaultValueTuple;

    void bar(int n = 1, string s = "hello"){}
    static assert(PDVT!bar.length == 2);
    static assert(PDVT!bar[0] == 1);
    static assert(PDVT!bar[1] == "hello");
    static assert(is(typeof(PDVT!bar) == typeof(AliasSeq!(1, "hello"))));

    void baz(int x, int n = 1, string s = "hello"){}
    static assert(PDVT!baz.length == 3);
    static assert(is(PDVT!baz[0] == void));
    static assert(   PDVT!baz[1] == 1);
    static assert(   PDVT!baz[2] == "hello");
    static assert(is(typeof(PDVT!baz) == typeof(AliasSeq!(void, 1, "hello"))));

    // property functions return empty string
    // https://issues.dlang.org/show_bug.cgi?id=10800
    @property void foo(int x = 3) { }
    static assert(PDVT!foo.length == 1);
    static assert(PDVT!foo[0] == 3);
    static assert(is(typeof(PDVT!foo) == typeof(AliasSeq!(3))));

    struct Colour
    {
        ubyte a,r,g,b;

        static immutable Colour white = Colour(255,255,255,255);
    }
    // https://issues.dlang.org/show_bug.cgi?id=8106
    void bug8106(Colour c = Colour.white) {}
    //pragma(msg, PDVT!bug8106);
    static assert(PDVT!bug8106[0] == Colour.white);
    // https://issues.dlang.org/show_bug.cgi?id=16582
    void bug16582(scope int* val = null) {}
    static assert(PDVT!bug16582[0] is null);
}


/**
Returns the FunctionAttribute mask for function `func`.

See_Also:
    $(LREF hasFunctionAttributes)
 */
enum FunctionAttribute : uint
{
    /**
     * These flags can be bitwise OR-ed together to represent a complex attribute.
     */
    none       = 0,
    pure_      = 1 << 0,  /// ditto
    nothrow_   = 1 << 1,  /// ditto
    ref_       = 1 << 2,  /// ditto
    property   = 1 << 3,  /// ditto
    trusted    = 1 << 4,  /// ditto
    safe       = 1 << 5,  /// ditto
    nogc       = 1 << 6,  /// ditto
    system     = 1 << 7,  /// ditto
    const_     = 1 << 8,  /// ditto
    immutable_ = 1 << 9,  /// ditto
    inout_     = 1 << 10, /// ditto
    shared_    = 1 << 11, /// ditto
    return_    = 1 << 12, /// ditto
    scope_     = 1 << 13, /// ditto
    live       = 1 << 14, /// ditto
}

/// ditto
template functionAttributes(alias func)
if (isCallable!func)
{
    // @bug: workaround for opCall
    alias FuncSym = Select!(is(typeof(__traits(getFunctionAttributes, func))),
                            func, Unqual!(FunctionTypeOf!func));

    enum FunctionAttribute functionAttributes =
        extractAttribFlags!(__traits(getFunctionAttributes, FuncSym))();
}

///
@safe unittest
{
    alias FA = FunctionAttribute; // shorten the enum name

    real func(real x) pure nothrow @safe
    {
        return x;
    }
    static assert(functionAttributes!func & FA.pure_);
    static assert(functionAttributes!func & FA.safe);
    static assert(!(functionAttributes!func & FA.trusted)); // not @trusted
}

@system unittest
{
    alias FA = FunctionAttribute;

    struct S
    {
        int noF() { return 0; }
        int constF() const { return 0; }
        int immutableF() immutable { return 0; }
        int inoutF() inout { return 0; }
        int sharedF() shared { return 0; }

        int x;
        ref int refF() return { return x; }
        int propertyF() @property { return 0; }
        int nothrowF() nothrow { return 0; }
        int nogcF() @nogc { return 0; }

        int systemF() @system { return 0; }
        int trustedF() @trusted { return 0; }
        int safeF() @safe { return 0; }

        int pureF() pure { return 0; }

        int liveF() @live { return 0; }
    }

    static assert(functionAttributes!(S.noF) == FA.system);
    static assert(functionAttributes!(typeof(S.noF)) == FA.system);

    static assert(functionAttributes!(S.constF) == (FA.const_ | FA.system));
    static assert(functionAttributes!(typeof(S.constF)) == (FA.const_ | FA.system));

    static assert(functionAttributes!(S.immutableF) == (FA.immutable_ | FA.system));
    static assert(functionAttributes!(typeof(S.immutableF)) == (FA.immutable_ | FA.system));

    static assert(functionAttributes!(S.inoutF) == (FA.inout_ | FA.system));
    static assert(functionAttributes!(typeof(S.inoutF)) == (FA.inout_ | FA.system));

    static assert(functionAttributes!(S.sharedF) == (FA.shared_ | FA.system));
    static assert(functionAttributes!(typeof(S.sharedF)) == (FA.shared_ | FA.system));

    static assert(functionAttributes!(S.refF) == (FA.ref_ | FA.system | FA.return_));
    static assert(functionAttributes!(typeof(S.refF)) == (FA.ref_ | FA.system | FA.return_));

    static assert(functionAttributes!(S.propertyF) == (FA.property | FA.system));
    static assert(functionAttributes!(typeof(&S.propertyF)) == (FA.property | FA.system));

    static assert(functionAttributes!(S.nothrowF) == (FA.nothrow_ | FA.system));
    static assert(functionAttributes!(typeof(S.nothrowF)) == (FA.nothrow_ | FA.system));

    static assert(functionAttributes!(S.nogcF) == (FA.nogc | FA.system));
    static assert(functionAttributes!(typeof(S.nogcF)) == (FA.nogc | FA.system));

    static assert(functionAttributes!(S.systemF) == FA.system);
    static assert(functionAttributes!(typeof(S.systemF)) == FA.system);

    static assert(functionAttributes!(S.trustedF) == FA.trusted);
    static assert(functionAttributes!(typeof(S.trustedF)) == FA.trusted);

    static assert(functionAttributes!(S.safeF) == FA.safe);
    static assert(functionAttributes!(typeof(S.safeF)) == FA.safe);

    static assert(functionAttributes!(S.pureF) == (FA.pure_ | FA.system));
    static assert(functionAttributes!(typeof(S.pureF)) == (FA.pure_ | FA.system));

    static assert(functionAttributes!(S.liveF) == (FA.live | FA.system));
    static assert(functionAttributes!(typeof(S.liveF)) == (FA.live | FA.system));

    int pure_nothrow() nothrow pure;
    void safe_nothrow() @safe nothrow;
    static ref int static_ref_property() @property;
    ref int ref_property() @property;

    static assert(functionAttributes!(pure_nothrow) == (FA.pure_ | FA.nothrow_ | FA.system));
    static assert(functionAttributes!(typeof(pure_nothrow)) == (FA.pure_ | FA.nothrow_ | FA.system));

    static assert(functionAttributes!(safe_nothrow) == (FA.safe | FA.nothrow_));
    static assert(functionAttributes!(typeof(safe_nothrow)) == (FA.safe | FA.nothrow_));

    static assert(functionAttributes!(static_ref_property) == (FA.property | FA.ref_ | FA.system));
    static assert(functionAttributes!(typeof(&static_ref_property)) == (FA.property | FA.ref_ | FA.system));

    static assert(functionAttributes!(ref_property) == (FA.property | FA.ref_ | FA.system));
    static assert(functionAttributes!(typeof(&ref_property)) == (FA.property | FA.ref_ | FA.system));

    struct S2
    {
        int pure_const() const pure { return 0; }
        int pure_sharedconst() const shared pure { return 0; }
    }

    static assert(functionAttributes!(S2.pure_const) == (FA.const_ | FA.pure_ | FA.system));
    static assert(functionAttributes!(typeof(S2.pure_const)) == (FA.const_ | FA.pure_ | FA.system));

    static assert(functionAttributes!(S2.pure_sharedconst) == (FA.const_ | FA.shared_ | FA.pure_ | FA.system));
    static assert(functionAttributes!(typeof(S2.pure_sharedconst)) == (FA.const_ | FA.shared_ | FA.pure_ | FA.system));

    static assert(functionAttributes!((int a) { }) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.safe));
    static assert(functionAttributes!(typeof((int a) { })) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.safe));

    auto safeDel = delegate() @safe { };
    static assert(functionAttributes!(safeDel) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.safe));
    static assert(functionAttributes!(typeof(safeDel)) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.safe));

    auto trustedDel = delegate() @trusted { };
    static assert(functionAttributes!(trustedDel) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.trusted));
    static assert(functionAttributes!(typeof(trustedDel)) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.trusted));

    auto systemDel = delegate() @system { };
    static assert(functionAttributes!(systemDel) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.system));
    static assert(functionAttributes!(typeof(systemDel)) == (FA.pure_ | FA.nothrow_ | FA.nogc | FA.system));
}

private FunctionAttribute extractAttribFlags(Attribs...)()
{
    auto res = FunctionAttribute.none;

    static foreach (attrib; Attribs)
    {
        switch (attrib) with (FunctionAttribute)
        {
            case "pure":      res |= pure_; break;
            case "nothrow":   res |= nothrow_; break;
            case "ref":       res |= ref_; break;
            case "@property": res |= property; break;
            case "@trusted":  res |= trusted; break;
            case "@safe":     res |= safe; break;
            case "@nogc":     res |= nogc; break;
            case "@system":   res |= system; break;
            case "const":     res |= const_; break;
            case "immutable": res |= immutable_; break;
            case "inout":     res |= inout_; break;
            case "shared":    res |= shared_; break;
            case "return":    res |= return_; break;
            case "scope":     res |= scope_; break;
            case "@live":     res |= live; break;
            default: assert(0, attrib);
        }
    }

    return res;
}

/**
Checks whether a function has the given attributes attached.

Params:
    args = Function to check, followed by a
    variadic number of function attributes as strings

Returns:
    `true`, if the function has the list of attributes attached and `false` otherwise.

See_Also:
    $(LREF functionAttributes)
*/
template hasFunctionAttributes(args...)
if (args.length > 0 && isCallable!(args[0])
     && allSatisfy!(isSomeString, typeof(args[1 .. $])))
{
    enum bool hasFunctionAttributes = {
        import std.algorithm.searching : canFind;
        import std.range : only;
        enum funcAttribs = only(__traits(getFunctionAttributes, args[0]));
        static foreach (attribute; args[1 .. $])
        {
            if (!funcAttribs.canFind(attribute))
                return false;
        }
        return true;
    }();
}

///
@safe unittest
{
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

@system unittest
{
    struct S
    {
        int noF();
        int constF() const;
        int immutableF() immutable;
        int inoutF() inout;
        int sharedF() shared;

        ref int refF() return;
        int propertyF() @property;
        int nothrowF() nothrow;
        int nogcF() @nogc;

        int systemF() @system;
        int trustedF() @trusted;
        int safeF() @safe;

        int pureF() pure;

        int liveF() @live;
    }

    // true if no args passed
    static assert(hasFunctionAttributes!(S.noF));

    static assert(hasFunctionAttributes!(S.noF, "@system"));
    static assert(hasFunctionAttributes!(typeof(S.noF), "@system"));
    static assert(!hasFunctionAttributes!(S.noF, "@system", "pure"));

    static assert(hasFunctionAttributes!(S.constF, "const", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.constF), "const", "@system"));
    static assert(!hasFunctionAttributes!(S.constF, "const", "@system", "@nogc"));

    static assert(hasFunctionAttributes!(S.immutableF, "immutable", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.immutableF), "immutable", "@system"));
    static assert(!hasFunctionAttributes!(S.immutableF, "immutable", "@system", "pure"));

    static assert(hasFunctionAttributes!(S.inoutF, "inout", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.inoutF), "inout", "@system"));
    static assert(!hasFunctionAttributes!(S.inoutF, "inout", "@system", "pure"));

    static assert(hasFunctionAttributes!(S.sharedF, "shared", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.sharedF), "shared", "@system"));
    static assert(!hasFunctionAttributes!(S.sharedF, "shared", "@system", "@trusted"));

    static assert(hasFunctionAttributes!(S.refF, "ref", "@system", "return"));
    static assert(hasFunctionAttributes!(typeof(S.refF), "ref", "@system", "return"));
    static assert(!hasFunctionAttributes!(S.refF, "ref", "@system", "return", "pure"));

    static assert(hasFunctionAttributes!(S.propertyF, "@property", "@system"));
    static assert(hasFunctionAttributes!(typeof(&S.propertyF), "@property", "@system"));
    static assert(!hasFunctionAttributes!(S.propertyF, "@property", "@system", "ref"));

    static assert(hasFunctionAttributes!(S.nothrowF, "nothrow", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.nothrowF), "nothrow", "@system"));
    static assert(!hasFunctionAttributes!(S.nothrowF, "nothrow", "@system", "@trusted"));

    static assert(hasFunctionAttributes!(S.nogcF, "@nogc", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.nogcF), "@nogc", "@system"));
    static assert(!hasFunctionAttributes!(S.nogcF, "@nogc", "@system", "ref"));

    static assert(hasFunctionAttributes!(S.systemF, "@system"));
    static assert(hasFunctionAttributes!(typeof(S.systemF), "@system"));
    static assert(!hasFunctionAttributes!(S.systemF, "@system", "ref"));

    static assert(hasFunctionAttributes!(S.trustedF, "@trusted"));
    static assert(hasFunctionAttributes!(typeof(S.trustedF), "@trusted"));
    static assert(!hasFunctionAttributes!(S.trustedF, "@trusted", "@safe"));

    static assert(hasFunctionAttributes!(S.safeF, "@safe"));
    static assert(hasFunctionAttributes!(typeof(S.safeF), "@safe"));
    static assert(!hasFunctionAttributes!(S.safeF, "@safe", "nothrow"));

    static assert(hasFunctionAttributes!(S.pureF, "pure", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.pureF), "pure", "@system"));
    static assert(!hasFunctionAttributes!(S.pureF, "pure", "@system", "ref"));

    static assert(hasFunctionAttributes!(S.liveF, "@live", "@system"));
    static assert(hasFunctionAttributes!(typeof(S.liveF), "@live", "@system"));
    static assert(!hasFunctionAttributes!(S.liveF, "@live", "@system", "ref"));

    int pure_nothrow() nothrow pure { return 0; }
    void safe_nothrow() @safe nothrow { }
    static ref int static_ref_property() @property { return *(new int); }
    ref int ref_property() @property { return *(new int); }

    static assert(hasFunctionAttributes!(pure_nothrow, "pure", "nothrow", "@safe"));
    static assert(hasFunctionAttributes!(typeof(pure_nothrow), "pure", "nothrow", "@safe"));
    static assert(!hasFunctionAttributes!(pure_nothrow, "pure", "nothrow", "@safe", "@trusted"));

    static assert(hasFunctionAttributes!(safe_nothrow, "@safe", "nothrow"));
    static assert(hasFunctionAttributes!(typeof(safe_nothrow), "@safe", "nothrow"));
    static assert(hasFunctionAttributes!(safe_nothrow, "@safe", "nothrow", "pure"));
    static assert(!hasFunctionAttributes!(safe_nothrow, "@safe", "nothrow", "pure", "@trusted"));

    static assert(hasFunctionAttributes!(static_ref_property, "@property", "ref", "@safe"));
    static assert(hasFunctionAttributes!(typeof(&static_ref_property), "@property", "ref", "@safe"));
    static assert(hasFunctionAttributes!(static_ref_property, "@property", "ref", "@safe", "nothrow"));
    static assert(!hasFunctionAttributes!(static_ref_property, "@property", "ref", "@safe", "nothrow", "@nogc"));

    static assert(hasFunctionAttributes!(ref_property, "@property", "ref", "@safe"));
    static assert(hasFunctionAttributes!(typeof(&ref_property), "@property", "ref", "@safe"));
    static assert(!hasFunctionAttributes!(ref_property, "@property", "ref", "@safe", "@nogc"));

    struct S2
    {
        int pure_const() const pure { return 0; }
        int pure_sharedconst() const shared pure { return 0; }
    }

    static assert(hasFunctionAttributes!(S2.pure_const, "const", "pure", "@system"));
    static assert(hasFunctionAttributes!(typeof(S2.pure_const), "const", "pure", "@system"));
    static assert(!hasFunctionAttributes!(S2.pure_const, "const", "pure", "@system", "ref"));

    static assert(hasFunctionAttributes!(S2.pure_sharedconst, "const", "shared", "pure", "@system"));
    static assert(hasFunctionAttributes!(typeof(S2.pure_sharedconst), "const", "shared", "pure", "@system"));
    static assert(!hasFunctionAttributes!(S2.pure_sharedconst, "const", "shared", "pure", "@system", "@nogc"));

    static assert(hasFunctionAttributes!((int a) { }, "pure", "nothrow", "@nogc", "@safe"));
    static assert(hasFunctionAttributes!(typeof((int a) { }), "pure", "nothrow", "@nogc", "@safe"));
    static assert(!hasFunctionAttributes!((int a) { }, "pure", "nothrow", "@nogc", "@safe", "ref"));

    auto safeDel = delegate() @safe { };
    static assert(hasFunctionAttributes!(safeDel, "pure", "nothrow", "@nogc", "@safe"));
    static assert(hasFunctionAttributes!(typeof(safeDel), "pure", "nothrow", "@nogc", "@safe"));
    static assert(!hasFunctionAttributes!(safeDel, "pure", "nothrow", "@nogc", "@safe", "@system"));

    auto trustedDel = delegate() @trusted { };
    static assert(hasFunctionAttributes!(trustedDel, "pure", "nothrow", "@nogc", "@trusted"));
    static assert(hasFunctionAttributes!(typeof(trustedDel), "pure", "nothrow", "@nogc", "@trusted"));
    static assert(!hasFunctionAttributes!(trustedDel, "pure", "nothrow", "@nogc", "@trusted", "ref"));

    auto systemDel = delegate() @system { };
    static assert(hasFunctionAttributes!(systemDel, "pure", "nothrow", "@nogc", "@system"));
    static assert(hasFunctionAttributes!(typeof(systemDel), "pure", "nothrow", "@nogc", "@system"));
    static assert(!hasFunctionAttributes!(systemDel, "pure", "nothrow", "@nogc", "@system", "@property"));


    // call functions to make CodeCov happy
    {
        assert(pure_nothrow == 0);
        safe_nothrow;
        assert(static_ref_property == 0);
        assert(ref_property == 0);
        assert(S2().pure_const == 0);
        assert((shared S2()).pure_sharedconst == 0);
        cast(void) safeDel;
        cast(void) trustedDel;
        cast(void) systemDel;
    }
}

/**
`true` if `func` is `@safe` or `@trusted`.
 */
template isSafe(alias func)
if (isCallable!func)
{
    enum isSafe = (functionAttributes!func & FunctionAttribute.safe) != 0 ||
                  (functionAttributes!func & FunctionAttribute.trusted) != 0;
}

///
@safe unittest
{
    @safe    int add(int a, int b) {return a+b;}
    @trusted int sub(int a, int b) {return a-b;}
    @system  int mul(int a, int b) {return a*b;}

    static assert( isSafe!add);
    static assert( isSafe!sub);
    static assert(!isSafe!mul);
}


@safe unittest
{
    //Member functions
    interface Set
    {
        int systemF() @system;
        int trustedF() @trusted;
        int safeF() @safe;
    }
    static assert( isSafe!(Set.safeF));
    static assert( isSafe!(Set.trustedF));
    static assert(!isSafe!(Set.systemF));

    //Functions
    @safe static void safeFunc() {}
    @trusted static void trustedFunc() {}
    @system static void systemFunc() {}

    static assert( isSafe!safeFunc);
    static assert( isSafe!trustedFunc);
    static assert(!isSafe!systemFunc);

    //Delegates
    auto safeDel = delegate() @safe {};
    auto trustedDel = delegate() @trusted {};
    auto systemDel = delegate() @system {};

    static assert( isSafe!safeDel);
    static assert( isSafe!trustedDel);
    static assert(!isSafe!systemDel);

    //Lambdas
    static assert( isSafe!({safeDel();}));
    static assert( isSafe!({trustedDel();}));
    static assert(!isSafe!({systemDel();}));

    //Static opCall
    struct SafeStatic { @safe static SafeStatic opCall() { return SafeStatic.init; } }
    struct TrustedStatic { @trusted static TrustedStatic opCall() { return TrustedStatic.init; } }
    struct SystemStatic { @system static SystemStatic opCall() { return SystemStatic.init; } }

    static assert( isSafe!(SafeStatic()));
    static assert( isSafe!(TrustedStatic()));
    static assert(!isSafe!(SystemStatic()));

    //Non-static opCall
    struct Safe { @safe Safe opCall() { return Safe.init; } }
    struct Trusted { @trusted Trusted opCall() { return Trusted.init; } }
    struct System { @system System opCall() { return System.init; } }

    static assert( isSafe!(Safe.init()));
    static assert( isSafe!(Trusted.init()));
    static assert(!isSafe!(System.init()));
}


/**
`true` if `func` is `@system`.
*/
template isUnsafe(alias func)
{
    enum isUnsafe = !isSafe!func;
}

///
@safe unittest
{
    @safe    int add(int a, int b) {return a+b;}
    @trusted int sub(int a, int b) {return a-b;}
    @system  int mul(int a, int b) {return a*b;}

    static assert(!isUnsafe!add);
    static assert(!isUnsafe!sub);
    static assert( isUnsafe!mul);
}

@safe unittest
{
    //Member functions
    interface Set
    {
        int systemF() @system;
        int trustedF() @trusted;
        int safeF() @safe;
    }
    static assert(!isUnsafe!(Set.safeF));
    static assert(!isUnsafe!(Set.trustedF));
    static assert( isUnsafe!(Set.systemF));

    //Functions
    @safe static void safeFunc() {}
    @trusted static void trustedFunc() {}
    @system static void systemFunc() {}

    static assert(!isUnsafe!safeFunc);
    static assert(!isUnsafe!trustedFunc);
    static assert( isUnsafe!systemFunc);

    //Delegates
    auto safeDel = delegate() @safe {};
    auto trustedDel = delegate() @trusted {};
    auto systemDel = delegate() @system {};

    static assert(!isUnsafe!safeDel);
    static assert(!isUnsafe!trustedDel);
    static assert( isUnsafe!systemDel);

    //Lambdas
    static assert(!isUnsafe!({safeDel();}));
    static assert(!isUnsafe!({trustedDel();}));
    static assert( isUnsafe!({systemDel();}));

    //Static opCall
    struct SafeStatic { @safe static SafeStatic opCall() { return SafeStatic.init; } }
    struct TrustedStatic { @trusted static TrustedStatic opCall() { return TrustedStatic.init; } }
    struct SystemStatic { @system static SystemStatic opCall() { return SystemStatic.init; } }

    static assert(!isUnsafe!(SafeStatic()));
    static assert(!isUnsafe!(TrustedStatic()));
    static assert( isUnsafe!(SystemStatic()));

    //Non-static opCall
    struct Safe { @safe Safe opCall() { return Safe.init; } }
    struct Trusted { @trusted Trusted opCall() { return Trusted.init; } }
    struct System { @system System opCall() { return System.init; } }

    static assert(!isUnsafe!(Safe.init()));
    static assert(!isUnsafe!(Trusted.init()));
    static assert( isUnsafe!(System.init()));
}


/**
Determine the linkage attribute of the function.
Params:
    func = the function symbol, or the type of a function, delegate, or pointer to function
Returns:
    one of the strings "D", "C", "C++", "Windows", "Objective-C", or "System".
*/
template functionLinkage(alias func)
if (isCallable!func)
{
    enum string functionLinkage = __traits(getLinkage, FunctionTypeOf!func);
}

///
@safe unittest
{
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
    interface Test
    {
        void const_func() const;
        void sharedconst_func() shared const;
    }
    static assert(functionLinkage!(Test.const_func) == "D");
    static assert(functionLinkage!(Test.sharedconst_func) == "D");

    static assert(functionLinkage!((int a){}) == "D");
}


/**
Determines what kind of variadic parameters function has.
Params:
    func = function symbol or type of function, delegate, or pointer to function
Returns:
    enum Variadic
 */
enum Variadic
{
    /// Function is not variadic.
    no,
    /// Function is a _C-style variadic function, which uses
    /// `core.stdc.stdarg`
    c,
    /// Function is a _D-style variadic function, which uses
    /// `__argptr` and `__arguments`.
    d,
    /// Function is a typesafe variadic function.
    typesafe,
}

/// ditto
template variadicFunctionStyle(alias func)
if (isCallable!func)
{
    enum string varargs = __traits(getFunctionVariadicStyle, FunctionTypeOf!func);
    enum Variadic variadicFunctionStyle =
        (varargs == "stdarg") ? Variadic.c :
        (varargs == "argptr") ? Variadic.d :
        (varargs == "typesafe") ? Variadic.typesafe :
        (varargs == "none") ? Variadic.no : Variadic.no;
}

///
@safe unittest
{
    void func() {}
    static assert(variadicFunctionStyle!func == Variadic.no);

    extern(C) int printf(const char*, ...);
    static assert(variadicFunctionStyle!printf == Variadic.c);
}

@safe unittest
{
    import core.vararg;

    extern(D) void novar() {}
    extern(C) void cstyle(int, ...) {}
    extern(D) void dstyle(...) {}
    extern(D) void typesafe(int[]...) {}

    static assert(variadicFunctionStyle!novar == Variadic.no);
    static assert(variadicFunctionStyle!cstyle == Variadic.c);
    static assert(variadicFunctionStyle!dstyle == Variadic.d);
    static assert(variadicFunctionStyle!typesafe == Variadic.typesafe);

    static assert(variadicFunctionStyle!((int[] a...) {}) == Variadic.typesafe);
}


/**
Get the function type from a callable object `func`.

Using builtin `typeof` on a property function yields the types of the
property value, not of the property function itself.  Still,
`FunctionTypeOf` is able to obtain function types of properties.

Note:
Do not confuse function types with function pointer types; function types are
usually used for compile-time reflection purposes.
 */
template FunctionTypeOf(alias func)
if (isCallable!func)
{
    static if ((is(typeof(& func) Fsym : Fsym*) && is(Fsym == function)) || is(typeof(& func) Fsym == delegate))
    {
        alias FunctionTypeOf = Fsym; // HIT: (nested) function symbol
    }
    else static if (is(typeof(& func.opCall) Fobj == delegate) || is(typeof(& func.opCall!()) Fobj == delegate))
    {
        alias FunctionTypeOf = Fobj; // HIT: callable object
    }
    else static if (
            (is(typeof(& func.opCall) Ftyp : Ftyp*) && is(Ftyp == function)) ||
            (is(typeof(& func.opCall!()) Ftyp : Ftyp*) && is(Ftyp == function))
        )
    {
        alias FunctionTypeOf = Ftyp; // HIT: callable type
    }
    else static if (is(func T) || is(typeof(func) T))
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

///
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

    struct TemplatedOpCallF { int opCall()(int) { return 0; } }
    static assert(is( FunctionTypeOf!TemplatedOpCallF == typeof(TemplatedOpCallF.opCall!()) ));

    int foovar;
    struct TemplatedOpCallDg { int opCall()() { return foovar; } }
    static assert(is( FunctionTypeOf!TemplatedOpCallDg == typeof(TemplatedOpCallDg.opCall!()) ));

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

/**
 * Constructs a new function or delegate type with the same basic signature
 * as the given one, but different attributes (including linkage).
 *
 * This is especially useful for adding/removing attributes to/from types in
 * generic code, where the actual type name cannot be spelt out.
 *
 * Params:
 *    T = The base type.
 *    linkage = The desired linkage of the result type.
 *    attrs = The desired $(LREF FunctionAttribute)s of the result type.
 */
template SetFunctionAttributes(T, string linkage, uint attrs)
if (isFunctionPointer!T || isDelegate!T)
{
    mixin({
        import std.algorithm.searching : canFind;

        static assert(!(attrs & FunctionAttribute.trusted) ||
            !(attrs & FunctionAttribute.safe),
            "Cannot have a function/delegate that is both trusted and safe.");

        static immutable linkages = ["D", "C", "Windows", "C++", "System"];
        static assert(canFind(linkages, linkage), "Invalid linkage '" ~
            linkage ~ "', must be one of " ~ linkages.stringof ~ ".");

        string result = "alias ";

        static if (linkage != "D")
            result ~= "extern(" ~ linkage ~ ") ";

        static if (attrs & FunctionAttribute.ref_)
            result ~= "ref ";

        result ~= "ReturnType!T";

        static if (isDelegate!T)
            result ~= " delegate";
        else
            result ~= " function";

        result ~= "(";

        static if (Parameters!T.length > 0)
            result ~= "Parameters!T";

        enum varStyle = variadicFunctionStyle!T;
        static if (varStyle == Variadic.c)
            result ~= ", ...";
        else static if (varStyle == Variadic.d)
            result ~= "...";
        else static if (varStyle == Variadic.typesafe)
            result ~= "...";

        result ~= ")";

        static if (attrs & FunctionAttribute.pure_)
            result ~= " pure";
        static if (attrs & FunctionAttribute.nothrow_)
            result ~= " nothrow";
        static if (attrs & FunctionAttribute.property)
            result ~= " @property";
        static if (attrs & FunctionAttribute.trusted)
            result ~= " @trusted";
        static if (attrs & FunctionAttribute.safe)
            result ~= " @safe";
        static if (attrs & FunctionAttribute.nogc)
            result ~= " @nogc";
        static if (attrs & FunctionAttribute.system)
            result ~= " @system";
        static if (attrs & FunctionAttribute.const_)
            result ~= " const";
        static if (attrs & FunctionAttribute.immutable_)
            result ~= " immutable";
        static if (attrs & FunctionAttribute.inout_)
            result ~= " inout";
        static if (attrs & FunctionAttribute.shared_)
            result ~= " shared";
        static if (attrs & FunctionAttribute.return_)
            result ~= " return";
        static if (attrs & FunctionAttribute.live)
            result ~= " @live";

        result ~= " SetFunctionAttributes;";
        return result;
    }());
}

/// Ditto
template SetFunctionAttributes(T, string linkage, uint attrs)
if (is(T == function))
{
    // To avoid a lot of syntactic headaches, we just use the above version to
    // operate on the corresponding function pointer type and then remove the
    // indirection again.
    alias SetFunctionAttributes = FunctionTypeOf!(SetFunctionAttributes!(T*, linkage, attrs));
}

///
@safe unittest
{
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

version (StdUnittest)
{
private:
    // Some function types to test.
    int sc(scope int, ref int, out int, lazy int, int);
    extern(System) int novar();
    extern(C) int cstyle(int, ...);
    extern(D) int dstyle(...);
    extern(D) int typesafe(int[]...);
}
@safe unittest
{
    import std.algorithm.iteration : reduce;

    alias FA = FunctionAttribute;
    static foreach (BaseT; AliasSeq!(typeof(&sc), typeof(&novar), typeof(&cstyle),
        typeof(&dstyle), typeof(&typesafe)))
    {
        static foreach (T; AliasSeq!(BaseT, FunctionTypeOf!BaseT))
        {{
            enum linkage = functionLinkage!T;
            enum attrs = functionAttributes!T;

            static assert(is(SetFunctionAttributes!(T, linkage, attrs) == T),
                "Identity check failed for: " ~ T.stringof);

            // Check that all linkage types work (D-style variadics require D linkage).
            static if (variadicFunctionStyle!T != Variadic.d)
            {
                static foreach (newLinkage; AliasSeq!("D", "C", "Windows", "C++"))
                {{
                    alias New = SetFunctionAttributes!(T, newLinkage, attrs);
                    static assert(functionLinkage!New == newLinkage,
                        "Linkage test failed for: " ~ T.stringof ~ ", " ~ newLinkage ~
                        " (got " ~ New.stringof ~ ")");
                }}
            }

            // Add @safe.
            alias T1 = SetFunctionAttributes!(T, functionLinkage!T, FA.safe);
            static assert(functionAttributes!T1 == FA.safe);

            // Add all known attributes, excluding conflicting ones.
            enum allAttrs = reduce!"a | b"([EnumMembers!FA])
                & ~FA.safe & ~FA.property & ~FA.const_ & ~FA.immutable_ & ~FA.inout_
                & ~FA.shared_ & ~FA.system & ~FA.return_ & ~FA.scope_;

            alias T2 = SetFunctionAttributes!(T1, functionLinkage!T, allAttrs);
            static assert(functionAttributes!T2 == allAttrs);

            // Strip all attributes again.
            alias T3 = SetFunctionAttributes!(T2, functionLinkage!T, FA.none);
            static assert(is(T3 == T));
        }}
    }
}


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Aggregate Types
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
Determines whether `T` is a class nested inside another class
and that `T.outer` is the implicit reference to the outer class
(i.e. `outer` has not been used as a field or method name)

Params:
    T = type to test

Returns:
`true` if `T` is a class nested inside another, with the conditions described above;
`false` otherwise
*/
template isInnerClass(T)
if (is(T == class))
{
    static if (is(typeof(T.outer)))
    {
        bool hasOuterMember(string[] members...)
        {
            foreach (m; members)
            {
                if (m == "outer")
                    return true;
            }
            return false;
        }
        enum isInnerClass = __traits(isSame, typeof(T.outer), __traits(parent, T)) &&
                            !hasOuterMember(__traits(allMembers, T));
    }
    else
        enum isInnerClass = false;
}

///
@safe unittest
{
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

/**
Determines whether `T` has its own context pointer.
`T` must be either `class`, `struct`, or `union`.

See also: $(DDSUBLINK spec/traits, isNested, `__traits(isNested, T)`)
*/
template isNested(T)
if (is(T == class) || is(T == struct) || is(T == union))
{
    enum isNested = __traits(isNested, T);
}

///
@safe unittest
{
    static struct S { }
    static assert(!isNested!S);

    int i;
    struct NestedStruct { void f() { ++i; } }
    static assert(isNested!NestedStruct);
}

/**
Determines whether `T` or any of its representation types
have a context pointer.
*/
template hasNested(T)
{
    import std.meta : Filter;

    static if (isStaticArray!T && T.length)
        enum hasNested = hasNested!(typeof(T.init[0]));
    else static if (is(T == class) || is(T == struct) || is(T == union))
    {
        // prevent infinite recursion for class with member of same type
        enum notSame(U) = !is(immutable T == immutable U);
        enum hasNested = isNested!T ||
            anySatisfy!(.hasNested, Filter!(notSame, Fields!T));
    }
    else
        enum hasNested = false;
}

///
@safe unittest
{
    static struct S { }

    int i;
    struct NS { void f() { ++i; } }

    static assert(!hasNested!(S[2]));
    static assert(hasNested!(NS[2]));
}

@safe unittest
{
    static assert(!__traits(compiles, isNested!int));
    static assert(!hasNested!int);

    static struct StaticStruct { }
    static assert(!isNested!StaticStruct);
    static assert(!hasNested!StaticStruct);

    int i;
    struct NestedStruct { void f() { ++i; } }
    static assert( isNested!NestedStruct);
    static assert( hasNested!NestedStruct);
    static assert( isNested!(immutable NestedStruct));
    static assert( hasNested!(immutable NestedStruct));

    static assert(!__traits(compiles, isNested!(NestedStruct[1])));
    static assert( hasNested!(NestedStruct[1]));
    static assert(!hasNested!(NestedStruct[0]));

    struct S1 { NestedStruct nested; }
    static assert(!isNested!S1);
    static assert( hasNested!S1);

    static struct S2 { NestedStruct nested; }
    static assert(!isNested!S2);
    static assert( hasNested!S2);

    static struct S3 { NestedStruct[0] nested; }
    static assert(!isNested!S3);
    static assert(!hasNested!S3);

    static union U { NestedStruct nested; }
    static assert(!isNested!U);
    static assert( hasNested!U);

    static class StaticClass { }
    static assert(!isNested!StaticClass);
    static assert(!hasNested!StaticClass);

    class NestedClass { void f() { ++i; } }
    static assert( isNested!NestedClass);
    static assert( hasNested!NestedClass);
    static assert( isNested!(immutable NestedClass));
    static assert( hasNested!(immutable NestedClass));

    static assert(!__traits(compiles, isNested!(NestedClass[1])));
    static assert( hasNested!(NestedClass[1]));
    static assert(!hasNested!(NestedClass[0]));

    static class A
    {
        A a;
    }
    static assert(!hasNested!A);
}


/***
 * Get as a tuple the types of the fields of a struct, class, or union.
 * This consists of the fields that take up memory space,
 * excluding the hidden fields like the virtual function
 * table pointer or a context pointer for nested types.
 * If `T` isn't a struct, class, interface or union returns a tuple
 * with one element `T`.
 *
 * History:
 *   - Returned `AliasSeq!(Interface)` for interfaces prior to 2.097
 */
template Fields(T)
{
    import core.internal.traits : _Fields = Fields;
    alias Fields = _Fields!T;
}

///
@safe unittest
{
    import std.meta : AliasSeq;
    struct S { int x; float y; }
    static assert(is(Fields!S == AliasSeq!(int, float)));
}

/**
 * Alternate name for $(LREF Fields), kept for legacy compatibility.
 */
alias FieldTypeTuple = Fields;

@safe unittest
{
    static assert(is(FieldTypeTuple!int == AliasSeq!int));

    static struct StaticStruct1 { }
    static assert(is(FieldTypeTuple!StaticStruct1 == AliasSeq!()));

    static struct StaticStruct2 { int a, b; }
    static assert(is(FieldTypeTuple!StaticStruct2 == AliasSeq!(int, int)));

    int i;

    struct NestedStruct1 { void f() { ++i; } }
    static assert(is(FieldTypeTuple!NestedStruct1 == AliasSeq!()));

    struct NestedStruct2 { int a; void f() { ++i; } }
    static assert(is(FieldTypeTuple!NestedStruct2 == AliasSeq!int));

    class NestedClass { int a; void f() { ++i; } }
    static assert(is(FieldTypeTuple!NestedClass == AliasSeq!int));

    static interface I {}
    static assert(is(Fields!I == AliasSeq!()));
}

//Required for FieldNameTuple
private enum NameOf(alias T) = T.stringof;

/**
 * Get as an expression tuple the names of the fields of a struct, class, or
 * union. This consists of the fields that take up memory space, excluding the
 * hidden fields like the virtual function table pointer or a context pointer
 * for nested types.
 * Inherited fields (for classes) are not included.
 * If `T` isn't a struct, class, interface or union, an
 * expression tuple with an empty string is returned.
 *
 * History:
 *   - Returned `AliasSeq!""` for interfaces prior to 2.097
 */
template FieldNameTuple(T)
{
    import std.meta : staticMap;
    static if (is(T == struct) || is(T == union))
        alias FieldNameTuple = staticMap!(NameOf, T.tupleof[0 .. $ - isNested!T]);
    else static if (is(T == class) || is(T == interface))
        alias FieldNameTuple = staticMap!(NameOf, T.tupleof);
    else
        alias FieldNameTuple = AliasSeq!"";
}

///
@safe unittest
{
    import std.meta : AliasSeq;
    struct S { int x; float y; }
    static assert(FieldNameTuple!S == AliasSeq!("x", "y"));
    static assert(FieldNameTuple!int == AliasSeq!"");
}

@safe unittest
{
    static assert(FieldNameTuple!int == AliasSeq!"");

    static struct StaticStruct1 { }
    static assert(is(FieldNameTuple!StaticStruct1 == AliasSeq!()));

    static struct StaticStruct2 { int a, b; }
    static assert(FieldNameTuple!StaticStruct2 == AliasSeq!("a", "b"));

    static class StaticClass1 { }
    static assert(is(FieldNameTuple!StaticClass1 == AliasSeq!()));

    static class StaticClass2 : StaticClass1 { int a, b; }
    static assert(FieldNameTuple!StaticClass2 == AliasSeq!("a", "b"));

    static class StaticClass3 : StaticClass2 { int c; }
    static assert(FieldNameTuple!StaticClass3 == AliasSeq!("c"));

    int i;

    struct NestedStruct1 { void f() { ++i; } }
    static assert(is(FieldNameTuple!NestedStruct1 == AliasSeq!()));

    struct NestedStruct2 { int a; void f() { ++i; } }
    static assert(FieldNameTuple!NestedStruct2 == AliasSeq!"a");

    class NestedClass { int a; void f() { ++i; } }
    static assert(FieldNameTuple!NestedClass == AliasSeq!"a");

    interface I {}
    static assert(FieldNameTuple!I == AliasSeq!());
}


/***
Get the primitive types of the fields of a struct or class, in
topological order.
*/
template RepresentationTypeTuple(T)
{
    static if (is(T == struct) || is(T == union) || is(T == class))
    {
        alias RepresentationTypeTuple = staticMapMeta!(RepresentationTypeTupleImpl, FieldTypeTuple!T);
    }
    else
    {
        alias RepresentationTypeTuple = RepresentationTypeTupleImpl!T;
    }
}

///
@safe unittest
{
    struct S1 { int a; float b; }
    struct S2 { char[] a; union { S1 b; S1 * c; } }
    alias R = RepresentationTypeTuple!S2;
    assert(R.length == 4
        && is(R[0] == char[]) && is(R[1] == int)
        && is(R[2] == float) && is(R[3] == S1*));
}

@safe unittest
{
    alias S1 = RepresentationTypeTuple!int;
    static assert(is(S1 == AliasSeq!int));

    struct S2 { int a; }
    struct S3 { int a; char b; }
    struct S4 { S1 a; int b; S3 c; }
    static assert(is(RepresentationTypeTuple!S2 == AliasSeq!int));
    static assert(is(RepresentationTypeTuple!S3 == AliasSeq!(int, char)));
    static assert(is(RepresentationTypeTuple!S4 == AliasSeq!(int, int, int, char)));

    struct S11 { int a; float b; }
    struct S21 { char[] a; union { S11 b; S11 * c; } }
    alias R = RepresentationTypeTuple!S21;
    assert(R.length == 4
           && is(R[0] == char[]) && is(R[1] == int)
           && is(R[2] == float) && is(R[3] == S11*));

    class C { int a; float b; }
    alias R1 = RepresentationTypeTuple!C;
    static assert(R1.length == 2 && is(R1[0] == int) && is(R1[1] == float));

    /* https://issues.dlang.org/show_bug.cgi?id=6642 */
    import std.typecons : Rebindable;

    struct S5 { int a; Rebindable!(immutable Object) b; }
    alias R2 = RepresentationTypeTuple!S5;
    static assert(R2.length == 2 && is(R2[0] == int) && is(R2[1] == immutable(Object)));

    static assert(is(RepresentationTypeTuple!noreturn == AliasSeq!noreturn));
}

@safe unittest
{
    struct VeryLargeType
    {
        import std.format : format;
        import std.range : iota;

        static foreach (i; 500.iota)
        {
            mixin(format!"int v%s;"(i));
        }
    }

    alias BigList = RepresentationTypeTuple!VeryLargeType;
}

private template RepresentationTypeTupleImpl(T)
{
    import std.typecons : Rebindable;

    static if (is(immutable T == immutable Rebindable!R, R))
    {
        alias RepresentationTypeTupleImpl
            = staticMapMeta!(.RepresentationTypeTupleImpl, RepresentationTypeTupleImpl!R);
    }
    else  static if (is(T == struct) || is(T == union))
    {
        // @@@BUG@@@ this should work
        //alias .RepresentationTypes!(T[0].tupleof)
        //    RepresentationTypes;
        alias RepresentationTypeTupleImpl
            = staticMapMeta!(.RepresentationTypeTupleImpl, FieldTypeTuple!(T));
    }
    else
    {
        alias RepresentationTypeTupleImpl
            = AliasSeq!T;
    }
}

/*
Statically evaluates to `true` if and only if `T`'s
representation contains at least one field of pointer or array type.
Members of class types are not considered raw pointers. Pointers to
immutable objects are not considered raw aliasing.
*/
private template hasRawAliasing(T)
{
    enum hasRawAliasing = anySatisfy!(hasRawAliasingImpl, RepresentationTypeTuple!T);
}

//
@safe unittest
{
    // simple types
    static assert(!hasRawAliasing!int);
    static assert( hasRawAliasing!(char*));
    // references aren't raw pointers
    static assert(!hasRawAliasing!Object);
    // built-in arrays do contain raw pointers
    static assert( hasRawAliasing!(int[]));
    // aggregate of simple types
    struct S1 { int a; double b; }
    static assert(!hasRawAliasing!S1);
    // indirect aggregation
    struct S2 { S1 a; double b; }
    static assert(!hasRawAliasing!S2);
}

// https://issues.dlang.org/show_bug.cgi?id=19228
@safe unittest
{
    static struct C
    {
        int*[1] a;
    }
    static assert(hasRawAliasing!C);
}

@safe unittest
{
    // struct with a pointer member
    struct S3 { int a; double * b; }
    static assert( hasRawAliasing!S3);
    // struct with an indirect pointer member
    struct S4 { S3 a; double b; }
    static assert( hasRawAliasing!S4);
    struct S5 { int a; Object z; int c; }
    static assert( hasRawAliasing!S3);
    static assert( hasRawAliasing!S4);
    static assert(!hasRawAliasing!S5);

    union S6 { int a; int b; }
    union S7 { int a; int * b; }
    static assert(!hasRawAliasing!S6);
    static assert( hasRawAliasing!S7);

    static assert(!hasRawAliasing!(void delegate()));
    static assert(!hasRawAliasing!(void delegate() const));
    static assert(!hasRawAliasing!(void delegate() immutable));
    static assert(!hasRawAliasing!(void delegate() shared));
    static assert(!hasRawAliasing!(void delegate() shared const));
    static assert(!hasRawAliasing!(const(void delegate())));
    static assert(!hasRawAliasing!(immutable(void delegate())));

    struct S8 { void delegate() a; int b; Object c; }
    class S12 { typeof(S8.tupleof) a; }
    class S13 { typeof(S8.tupleof) a; int* b; }
    static assert(!hasRawAliasing!S8);
    static assert(!hasRawAliasing!S12);
    static assert( hasRawAliasing!S13);

    enum S9 { a }
    static assert(!hasRawAliasing!S9);

    // indirect members
    struct S10 { S7 a; int b; }
    struct S11 { S6 a; int b; }
    static assert( hasRawAliasing!S10);
    static assert(!hasRawAliasing!S11);

    static assert( hasRawAliasing!(int[string]));
    static assert(!hasRawAliasing!(immutable(int[string])));
}

private template hasRawAliasingImpl(T)
{
    static if (is(T foo : U*, U) && !isFunctionPointer!T)
        enum hasRawAliasingImpl = !is(U == immutable);
    else static if (is(T foo : U[N], U, size_t N))
        // separate static ifs to avoid forward reference
        static if (is(U == class) || is(U == interface))
            enum hasRawAliasingImpl = false;
        else
            enum hasRawAliasingImpl = hasRawAliasingImpl!U;
    else static if (is(T foo : U[], U) && !isStaticArray!(T))
        enum hasRawAliasingImpl = !is(U == immutable);
    else static if (isAssociativeArray!(T))
        enum hasRawAliasingImpl = !is(T == immutable);
    else
        enum hasRawAliasingImpl = false;
}

/*
Statically evaluates to `true` if and only if `T`'s
representation contains at least one non-shared field of pointer or
array type.  Members of class types are not considered raw pointers.
Pointers to immutable objects are not considered raw aliasing.
*/
private template hasRawUnsharedAliasing(T)
{
    enum hasRawUnsharedAliasing = anySatisfy!(hasRawUnsharedAliasingImpl, RepresentationTypeTuple!T);
}

//
@safe unittest
{
    // simple types
    static assert(!hasRawUnsharedAliasing!int);
    static assert( hasRawUnsharedAliasing!(char*));
    static assert(!hasRawUnsharedAliasing!(shared char*));
    // references aren't raw pointers
    static assert(!hasRawUnsharedAliasing!Object);
    // built-in arrays do contain raw pointers
    static assert( hasRawUnsharedAliasing!(int[]));
    static assert(!hasRawUnsharedAliasing!(shared int[]));
    // aggregate of simple types
    struct S1 { int a; double b; }
    static assert(!hasRawUnsharedAliasing!S1);
    // indirect aggregation
    struct S2 { S1 a; double b; }
    static assert(!hasRawUnsharedAliasing!S2);
    // struct with a pointer member
    struct S3 { int a; double * b; }
    static assert( hasRawUnsharedAliasing!S3);
    struct S4 { int a; shared double * b; }
    static assert(!hasRawUnsharedAliasing!S4);
}

@safe unittest
{
    // struct with a pointer member
    struct S3 { int a; double * b; }
    static assert( hasRawUnsharedAliasing!S3);
    struct S4 { int a; shared double * b; }
    static assert(!hasRawUnsharedAliasing!S4);
    // struct with an indirect pointer member
    struct S5 { S3 a; double b; }
    static assert( hasRawUnsharedAliasing!S5);
    struct S6 { S4 a; double b; }
    static assert(!hasRawUnsharedAliasing!S6);
    struct S7 { int a; Object z;      int c; }
    static assert( hasRawUnsharedAliasing!S5);
    static assert(!hasRawUnsharedAliasing!S6);
    static assert(!hasRawUnsharedAliasing!S7);

    union S8  { int a; int b; }
    union S9  { int a; int* b; }
    union S10 { int a; shared int* b; }
    static assert(!hasRawUnsharedAliasing!S8);
    static assert( hasRawUnsharedAliasing!S9);
    static assert(!hasRawUnsharedAliasing!S10);

    static assert(!hasRawUnsharedAliasing!(void delegate()));
    static assert(!hasRawUnsharedAliasing!(void delegate() const));
    static assert(!hasRawUnsharedAliasing!(void delegate() immutable));
    static assert(!hasRawUnsharedAliasing!(void delegate() shared));
    static assert(!hasRawUnsharedAliasing!(void delegate() shared const));
    static assert(!hasRawUnsharedAliasing!(const(void delegate())));
    static assert(!hasRawUnsharedAliasing!(const(void delegate() const)));
    static assert(!hasRawUnsharedAliasing!(const(void delegate() immutable)));
    static assert(!hasRawUnsharedAliasing!(const(void delegate() shared)));
    static assert(!hasRawUnsharedAliasing!(const(void delegate() shared const)));
    static assert(!hasRawUnsharedAliasing!(immutable(void delegate())));
    static assert(!hasRawUnsharedAliasing!(immutable(void delegate() const)));
    static assert(!hasRawUnsharedAliasing!(immutable(void delegate() immutable)));
    static assert(!hasRawUnsharedAliasing!(immutable(void delegate() shared)));
    static assert(!hasRawUnsharedAliasing!(immutable(void delegate() shared const)));
    static assert(!hasRawUnsharedAliasing!(shared(void delegate())));
    static assert(!hasRawUnsharedAliasing!(shared(void delegate() const)));
    static assert(!hasRawUnsharedAliasing!(shared(void delegate() immutable)));
    static assert(!hasRawUnsharedAliasing!(shared(void delegate() shared)));
    static assert(!hasRawUnsharedAliasing!(shared(void delegate() shared const)));
    static assert(!hasRawUnsharedAliasing!(shared(const(void delegate()))));
    static assert(!hasRawUnsharedAliasing!(shared(const(void delegate() const))));
    static assert(!hasRawUnsharedAliasing!(shared(const(void delegate() immutable))));
    static assert(!hasRawUnsharedAliasing!(shared(const(void delegate() shared))));
    static assert(!hasRawUnsharedAliasing!(shared(const(void delegate() shared const))));
    static assert(!hasRawUnsharedAliasing!(void function()));

    enum S13 { a }
    static assert(!hasRawUnsharedAliasing!S13);

    // indirect members
    struct S14 { S9  a; int b; }
    struct S15 { S10 a; int b; }
    struct S16 { S6  a; int b; }
    static assert( hasRawUnsharedAliasing!S14);
    static assert(!hasRawUnsharedAliasing!S15);
    static assert(!hasRawUnsharedAliasing!S16);

    static assert( hasRawUnsharedAliasing!(int[string]));
    static assert(!hasRawUnsharedAliasing!(shared(int[string])));
    static assert(!hasRawUnsharedAliasing!(immutable(int[string])));

    struct S17
    {
        void delegate() shared a;
        void delegate() immutable b;
        void delegate() shared const c;
        shared(void delegate()) d;
        shared(void delegate() shared) e;
        shared(void delegate() immutable) f;
        shared(void delegate() shared const) g;
        immutable(void delegate()) h;
        immutable(void delegate() shared) i;
        immutable(void delegate() immutable) j;
        immutable(void delegate() shared const) k;
        shared(const(void delegate())) l;
        shared(const(void delegate() shared)) m;
        shared(const(void delegate() immutable)) n;
        shared(const(void delegate() shared const)) o;
    }
    struct S18 { typeof(S17.tupleof) a; void delegate() p; }
    struct S19 { typeof(S17.tupleof) a; Object p; }
    struct S20 { typeof(S17.tupleof) a; int* p; }
    class S21 { typeof(S17.tupleof) a; }
    class S22 { typeof(S17.tupleof) a; void delegate() p; }
    class S23 { typeof(S17.tupleof) a; Object p; }
    class S24 { typeof(S17.tupleof) a; int* p; }
    static assert(!hasRawUnsharedAliasing!S17);
    static assert(!hasRawUnsharedAliasing!(immutable(S17)));
    static assert(!hasRawUnsharedAliasing!(shared(S17)));
    static assert(!hasRawUnsharedAliasing!S18);
    static assert(!hasRawUnsharedAliasing!(immutable(S18)));
    static assert(!hasRawUnsharedAliasing!(shared(S18)));
    static assert(!hasRawUnsharedAliasing!S19);
    static assert(!hasRawUnsharedAliasing!(immutable(S19)));
    static assert(!hasRawUnsharedAliasing!(shared(S19)));
    static assert( hasRawUnsharedAliasing!S20);
    static assert(!hasRawUnsharedAliasing!(immutable(S20)));
    static assert(!hasRawUnsharedAliasing!(shared(S20)));
    static assert(!hasRawUnsharedAliasing!S21);
    static assert(!hasRawUnsharedAliasing!(immutable(S21)));
    static assert(!hasRawUnsharedAliasing!(shared(S21)));
    static assert(!hasRawUnsharedAliasing!S22);
    static assert(!hasRawUnsharedAliasing!(immutable(S22)));
    static assert(!hasRawUnsharedAliasing!(shared(S22)));
    static assert(!hasRawUnsharedAliasing!S23);
    static assert(!hasRawUnsharedAliasing!(immutable(S23)));
    static assert(!hasRawUnsharedAliasing!(shared(S23)));
    static assert( hasRawUnsharedAliasing!S24);
    static assert(!hasRawUnsharedAliasing!(immutable(S24)));
    static assert(!hasRawUnsharedAliasing!(shared(S24)));
    struct S25 {}
    class S26 {}
    interface S27 {}
    union S28 {}
    static assert(!hasRawUnsharedAliasing!S25);
    static assert(!hasRawUnsharedAliasing!S26);
    static assert(!hasRawUnsharedAliasing!S27);
    static assert(!hasRawUnsharedAliasing!S28);
}

private template hasRawUnsharedAliasingImpl(T)
{
    static if (is(T foo : U*, U) && !isFunctionPointer!T)
        enum hasRawUnsharedAliasingImpl = !is(U == immutable) && !is(U == shared);
    else static if (is(T foo : U[], U) && !isStaticArray!T)
        enum hasRawUnsharedAliasingImpl = !is(U == immutable) && !is(U == shared);
    else static if (isAssociativeArray!T)
        enum hasRawUnsharedAliasingImpl = !is(T == immutable) && !is(T == shared);
    else
        enum hasRawUnsharedAliasingImpl = false;
}

/*
Statically evaluates to `true` if and only if `T`'s
representation includes at least one non-immutable object reference.
*/

private template hasObjects(T)
{
    static if (is(T == struct))
    {
        enum hasObjects = anySatisfy!(.hasObjects, RepresentationTypeTuple!T);
    }
    else
    {
        enum hasObjects = (is(T == class) || is(T == interface)) && !is(T == immutable);
    }
}

/*
Statically evaluates to `true` if and only if `T`'s
representation includes at least one non-immutable non-shared object
reference.
*/
private template hasUnsharedObjects(T)
{
    static if (is(T == struct))
    {
        enum hasUnsharedObjects = anySatisfy!(.hasUnsharedObjects, RepresentationTypeTuple!T);
    }
    else
    {
        enum hasUnsharedObjects = (is(T == class) || is(T == interface)) &&
                                  !is(T == immutable) && !is(T == shared);
    }
}

/**
Returns `true` if and only if `T`'s representation includes at
least one of the following: $(OL $(LI a raw pointer `U*` and `U`
is not immutable;) $(LI an array `U[]` and `U` is not
immutable;) $(LI a reference to a class or interface type `C` and `C` is
not immutable.) $(LI an associative array that is not immutable.)
$(LI a delegate.))
*/
template hasAliasing(T...)
{
    enum hasAliasing = anySatisfy!(hasAliasingImpl, T);
}

///
@safe unittest
{
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
    static assert( hasAliasing!(uint[uint]));
    static assert(!hasAliasing!(immutable(uint[uint])));
    static assert( hasAliasing!(void delegate()));
    static assert( hasAliasing!(void delegate() const));
    static assert(!hasAliasing!(void delegate() immutable));
    static assert( hasAliasing!(void delegate() shared));
    static assert( hasAliasing!(void delegate() shared const));
    static assert( hasAliasing!(const(void delegate())));
    static assert( hasAliasing!(const(void delegate() const)));
    static assert(!hasAliasing!(const(void delegate() immutable)));
    static assert( hasAliasing!(const(void delegate() shared)));
    static assert( hasAliasing!(const(void delegate() shared const)));
    static assert(!hasAliasing!(immutable(void delegate())));
    static assert(!hasAliasing!(immutable(void delegate() const)));
    static assert(!hasAliasing!(immutable(void delegate() immutable)));
    static assert(!hasAliasing!(immutable(void delegate() shared)));
    static assert(!hasAliasing!(immutable(void delegate() shared const)));
    static assert( hasAliasing!(shared(const(void delegate()))));
    static assert( hasAliasing!(shared(const(void delegate() const))));
    static assert(!hasAliasing!(shared(const(void delegate() immutable))));
    static assert( hasAliasing!(shared(const(void delegate() shared))));
    static assert( hasAliasing!(shared(const(void delegate() shared const))));
    static assert(!hasAliasing!(void function()));

    interface I;
    static assert( hasAliasing!I);

    import std.typecons : Rebindable;
    static assert( hasAliasing!(Rebindable!(const Object)));
    static assert(!hasAliasing!(Rebindable!(immutable Object)));
    static assert( hasAliasing!(Rebindable!(shared Object)));
    static assert( hasAliasing!(Rebindable!Object));

    struct S5
    {
        void delegate() immutable b;
        shared(void delegate() immutable) f;
        immutable(void delegate() immutable) j;
        shared(const(void delegate() immutable)) n;
    }
    struct S6 { typeof(S5.tupleof) a; void delegate() p; }
    static assert(!hasAliasing!S5);
    static assert( hasAliasing!S6);

    struct S7 { void delegate() a; int b; Object c; }
    class S8 { int a; int b; }
    class S9 { typeof(S8.tupleof) a; }
    class S10 { typeof(S8.tupleof) a; int* b; }
    static assert( hasAliasing!S7);
    static assert( hasAliasing!S8);
    static assert( hasAliasing!S9);
    static assert( hasAliasing!S10);
    struct S11 {}
    class S12 {}
    interface S13 {}
    union S14 {}
    static assert(!hasAliasing!S11);
    static assert( hasAliasing!S12);
    static assert( hasAliasing!S13);
    static assert(!hasAliasing!S14);

    class S15 { S15[1] a; }
    static assert( hasAliasing!S15);
    static assert(!hasAliasing!(immutable(S15)));

    static assert(!hasAliasing!noreturn);
}

private template hasAliasingImpl(T)
{
    import std.typecons : Rebindable;

    static if (is(immutable T == immutable Rebindable!R, R))
    {
        enum hasAliasingImpl = hasAliasingImpl!R;
    }
    else
    {
        template isAliasingDelegate(T)
        {
            enum isAliasingDelegate = isDelegate!T
                                  && !is(T == immutable)
                                  && !is(FunctionTypeOf!T == immutable);
        }
        enum hasAliasingImpl = hasRawAliasing!T || hasObjects!T ||
            anySatisfy!(isAliasingDelegate, T, RepresentationTypeTuple!T);
    }
}

/**
Returns `true` if and only if `T`'s representation includes at
least one of the following: $(OL $(LI a raw pointer `U*`;) $(LI an
array `U[]`;) $(LI a reference to a class type `C`;)
$(LI an associative array;) $(LI a delegate;)
$(LI a [context pointer][isNested].))
 */
template hasIndirections(T)
{
    import core.internal.traits : _hasIndirections = hasIndirections;
    alias hasIndirections = _hasIndirections!T;
}

///
@safe unittest
{
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
    // void static array hides actual type of bits, so "may have indirections".
    static assert( hasIndirections!(void[1]));
    interface I {}
    struct S1 {}
    struct S2 { int a; }
    struct S3 { int a; int b; }
    struct S4 { int a; int* b; }
    struct S5 { int a; Object b; }
    struct S6 { int a; string b; }
    struct S7 { int a; immutable Object b; }
    struct S8 { int a; immutable I b; }
    struct S9 { int a; void delegate() b; }
    struct S10 { int a; immutable(void delegate()) b; }
    struct S11 { int a; void delegate() immutable b; }
    struct S12 { int a; immutable(void delegate() immutable) b; }
    class S13 {}
    class S14 { int a; }
    class S15 { int a; int b; }
    class S16 { int a; Object b; }
    class S17 { string a; }
    class S18 { int a; immutable Object b; }
    class S19 { int a; immutable(void delegate() immutable) b; }
    union S20 {}
    union S21 { int a; }
    union S22 { int a; int b; }
    union S23 { int a; Object b; }
    union S24 { string a; }
    union S25 { int a; immutable Object b; }
    union S26 { int a; immutable(void delegate() immutable) b; }
    static assert( hasIndirections!I);
    static assert(!hasIndirections!S1);
    static assert(!hasIndirections!S2);
    static assert(!hasIndirections!S3);
    static assert( hasIndirections!S4);
    static assert( hasIndirections!S5);
    static assert( hasIndirections!S6);
    static assert( hasIndirections!S7);
    static assert( hasIndirections!S8);
    static assert( hasIndirections!S9);
    static assert( hasIndirections!S10);
    static assert( hasIndirections!S12);
    static assert( hasIndirections!S13);
    static assert( hasIndirections!S14);
    static assert( hasIndirections!S15);
    static assert( hasIndirections!S16);
    static assert( hasIndirections!S17);
    static assert( hasIndirections!S18);
    static assert( hasIndirections!S19);
    static assert(!hasIndirections!S20);
    static assert(!hasIndirections!S21);
    static assert(!hasIndirections!S22);
    static assert( hasIndirections!S23);
    static assert( hasIndirections!S24);
    static assert( hasIndirections!S25);
    static assert( hasIndirections!S26);
    int local;
    struct HasContextPointer { int opCall() { return ++local; } }
    static assert(hasIndirections!HasContextPointer);

    static assert(!hasIndirections!noreturn);
}

// https://issues.dlang.org/show_bug.cgi?id=12000
@safe unittest
{
    static struct S(T)
    {
        static assert(hasIndirections!T);
    }

    static class A(T)
    {
        S!A a;
    }

    A!int dummy;
}

/**
Returns `true` if and only if `T`'s representation includes at
least one of the following: $(OL $(LI a raw pointer `U*` and `U`
is not immutable or shared;) $(LI an array `U[]` and `U` is not
immutable or shared;) $(LI a reference to a class type `C` and
`C` is not immutable or shared.) $(LI an associative array that is not
immutable or shared.) $(LI a delegate that is not shared.))
*/

template hasUnsharedAliasing(T...)
{
    enum hasUnsharedAliasing = anySatisfy!(hasUnsharedAliasingImpl, T);
}

///
@safe unittest
{
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
    /* https://issues.dlang.org/show_bug.cgi?id=6642 */
    import std.typecons : Rebindable;
    struct S8 { int a; Rebindable!(immutable Object) b; }
    static assert(!hasUnsharedAliasing!S8);

    static assert( hasUnsharedAliasing!(uint[uint]));

    static assert( hasUnsharedAliasing!(void delegate()));
    static assert( hasUnsharedAliasing!(void delegate() const));
    static assert(!hasUnsharedAliasing!(void delegate() immutable));
    static assert(!hasUnsharedAliasing!(void delegate() shared));
    static assert(!hasUnsharedAliasing!(void delegate() shared const));
}

@safe unittest
{
    import std.typecons : Rebindable;
    static assert( hasUnsharedAliasing!(const(void delegate())));
    static assert( hasUnsharedAliasing!(const(void delegate() const)));
    static assert(!hasUnsharedAliasing!(const(void delegate() immutable)));
    static assert(!hasUnsharedAliasing!(const(void delegate() shared)));
    static assert(!hasUnsharedAliasing!(const(void delegate() shared const)));
    static assert(!hasUnsharedAliasing!(immutable(void delegate())));
    static assert(!hasUnsharedAliasing!(immutable(void delegate() const)));
    static assert(!hasUnsharedAliasing!(immutable(void delegate() immutable)));
    static assert(!hasUnsharedAliasing!(immutable(void delegate() shared)));
    static assert(!hasUnsharedAliasing!(immutable(void delegate() shared const)));
    static assert(!hasUnsharedAliasing!(shared(void delegate())));
    static assert(!hasUnsharedAliasing!(shared(void delegate() const)));
    static assert(!hasUnsharedAliasing!(shared(void delegate() immutable)));
    static assert(!hasUnsharedAliasing!(shared(void delegate() shared)));
    static assert(!hasUnsharedAliasing!(shared(void delegate() shared const)));
    static assert(!hasUnsharedAliasing!(shared(const(void delegate()))));
    static assert(!hasUnsharedAliasing!(shared(const(void delegate() const))));
    static assert(!hasUnsharedAliasing!(shared(const(void delegate() immutable))));
    static assert(!hasUnsharedAliasing!(shared(const(void delegate() shared))));
    static assert(!hasUnsharedAliasing!(shared(const(void delegate() shared const))));
    static assert(!hasUnsharedAliasing!(void function()));

    interface I {}
    static assert(hasUnsharedAliasing!I);

    static assert( hasUnsharedAliasing!(Rebindable!(const Object)));
    static assert(!hasUnsharedAliasing!(Rebindable!(immutable Object)));
    static assert(!hasUnsharedAliasing!(Rebindable!(shared Object)));
    static assert( hasUnsharedAliasing!(Rebindable!Object));

    /* https://issues.dlang.org/show_bug.cgi?id=6979 */
    static assert(!hasUnsharedAliasing!(int, shared(int)*));
    static assert( hasUnsharedAliasing!(int, int*));
    static assert( hasUnsharedAliasing!(int, const(int)[]));
    static assert( hasUnsharedAliasing!(int, shared(int)*, Rebindable!Object));
    static assert(!hasUnsharedAliasing!(shared(int)*, Rebindable!(shared Object)));
    static assert(!hasUnsharedAliasing!());

    struct S9
    {
        void delegate() shared a;
        void delegate() immutable b;
        void delegate() shared const c;
        shared(void delegate()) d;
        shared(void delegate() shared) e;
        shared(void delegate() immutable) f;
        shared(void delegate() shared const) g;
        immutable(void delegate()) h;
        immutable(void delegate() shared) i;
        immutable(void delegate() immutable) j;
        immutable(void delegate() shared const) k;
        shared(const(void delegate())) l;
        shared(const(void delegate() shared)) m;
        shared(const(void delegate() immutable)) n;
        shared(const(void delegate() shared const)) o;
    }
    struct S10 { typeof(S9.tupleof) a; void delegate() p; }
    struct S11 { typeof(S9.tupleof) a; Object p; }
    struct S12 { typeof(S9.tupleof) a; int* p; }
    class S13 { typeof(S9.tupleof) a; }
    class S14 { typeof(S9.tupleof) a; void delegate() p; }
    class S15 { typeof(S9.tupleof) a; Object p; }
    class S16 { typeof(S9.tupleof) a; int* p; }
    static assert(!hasUnsharedAliasing!S9);
    static assert(!hasUnsharedAliasing!(immutable(S9)));
    static assert(!hasUnsharedAliasing!(shared(S9)));
    static assert( hasUnsharedAliasing!S10);
    static assert(!hasUnsharedAliasing!(immutable(S10)));
    static assert(!hasUnsharedAliasing!(shared(S10)));
    static assert( hasUnsharedAliasing!S11);
    static assert(!hasUnsharedAliasing!(immutable(S11)));
    static assert(!hasUnsharedAliasing!(shared(S11)));
    static assert( hasUnsharedAliasing!S12);
    static assert(!hasUnsharedAliasing!(immutable(S12)));
    static assert(!hasUnsharedAliasing!(shared(S12)));
    static assert( hasUnsharedAliasing!S13);
    static assert(!hasUnsharedAliasing!(immutable(S13)));
    static assert(!hasUnsharedAliasing!(shared(S13)));
    static assert( hasUnsharedAliasing!S14);
    static assert(!hasUnsharedAliasing!(immutable(S14)));
    static assert(!hasUnsharedAliasing!(shared(S14)));
    static assert( hasUnsharedAliasing!S15);
    static assert(!hasUnsharedAliasing!(immutable(S15)));
    static assert(!hasUnsharedAliasing!(shared(S15)));
    static assert( hasUnsharedAliasing!S16);
    static assert(!hasUnsharedAliasing!(immutable(S16)));
    static assert(!hasUnsharedAliasing!(shared(S16)));
    struct S17 {}
    class S18 {}
    interface S19 {}
    union S20 {}
    static assert(!hasUnsharedAliasing!S17);
    static assert( hasUnsharedAliasing!S18);
    static assert( hasUnsharedAliasing!S19);
    static assert(!hasUnsharedAliasing!S20);

    static assert(!hasUnsharedAliasing!noreturn);
}

private template hasUnsharedAliasingImpl(T)
{
    import std.typecons : Rebindable;

    static if (is(immutable T == immutable Rebindable!R, R))
    {
        enum hasUnsharedAliasingImpl = hasUnsharedAliasingImpl!R;
    }
    else
    {
        template unsharedDelegate(T)
        {
            enum bool unsharedDelegate = isDelegate!T
                                     && !is(T == shared)
                                     && !is(T == immutable)
                                     && !is(FunctionTypeOf!T == shared)
                                     && !is(FunctionTypeOf!T == immutable);
        }

        enum hasUnsharedAliasingImpl =
            hasRawUnsharedAliasing!T ||
            anySatisfy!(unsharedDelegate, RepresentationTypeTuple!T) ||
            hasUnsharedObjects!T;
    }
}

version (StdDdoc)
{
    /**
       True if `S` or any type embedded directly in the representation of `S`
       defines an elaborate copy constructor. Elaborate copy constructors are
       introduced by defining `this(this)` for a `struct`.

       Classes and unions never have elaborate copy constructors.
    */
    template hasElaborateCopyConstructor(S)
    {
        import core.internal.traits : hasElabCCtor = hasElaborateCopyConstructor;
        alias hasElaborateCopyConstructor = hasElabCCtor!(S);
    }
}
else
{
    import core.internal.traits : hasElabCCtor = hasElaborateCopyConstructor;
    alias hasElaborateCopyConstructor = hasElabCCtor;
}

///
@safe unittest
{
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

/**
   True if `S` or any type directly embedded in the representation of `S`
   defines an elaborate assignment. Elaborate assignments are introduced by
   defining `opAssign(typeof(this))` or $(D opAssign(ref typeof(this)))
   for a `struct` or when there is a compiler-generated `opAssign`.

   A type `S` gets compiler-generated `opAssign` if it has
   an elaborate destructor.

   Classes and unions never have elaborate assignments.

   Note: Structs with (possibly nested) postblit operator(s) will have a
   hidden yet elaborate compiler generated assignment operator (unless
   explicitly disabled).
 */
template hasElaborateAssign(S)
{
    static if (isStaticArray!S && S.length)
    {
        enum bool hasElaborateAssign = hasElaborateAssign!(typeof(S.init[0]));
    }
    else static if (is(S == struct))
    {
        enum hasElaborateAssign = is(typeof(S.init.opAssign(rvalueOf!S))) ||
                                  is(typeof(S.init.opAssign(lvalueOf!S))) ||
            anySatisfy!(.hasElaborateAssign, FieldTypeTuple!S);
    }
    else
    {
        enum bool hasElaborateAssign = false;
    }
}

///
@safe unittest
{
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
    static struct S  { void opAssign(S) {} }
    static struct S4
    {
        void opAssign(U)(U u) {}
        @disable void opAssign(U)(ref U u);
    }
    static assert( hasElaborateAssign!S4);

    static struct S41
    {
        void opAssign(U)(ref U u) {}
        @disable void opAssign(U)(U u);
    }
    static assert( hasElaborateAssign!S41);

    static struct S5 { @disable this(); this(int n){ s = S(); } S s; }
    static assert( hasElaborateAssign!S5);

    static struct S6 { this(this) {} }
    static struct S7 { this(this) {} @disable void opAssign(S7); }
    static struct S8 { this(this) {} @disable void opAssign(S8); void opAssign(int) {} }
    static struct S9 { this(this) {}                             void opAssign(int) {} }
    static struct S10 { ~this() { } }
    static assert( hasElaborateAssign!S6);
    static assert(!hasElaborateAssign!S7);
    static assert(!hasElaborateAssign!S8);
    static assert( hasElaborateAssign!S9);
    static assert( hasElaborateAssign!S10);
    static struct SS6 { S6 s; }
    static struct SS7 { S7 s; }
    static struct SS8 { S8 s; }
    static struct SS9 { S9 s; }
    static assert( hasElaborateAssign!SS6);
    static assert(!hasElaborateAssign!SS7);
    static assert(!hasElaborateAssign!SS8);
    static assert( hasElaborateAssign!SS9);
}

version (StdDdoc)
{
    /**
       True if `S` or any type directly embedded in the representation
       of `S` defines an elaborate destructor. Elaborate destructors
       are introduced by defining `~this()` for a $(D
       struct).

       Classes and unions never have elaborate destructors, even
       though classes may define `~this()`.
    */
    template hasElaborateDestructor(S)
    {
        import core.internal.traits : hasElabDest = hasElaborateDestructor;
        alias hasElaborateDestructor = hasElabDest!(S);
    }
}
else
{
    import core.internal.traits : hasElabDest = hasElaborateDestructor;
    alias hasElaborateDestructor = hasElabDest;
}

///
@safe unittest
{
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

version (StdDdoc)
{
    /**
       True if `S` or any type embedded directly in the representation of `S`
       defines elaborate move semantics. Elaborate move semantics are
       introduced by defining `opPostMove(ref typeof(this))` for a `struct`.

       Classes and unions never have elaborate move semantics.
    */
    template hasElaborateMove(S)
    {
        import core.internal.traits : hasElabMove = hasElaborateMove;
        alias hasElaborateMove = hasElabMove!(S);
    }
}
else
{
    import core.internal.traits : hasElabMove = hasElaborateMove;
    alias hasElaborateMove = hasElabMove;
}

///
@safe unittest
{
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

package alias Identity(alias A) = A;

/**
   Yields `true` if and only if `T` is an aggregate that defines
   a symbol called `name`.

   See also: $(DDSUBLINK spec/traits, hasMember, `__traits(hasMember, T, name)`)
 */
enum hasMember(T, string name) = __traits(hasMember, T, name);

///
@safe unittest
{
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
    // https://issues.dlang.org/show_bug.cgi?id=8321
    struct S {
        int x;
        void f(){}
        void t()(){}
        template T(){}
    }
    struct R1(T) {
        T t;
        alias t this;
    }
    struct R2(T) {
        T t;
        @property ref inout(T) payload() inout { return t; }
        alias t this;
    }
    static assert(hasMember!(S, "x"));
    static assert(hasMember!(S, "f"));
    static assert(hasMember!(S, "t"));
    static assert(hasMember!(S, "T"));
    static assert(hasMember!(R1!S, "x"));
    static assert(hasMember!(R1!S, "f"));
    static assert(hasMember!(R1!S, "t"));
    static assert(hasMember!(R1!S, "T"));
    static assert(hasMember!(R2!S, "x"));
    static assert(hasMember!(R2!S, "f"));
    static assert(hasMember!(R2!S, "t"));
    static assert(hasMember!(R2!S, "T"));
}

@safe unittest
{
    static struct S
    {
        void opDispatch(string n, A)(A dummy) {}
    }
    static assert(hasMember!(S, "foo"));
}

/**
 * Whether the symbol represented by the string, member, exists and is a static member of T.
 *
 * Params:
 *     T = Type containing symbol `member`.
 *     member = Name of symbol to test that resides in `T`.
 *
 * Returns:
 *     `true` iff `member` exists and is static.
 */
template hasStaticMember(T, string member)
{
    static if (__traits(hasMember, T, member))
    {
        static if (is(T == V*, V))
            alias U = V;
        else
            alias U = T;

        import std.meta : Alias;
        alias sym = Alias!(__traits(getMember, U, member));

        static if (__traits(getOverloads, U, member).length == 0)
            enum bool hasStaticMember = __traits(compiles, &sym);
        else
            enum bool hasStaticMember = __traits(isStaticFunction, sym);
    }
    else
    {
        enum bool hasStaticMember = false;
    }
}

///
@safe unittest
{
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
    static struct S
    {
        enum X = 10;
        enum Y
        {
            i = 10
        }
        struct S {}
        class C {}

        static int sx = 0;
        __gshared int gx = 0;

        Y y;
        static Y sy;

        static void f();
        static void f2() pure nothrow @nogc @safe;

        void g() shared;

        static void function() fp;
        __gshared void function() gfp;
        void function() fpm;

        void delegate() dm;
        static void delegate() sd;

        void m();
        void m2() const pure nothrow @nogc @safe;

        inout(int) iom() inout;
        static inout(int) iosf(inout int x);

        @property int p();
        static @property int sp();
    }

    static class C
    {
        enum X = 10;
        enum Y
        {
            i = 10
        }
        struct S {}
        class C {}

        static int sx = 0;
        __gshared int gx = 0;

        Y y;
        static Y sy;

        static void f();
        static void f2() pure nothrow @nogc @safe;

        void g() shared { }

        static void function() fp;
        __gshared void function() gfp;
        void function() fpm;

        void delegate() dm;
        static void delegate() sd;

        void m() {}
        final void m2() const pure nothrow @nogc @safe;

        inout(int) iom() inout { return 10; }
        static inout(int) iosf(inout int x);

        @property int p() { return 10; }
        static @property int sp();
    }

    static assert(!hasStaticMember!(S, "na"));
    static assert(!hasStaticMember!(S, "X"));
    static assert(!hasStaticMember!(S, "Y"));
    static assert(!hasStaticMember!(S, "Y.i"));
    static assert(!hasStaticMember!(S, "S"));
    static assert(!hasStaticMember!(S, "C"));
    static assert( hasStaticMember!(S, "sx"));
    static assert( hasStaticMember!(S, "gx"));
    static assert(!hasStaticMember!(S, "y"));
    static assert( hasStaticMember!(S, "sy"));
    static assert( hasStaticMember!(S, "f"));
    static assert( hasStaticMember!(S, "f2"));
    static assert(!hasStaticMember!(S, "dm"));
    static assert( hasStaticMember!(S, "sd"));
    static assert(!hasStaticMember!(S, "g"));
    static assert( hasStaticMember!(S, "fp"));
    static assert( hasStaticMember!(S, "gfp"));
    static assert(!hasStaticMember!(S, "fpm"));
    static assert(!hasStaticMember!(S, "m"));
    static assert(!hasStaticMember!(S, "m2"));
    static assert(!hasStaticMember!(S, "iom"));
    static assert( hasStaticMember!(S, "iosf"));
    static assert(!hasStaticMember!(S, "p"));
    static assert( hasStaticMember!(S, "sp"));

    static assert(!hasStaticMember!(C, "na"));
    static assert(!hasStaticMember!(C, "X"));
    static assert(!hasStaticMember!(C, "Y"));
    static assert(!hasStaticMember!(C, "Y.i"));
    static assert(!hasStaticMember!(C, "S"));
    static assert(!hasStaticMember!(C, "C"));
    static assert( hasStaticMember!(C, "sx"));
    static assert( hasStaticMember!(C, "gx"));
    static assert(!hasStaticMember!(C, "y"));
    static assert( hasStaticMember!(C, "sy"));
    static assert( hasStaticMember!(C, "f"));
    static assert( hasStaticMember!(C, "f2"));
    static assert(!hasStaticMember!(C, "dm"));
    static assert( hasStaticMember!(C, "sd"));
    static assert(!hasStaticMember!(C, "g"));
    static assert( hasStaticMember!(C, "fp"));
    static assert( hasStaticMember!(C, "gfp"));
    static assert(!hasStaticMember!(C, "fpm"));
    static assert(!hasStaticMember!(C, "m"));
    static assert(!hasStaticMember!(C, "m2"));
    static assert(!hasStaticMember!(C, "iom"));
    static assert( hasStaticMember!(C, "iosf"));
    static assert(!hasStaticMember!(C, "p"));
    static assert( hasStaticMember!(C, "sp"));

    alias P = S*;
    static assert(!hasStaticMember!(P, "na"));
    static assert(!hasStaticMember!(P, "X"));
    static assert(!hasStaticMember!(P, "Y"));
    static assert(!hasStaticMember!(P, "Y.i"));
    static assert(!hasStaticMember!(P, "S"));
    static assert(!hasStaticMember!(P, "C"));
    static assert( hasStaticMember!(P, "sx"));
    static assert( hasStaticMember!(P, "gx"));
    static assert(!hasStaticMember!(P, "y"));
    static assert( hasStaticMember!(P, "sy"));
    static assert( hasStaticMember!(P, "f"));
    static assert( hasStaticMember!(P, "f2"));
    static assert(!hasStaticMember!(P, "dm"));
    static assert( hasStaticMember!(P, "sd"));
    static assert(!hasStaticMember!(P, "g"));
    static assert( hasStaticMember!(P, "fp"));
    static assert( hasStaticMember!(P, "gfp"));
    static assert(!hasStaticMember!(P, "fpm"));
    static assert(!hasStaticMember!(P, "m"));
    static assert(!hasStaticMember!(P, "m2"));
    static assert(!hasStaticMember!(P, "iom"));
    static assert( hasStaticMember!(P, "iosf"));
    static assert(!hasStaticMember!(P, "p"));
    static assert( hasStaticMember!(P, "sp"));
}

/**
Retrieves the members of an enumerated type `enum E`.

Params:
    E = An enumerated type. `E` may have duplicated values.

Returns:
    Static tuple composed of the members of the enumerated type `E`.
    The members are arranged in the same order as declared in `E`.
    The name of the enum can be found by querying the compiler for the
    name of the identifier, i.e. `__traits(identifier, EnumMembers!MyEnum[i])`.
    For enumerations with unique values, $(REF to, std,conv) can also be used.

Note:
    An enum can have multiple members which have the same value. If you want
    to use EnumMembers to e.g. generate switch cases at compile-time,
    you should use the $(REF NoDuplicates, std,meta) template to avoid
    generating duplicate switch cases.

Note:
    Returned values are strictly typed with `E`. Thus, the following code
    does not work without the explicit cast:
--------------------
enum E : int { a, b, c }
int[] abc = cast(int[]) [ EnumMembers!E ];
--------------------
    Cast is not necessary if the type of the variable is inferred. See the
    example below.
 */
template EnumMembers(E)
if (is(E == enum))
{
    alias EnumMembers = AliasSeq!();
    static foreach (M; __traits(allMembers, E))
        EnumMembers = AliasSeq!(EnumMembers, __traits(getMember, E, M));
}

/// Create an array of enumerated values
@safe unittest
{
    enum Sqrts : real
    {
        one = 1,
        two = 1.41421,
        three = 1.73205
    }
    auto sqrts = [EnumMembers!Sqrts];
    assert(sqrts == [Sqrts.one, Sqrts.two, Sqrts.three]);
}

/**
A generic function `rank(v)` in the following example uses this
template for finding a member `e` in an enumerated type `E`.
 */
@safe unittest
{
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

/**
Use EnumMembers to generate a switch statement using static foreach.
*/

@safe unittest
{
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
    enum A { a }
    static assert([ EnumMembers!A ] == [ A.a ]);
    enum B { a, b, c, d, e }
    static assert([ EnumMembers!B ] == [ B.a, B.b, B.c, B.d, B.e ]);
}

@safe unittest    // typed enums
{
    enum A : string { a = "alpha", b = "beta" }
    static assert([ EnumMembers!A ] == [ A.a, A.b ]);

    static struct S
    {
        int value;
        int opCmp(S rhs) const nothrow { return value - rhs.value; }
    }
    enum B : S { a = S(1), b = S(2), c = S(3) }
    static assert([ EnumMembers!B ] == [ B.a, B.b, B.c ]);
}

@safe unittest    // duplicated values
{
    enum A
    {
        a = 0, b = 0,
        c = 1, d = 1, e
    }
    static assert([ EnumMembers!A ] == [ A.a, A.b, A.c, A.d, A.e ]);
}

// https://issues.dlang.org/show_bug.cgi?id=14561: huge enums
@safe unittest
{
    string genEnum()
    {
        string result = "enum TLAs {";
        foreach (c0; '0'..'2'+1)
            foreach (c1; '0'..'9'+1)
                foreach (c2; '0'..'9'+1)
                    foreach (c3; '0'..'9'+1)
        {
            result ~= '_';
            result ~= c0;
            result ~= c1;
            result ~= c2;
            result ~= c3;
            result ~= ',';
        }
        result ~= '}';
        return result;
    }
    mixin(genEnum);
    static assert(EnumMembers!TLAs[0] == TLAs._0000);
    static assert(EnumMembers!TLAs[$-1] == TLAs._2999);
}

@safe unittest
{
    enum E { member, a = 0, b = 0 }
    static assert(__traits(identifier, EnumMembers!E[0]) == "member");
    static assert(__traits(identifier, EnumMembers!E[1]) == "a");
    static assert(__traits(identifier, EnumMembers!E[2]) == "b");
}


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Classes and Interfaces
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/***
 * Get a $(D_PARAM AliasSeq) of the base class and base interfaces of
 * this class or interface. $(D_PARAM BaseTypeTuple!Object) returns
 * the empty type tuple.
 */
template BaseTypeTuple(A)
{
    static if (is(A P == super))
        alias BaseTypeTuple = P;
    else
        static assert(0, "argument is not a class or interface");
}

///
@safe unittest
{
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
    interface I1 { }
    interface I2 { }
    class A { }
    class C : A, I1, I2 { }

    alias TL = BaseTypeTuple!C;
    assert(TL.length == 3);
    assert(is (TL[0] == A));
    assert(is (TL[1] == I1));
    assert(is (TL[2] == I2));

    assert(BaseTypeTuple!Object.length == 0);
}

/**
 * Get a $(D_PARAM AliasSeq) of $(I all) base classes of this class,
 * in decreasing order. Interfaces are not included. $(D_PARAM
 * BaseClassesTuple!Object) yields the empty type tuple.
 */
template BaseClassesTuple(T)
if (is(T == class))
{
    static if (is(T == Object))
    {
        alias BaseClassesTuple = AliasSeq!();
    }
    else static if (is(BaseTypeTuple!T[0] == Object))
    {
        alias BaseClassesTuple = AliasSeq!Object;
    }
    else static if (!is(BaseTypeTuple!T[0] == Object) && !is(BaseTypeTuple!T[0] == class))
    {
        alias BaseClassesTuple = AliasSeq!();
    }
    else
    {
        alias BaseClassesTuple =
            AliasSeq!(BaseTypeTuple!T[0],
                       BaseClassesTuple!(BaseTypeTuple!T[0]));
    }
}

///
@safe unittest
{
    import std.meta : AliasSeq;

    class C1 { }
    class C2 : C1 { }
    class C3 : C2 { }
    static assert(!BaseClassesTuple!Object.length);
    static assert(is(BaseClassesTuple!C1 == AliasSeq!(Object)));
    static assert(is(BaseClassesTuple!C2 == AliasSeq!(C1, Object)));
    static assert(is(BaseClassesTuple!C3 == AliasSeq!(C2, C1, Object)));
}

// https://issues.dlang.org/show_bug.cgi?id=17276
@safe unittest
{
    extern (C++) static interface Ext
    {
        void someext();
    }

    extern (C++) static class E : Ext
    {
        void someext() {}
    }

    alias BaseClassesWithNoObject = BaseClassesTuple!E;
}

@safe unittest
{
    struct S { }
    static assert(!__traits(compiles, BaseClassesTuple!S));
    interface I { }
    static assert(!__traits(compiles, BaseClassesTuple!I));
    class C4 : I { }
    class C5 : C4, I { }
    static assert(is(BaseClassesTuple!C5 == AliasSeq!(C4, Object)));
}

/**
Params:
    T = The `class` or `interface` to search.

Returns:
    $(REF AliasSeq,std,meta) of all interfaces directly or
    indirectly inherited by this class or interface. Interfaces
    do not repeat if multiply implemented.

    `InterfacesTuple!Object` yields an empty `AliasSeq`.
 */
template InterfacesTuple(T)
{
    import std.meta : NoDuplicates;
    template Flatten(H, T...)
    {
        static if (T.length)
        {
            alias Flatten = AliasSeq!(Flatten!H, Flatten!T);
        }
        else
        {
            static if (is(H == interface))
                alias Flatten = AliasSeq!(H, InterfacesTuple!H);
            else
                alias Flatten = InterfacesTuple!H;
        }
    }

    static if (is(T S == super) && S.length)
        alias InterfacesTuple = NoDuplicates!(Flatten!S);
    else
        alias InterfacesTuple = AliasSeq!();
}

///
@safe unittest
{
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
    interface Iaa {}
    interface Iab {}
    interface Iba {}
    interface Ibb {}
    interface Ia : Iaa, Iab {}
    interface Ib : Iba, Ibb {}
    interface I : Ia, Ib {}
    interface J {}
    class B2 : J {}
    class C2 : B2, Ia, Ib {}
    static assert(is(InterfacesTuple!I ==
                    AliasSeq!(Ia, Iaa, Iab, Ib, Iba, Ibb)));
    static assert(is(InterfacesTuple!C2 ==
                    AliasSeq!(J, Ia, Iaa, Iab, Ib, Iba, Ibb)));

}

/**
 * Get a $(D_PARAM AliasSeq) of $(I all) base classes of $(D_PARAM
 * T), in decreasing order, followed by $(D_PARAM T)'s
 * interfaces. $(D_PARAM TransitiveBaseTypeTuple!Object) yields the
 * empty type tuple.
 */
alias TransitiveBaseTypeTuple(T) = AliasSeq!(BaseClassesTuple!T, InterfacesTuple!T);

///
@safe unittest
{
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


/**
Returns a tuple of non-static functions with the name `name` declared in the
class or interface `C`.  Covariant duplicates are shrunk into the most
derived one.
 */
template MemberFunctionsTuple(C, string name)
if (is(C == class) || is(C == interface))
{
    static if (__traits(hasMember, C, name))
    {
        /*
         * First, collect all overloads in the class hierarchy.
         */
        template CollectOverloads(Node)
        {
            static if (__traits(hasMember, Node, name) && __traits(compiles, __traits(getMember, Node, name)))
            {
                // Get all overloads in sight (not hidden).
                alias inSight = __traits(getVirtualMethods, Node, name);

                // And collect all overloads in ancestor classes to reveal hidden
                // methods.  The result may contain duplicates.
                template walkThru(Parents...)
                {
                    static if (Parents.length > 0)
                        alias walkThru = AliasSeq!(
                                    CollectOverloads!(Parents[0]),
                                    walkThru!(Parents[1 .. $])
                                );
                    else
                        alias walkThru = AliasSeq!();
                }

                static if (is(Node Parents == super))
                    alias CollectOverloads = AliasSeq!(inSight, walkThru!Parents);
                else
                    alias CollectOverloads = AliasSeq!inSight;
            }
            else
                alias CollectOverloads = AliasSeq!(); // no overloads in this hierarchy
        }

        static if (name == "__ctor" || name == "__dtor")
            alias overloads = AliasSeq!(__traits(getOverloads, C, name));
        else
            // duplicates in this tuple will be removed by shrink()
            alias overloads = CollectOverloads!C;

        // shrinkOne!args[0]    = the most derived one in the covariant siblings of target
        // shrinkOne!args[1..$] = non-covariant others
        template shrinkOne(/+ alias target, rest... +/ args...)
        {
            import std.meta : AliasSeq;
            alias target = args[0 .. 1]; // prevent property functions from being evaluated
            alias rest = args[1 .. $];

            static if (rest.length > 0)
            {
                alias Target = FunctionTypeOf!target;
                alias Rest0 = FunctionTypeOf!(rest[0]);

                static if (isCovariantWith!(Target, Rest0) && isCovariantWith!(Rest0, Target))
                {
                    // One of these overrides the other. Choose the one from the most derived parent.
                    static if (is(__traits(parent, target) : __traits(parent, rest[0])))
                        alias shrinkOne = shrinkOne!(target, rest[1 .. $]);
                    else
                        alias shrinkOne = shrinkOne!(rest[0], rest[1 .. $]);
                }
                else static if (isCovariantWith!(Target, Rest0))
                    // target overrides rest[0] -- erase rest[0].
                    alias shrinkOne = shrinkOne!(target, rest[1 .. $]);
                else static if (isCovariantWith!(Rest0, Target))
                    // rest[0] overrides target -- erase target.
                    alias shrinkOne = shrinkOne!(rest[0], rest[1 .. $]);
                else
                    // target and rest[0] are distinct.
                    alias shrinkOne = AliasSeq!(
                                shrinkOne!(target, rest[1 .. $]),
                                rest[0] // keep
                            );
            }
            else
                alias shrinkOne = AliasSeq!target; // done
        }

        /*
         * Now shrink covariant overloads into one.
         */
        template shrink(overloads...)
        {
            static if (overloads.length > 0)
            {
                alias temp = shrinkOne!overloads;
                alias shrink = AliasSeq!(temp[0], shrink!(temp[1 .. $]));
            }
            else
                alias shrink = AliasSeq!(); // done
        }

        // done.
        alias MemberFunctionsTuple = shrink!overloads;
    }
    else
        alias MemberFunctionsTuple = AliasSeq!();
}

///
@safe unittest
{
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

// https://issues.dlang.org/show_bug.cgi?id=15920
@safe unittest
{
    import std.meta : AliasSeq;
    class A
    {
        void f(){}
        void f(int){}
    }
    class B : A
    {
        override void f(){}
        override void f(int){}
    }
    alias fs = MemberFunctionsTuple!(B, "f");
    alias bfs = __traits(getOverloads, B, "f");
    assert(__traits(isSame, fs[0], bfs[0]) || __traits(isSame, fs[0], bfs[1]));
    assert(__traits(isSame, fs[1], bfs[0]) || __traits(isSame, fs[1], bfs[1]));
}

// https://issues.dlang.org/show_bug.cgi?id=8388
@safe unittest
{
    class C
    {
        this() {}
        this(int i) {}
        this(int i, float j) {}
        this(string s) {}

        /*
         Commented out, because this causes a cyclic dependency
         between module constructors/destructors error. Might
         be caused by https://issues.dlang.org/show_bug.cgi?id=20529. */
        // static this() {}

        ~this() {}
    }

    class D : C
    {
        this() {}
        ~this() {}
    }

    alias test_ctor = MemberFunctionsTuple!(C, "__ctor");
    assert(test_ctor.length == 4);
    alias test_dtor = MemberFunctionsTuple!(C, "__dtor");
    assert(test_dtor.length == 1);
    alias test2_ctor = MemberFunctionsTuple!(D, "__ctor");
    assert(test2_ctor.length == 1);
    alias test2_dtor = MemberFunctionsTuple!(D, "__dtor");
    assert(test2_dtor.length == 1);
}

@safe unittest
{
    interface I     { I test(); }
    interface J : I { J test(); }
    interface K     { K test(int); }
    class B : I, K
    {
        K test(int) { return this; }
        B test() { return this; }
        static void test(string) { }
    }
    class C : B, J
    {
        override C test() { return this; }
    }
    alias test =MemberFunctionsTuple!(C, "test");
    static assert(test.length == 2);
    static assert(is(FunctionTypeOf!(test[0]) == FunctionTypeOf!(C.test)));
    static assert(is(FunctionTypeOf!(test[1]) == FunctionTypeOf!(K.test)));
    alias noexist = MemberFunctionsTuple!(C, "noexist");
    static assert(noexist.length == 0);

    interface L { int prop() @property; }
    alias prop = MemberFunctionsTuple!(L, "prop");
    static assert(prop.length == 1);

    interface Test_I
    {
        void foo();
        void foo(int);
        void foo(int, int);
    }
    interface Test : Test_I {}
    alias Test_foo = MemberFunctionsTuple!(Test, "foo");
    static assert(Test_foo.length == 3);
    static assert(is(typeof(&Test_foo[0]) == void function()));
    static assert(is(typeof(&Test_foo[2]) == void function(int)));
    static assert(is(typeof(&Test_foo[1]) == void function(int, int)));
}


/**
Returns an alias to the template that `T` is an instance of.
It will return `void` if a symbol without a template is given.
 */
alias TemplateOf(alias T : Base!Args, alias Base, Args...) = Base;

/// ditto
alias TemplateOf(T : Base!Args, alias Base, Args...) = Base;

/// ditto
alias TemplateOf(T) = void;

///
@safe unittest
{
    struct Foo(T, U) {}
    static assert(__traits(isSame, TemplateOf!(Foo!(int, real)), Foo));
}

@safe unittest
{
    template Foo1(A) {}
    template Foo2(A, B) {}
    template Foo3(alias A) {}
    template Foo4(string A) {}
    struct Foo5(A) {}
    struct Foo6(A, B) {}
    struct Foo7(alias A) {}
    template Foo8(A) { template Foo9(B) {} }
    template Foo10() {}

    static assert(__traits(isSame, TemplateOf!(Foo1!(int)), Foo1));
    static assert(__traits(isSame, TemplateOf!(Foo2!(int, int)), Foo2));
    static assert(__traits(isSame, TemplateOf!(Foo3!(123)), Foo3));
    static assert(__traits(isSame, TemplateOf!(Foo4!("123")), Foo4));
    static assert(__traits(isSame, TemplateOf!(Foo5!(int)), Foo5));
    static assert(__traits(isSame, TemplateOf!(Foo6!(int, int)), Foo6));
    static assert(__traits(isSame, TemplateOf!(Foo7!(123)), Foo7));
    static assert(__traits(isSame, TemplateOf!(Foo8!(int).Foo9!(real)), Foo8!(int).Foo9));
    static assert(__traits(isSame, TemplateOf!(Foo10!()), Foo10));
}

// https://issues.dlang.org/show_bug.cgi?id=18214
@safe unittest
{
    static assert(is(TemplateOf!(int[]) == void));
    static assert(is(TemplateOf!bool == void));
}

/**
Returns a `AliasSeq` of the template arguments used to instantiate `T`.
 */
alias TemplateArgsOf(alias T : Base!Args, alias Base, Args...) = Args;

/// ditto
alias TemplateArgsOf(T : Base!Args, alias Base, Args...) = Args;

///
@safe unittest
{
    import std.meta : AliasSeq;

    struct Foo(T, U) {}
    static assert(is(TemplateArgsOf!(Foo!(int, real)) == AliasSeq!(int, real)));
}

@safe unittest
{
    template Foo1(A) {}
    template Foo2(A, B) {}
    template Foo3(alias A) {}
    template Foo4(string A) {}
    struct Foo5(A) {}
    struct Foo6(A, B) {}
    struct Foo7(alias A) {}
    template Foo8(A) { template Foo9(B) {} }
    template Foo10() {}

    enum x = 123;
    enum y = "123";
    static assert(is(TemplateArgsOf!(Foo1!(int)) == AliasSeq!(int)));
    static assert(is(TemplateArgsOf!(Foo2!(int, int)) == AliasSeq!(int, int)));
    static assert(__traits(isSame, TemplateArgsOf!(Foo3!(x)), AliasSeq!(x)));
    static assert(TemplateArgsOf!(Foo4!(y)) == AliasSeq!(y));
    static assert(is(TemplateArgsOf!(Foo5!(int)) == AliasSeq!(int)));
    static assert(is(TemplateArgsOf!(Foo6!(int, int)) == AliasSeq!(int, int)));
    static assert(__traits(isSame, TemplateArgsOf!(Foo7!(x)), AliasSeq!(x)));
    static assert(is(TemplateArgsOf!(Foo8!(int).Foo9!(real)) == AliasSeq!(real)));
    static assert(is(TemplateArgsOf!(Foo10!()) == AliasSeq!()));
}

// Returns the largest alignment in a type tuple.
package enum maxAlignment(U...) =
{
    size_t result = U[0].alignof;
    static foreach (T; U[1 .. $])
        if (result < T.alignof)
            result = T.alignof;
    return result;
}();

/**
Returns class instance alignment.

See also: $(DDSUBLINK spec/traits, classInstanceAlignment, `__traits(classInstanceAlignment, T)`)
 */
template classInstanceAlignment(T)
if (is(T == class))
{
    enum classInstanceAlignment = __traits(classInstanceAlignment, T);
}

///
@safe unittest
{
    class A { byte b; }
    class B { long l; }

    // As class instance always has a hidden pointer
    static assert(classInstanceAlignment!A == (void*).alignof);
    static assert(classInstanceAlignment!B == long.alignof);
}


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Type Conversion
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
Get the type that all types can be implicitly converted to. Useful
e.g. in figuring out an array type from a bunch of initializing
values. Returns $(D_PARAM void) if passed an empty list, or if the
types have no common type.
 */
template CommonType(T...)
{
    static if (T.length == 1)
        alias CommonType = typeof(T[0].init);
    else static if (is(typeof(true ? T[0].init : T[1].init) U))
        alias CommonType = CommonType!(U, T[2 .. $]);
    else
        alias CommonType = void;
}

///
@safe unittest
{
    alias X = CommonType!(int, long, short);
    assert(is(X == long));
    alias Y = CommonType!(int, char[], short);
    assert(is(Y == void));
}

///
@safe unittest
{
    static assert(is(CommonType!(3) == int));
    static assert(is(CommonType!(double, 4, float) == double));
    static assert(is(CommonType!(string, char[]) == const(char)[]));
    static assert(is(CommonType!(3, 3U) == uint));
    static assert(is(CommonType!(double, int) == double));
}


/**
Params:
    T = The type to check

Returns:
    An $(REF AliasSeq,std,meta) with all possible target types of an implicit
    conversion `T`.

    If `T` is a class derived from `Object`, the result of
    $(LREF TransitiveBaseTypeTuple) is returned.

    If the type is not a built-in value type or a class derived from
    `Object`, an empty $(REF AliasSeq,std,meta) is returned.

See_Also:
    $(LREF isImplicitlyConvertible)
 */
template AllImplicitConversionTargets(T)
{
    static if (is(T == bool))
        alias AllImplicitConversionTargets =
            AliasSeq!(byte, AllImplicitConversionTargets!byte);
    else static if (is(T == byte))
        alias AllImplicitConversionTargets =
            AliasSeq!(char, ubyte, short, AllImplicitConversionTargets!short);
    else static if (is(T == ubyte))
        alias AllImplicitConversionTargets =
            AliasSeq!(byte, char, short, AllImplicitConversionTargets!short);
    else static if (is(T == short))
        alias AllImplicitConversionTargets =
            AliasSeq!(ushort, wchar, int, AllImplicitConversionTargets!int);
    else static if (is(T == ushort))
        alias AllImplicitConversionTargets =
            AliasSeq!(short, wchar, dchar, AllImplicitConversionTargets!dchar);
    else static if (is(T == int))
        alias AllImplicitConversionTargets =
            AliasSeq!(dchar, uint, long, AllImplicitConversionTargets!long);
    else static if (is(T == uint))
        alias AllImplicitConversionTargets =
            AliasSeq!(dchar, int, long, AllImplicitConversionTargets!long);
    else static if (is(T == long))
        alias AllImplicitConversionTargets = AliasSeq!(ulong, CentTypeList, float, double, real);
    else static if (is(T == ulong))
        alias AllImplicitConversionTargets = AliasSeq!(long, CentTypeList, float, double, real);
    else static if (is(T == float))
        alias AllImplicitConversionTargets = AliasSeq!(double, real);
    else static if (is(T == double))
        alias AllImplicitConversionTargets = AliasSeq!(float, real);
    else static if (is(T == real))
        alias AllImplicitConversionTargets = AliasSeq!(float, double);
    else static if (is(T == char))
        alias AllImplicitConversionTargets =
            AliasSeq!(byte, ubyte, short, AllImplicitConversionTargets!short);
    else static if (is(T == wchar))
        alias AllImplicitConversionTargets =
            AliasSeq!(short, ushort, dchar, AllImplicitConversionTargets!dchar);
    else static if (is(T == dchar))
        alias AllImplicitConversionTargets =
            AliasSeq!(int, uint, long, AllImplicitConversionTargets!long);
    else static if (is(T == class))
        alias AllImplicitConversionTargets = staticMap!(ApplyLeft!(CopyConstness, T), TransitiveBaseTypeTuple!T);
    else static if (is(T == interface))
        alias AllImplicitConversionTargets = staticMap!(ApplyLeft!(CopyConstness, T), InterfacesTuple!T);
    else static if (isDynamicArray!T && !is(typeof(T.init[0]) == const))
    {
       static if (is(typeof(T.init[0]) == shared))
           alias AllImplicitConversionTargets =
           AliasSeq!(const(shared(Unqual!(typeof(T.init[0]))))[]);
       else
           alias AllImplicitConversionTargets =
           AliasSeq!(const(Unqual!(typeof(T.init[0])))[]);
    }
    else static if (is(T : void*) && !is(T == void*))
        alias AllImplicitConversionTargets = AliasSeq!(void*);
    else static if (is(cent) && is(T == cent))
        alias AllImplicitConversionTargets = AliasSeq!(UnsignedCentTypeList, float, double, real);
    else static if (is(ucent) && is(T == ucent))
        alias AllImplicitConversionTargets = AliasSeq!(SignedCentTypeList, float, double, real);
    else
        alias AllImplicitConversionTargets = AliasSeq!();
}

///
@safe unittest
{
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
    static assert(is(AllImplicitConversionTargets!(double)[0] == float));
    static assert(is(AllImplicitConversionTargets!(double)[1] == real));
    static assert(is(AllImplicitConversionTargets!(string)[0] == const(char)[]));
}


/**
Params:
    T = The type to check

Warning:
    This template is considered out-dated. It will be removed from
    Phobos in 2.107.0. Please use $(LREF AllImplicitConversionTargets) instead.

Returns:
    An $(REF AliasSeq,std,meta) with all possible target types of an implicit
    conversion `T`.

    If `T` is a class derived from `Object`, the result of
    $(LREF TransitiveBaseTypeTuple) is returned.

    If the type is not a built-in value type or a class derived from
    `Object`, an empty $(REF AliasSeq,std,meta) is returned.

Note:
    The possible targets are computed more conservatively than the
    language allows, eliminating all dangerous conversions. For example,
    `ImplicitConversionTargets!double` does not include `float`.

See_Also:
    $(LREF isImplicitlyConvertible)
 */
// @@@DEPRECATED_[2.107.0]@@@
deprecated("ImplicitConversionTargets has been deprecated in favour of AllImplicitConversionTargets "
   ~ "and will be removed in 2.107.0")
template ImplicitConversionTargets(T)
{
    static if (is(T == bool))
        alias ImplicitConversionTargets =
            AliasSeq!(byte, ubyte, short, ushort, int, uint, long, ulong, CentTypeList,
                       float, double, real, char, wchar, dchar);
    else static if (is(T == byte))
        alias ImplicitConversionTargets =
            AliasSeq!(short, ushort, int, uint, long, ulong, CentTypeList,
                       float, double, real, char, wchar, dchar);
    else static if (is(T == ubyte))
        alias ImplicitConversionTargets =
            AliasSeq!(short, ushort, int, uint, long, ulong, CentTypeList,
                       float, double, real, char, wchar, dchar);
    else static if (is(T == short))
        alias ImplicitConversionTargets =
            AliasSeq!(int, uint, long, ulong, CentTypeList, float, double, real);
    else static if (is(T == ushort))
        alias ImplicitConversionTargets =
            AliasSeq!(int, uint, long, ulong, CentTypeList, float, double, real);
    else static if (is(T == int))
        alias ImplicitConversionTargets =
            AliasSeq!(long, ulong, CentTypeList, float, double, real);
    else static if (is(T == uint))
        alias ImplicitConversionTargets =
            AliasSeq!(long, ulong, CentTypeList, float, double, real);
    else static if (is(T == long))
        alias ImplicitConversionTargets = AliasSeq!(float, double, real);
    else static if (is(T == ulong))
        alias ImplicitConversionTargets = AliasSeq!(float, double, real);
    else static if (is(cent) && is(T == cent))
        alias ImplicitConversionTargets = AliasSeq!(float, double, real);
    else static if (is(ucent) && is(T == ucent))
        alias ImplicitConversionTargets = AliasSeq!(float, double, real);
    else static if (is(T == float))
        alias ImplicitConversionTargets = AliasSeq!(double, real);
    else static if (is(T == double))
        alias ImplicitConversionTargets = AliasSeq!real;
    else static if (is(T == char))
        alias ImplicitConversionTargets =
            AliasSeq!(wchar, dchar, byte, ubyte, short, ushort,
                       int, uint, long, ulong, CentTypeList, float, double, real);
    else static if (is(T == wchar))
        alias ImplicitConversionTargets =
            AliasSeq!(dchar, short, ushort, int, uint, long, ulong, CentTypeList,
                       float, double, real);
    else static if (is(T == dchar))
        alias ImplicitConversionTargets =
            AliasSeq!(int, uint, long, ulong, CentTypeList, float, double, real);
    else static if (is(T : typeof(null)))
        alias ImplicitConversionTargets = AliasSeq!(typeof(null));
    else static if (is(T == class))
        alias ImplicitConversionTargets = staticMap!(ApplyLeft!(CopyConstness, T), TransitiveBaseTypeTuple!(T));
    else static if (isDynamicArray!T && !is(typeof(T.init[0]) == const))
    {
       static if (is(typeof(T.init[0]) == shared))
           alias ImplicitConversionTargets =
           AliasSeq!(const(shared(Unqual!(typeof(T.init[0]))))[]);
       else
           alias ImplicitConversionTargets =
           AliasSeq!(const(Unqual!(typeof(T.init[0])))[]);
    }
    else static if (is(T : void*))
        alias ImplicitConversionTargets = AliasSeq!(void*);
    else
        alias ImplicitConversionTargets = AliasSeq!();
}

deprecated @safe unittest
{
    import std.meta : AliasSeq;

    static assert(is(ImplicitConversionTargets!(ulong) == AliasSeq!(float, double, real)));
    static assert(is(ImplicitConversionTargets!(int) == AliasSeq!(long, ulong, float, double, real)));
    static assert(is(ImplicitConversionTargets!(float) == AliasSeq!(double, real)));
    static assert(is(ImplicitConversionTargets!(double) == AliasSeq!(real)));

    static assert(is(ImplicitConversionTargets!(char) == AliasSeq!(
        wchar, dchar, byte, ubyte, short, ushort, int, uint, long, ulong, float, double, real
    )));
    static assert(is(ImplicitConversionTargets!(wchar) == AliasSeq!(
        dchar, short, ushort, int, uint, long, ulong, float, double, real
    )));
    static assert(is(ImplicitConversionTargets!(dchar) == AliasSeq!(
        int, uint, long, ulong, float, double, real
    )));

    static assert(is(ImplicitConversionTargets!(string) == AliasSeq!(const(char)[])));
    static assert(is(ImplicitConversionTargets!(void*) == AliasSeq!(void*)));

    interface A {}
    interface B {}
    class C : A, B {}

    static assert(is(ImplicitConversionTargets!(C) == AliasSeq!(Object, A, B)));
    static assert(is(ImplicitConversionTargets!(const C) == AliasSeq!(const Object, const A, const B)));
    static assert(is(ImplicitConversionTargets!(immutable C) == AliasSeq!(
        immutable Object, immutable A, immutable B
    )));
}

deprecated @safe unittest
{
    static assert(is(ImplicitConversionTargets!(double)[0] == real));
    static assert(is(ImplicitConversionTargets!(string)[0] == const(char)[]));
}

/**
Is `From` implicitly convertible to `To`?
 */
enum bool isImplicitlyConvertible(From, To) = is(From : To);

///
@safe unittest
{
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

/**
Returns `true` iff a value of type `Rhs` can be assigned to a variable of
type `Lhs`.

`isAssignable` returns whether both an lvalue and rvalue can be assigned.

If you omit `Rhs`, `isAssignable` will check identity assignable of `Lhs`.
*/
enum isAssignable(Lhs, Rhs = Lhs) = isRvalueAssignable!(Lhs, Rhs) && isLvalueAssignable!(Lhs, Rhs);

///
@safe unittest
{
    static assert( isAssignable!(long, int));
    static assert(!isAssignable!(int, long));
    static assert( isAssignable!(const(char)[], string));
    static assert(!isAssignable!(string, char[]));

    // int is assignable to int
    static assert( isAssignable!int);

    // immutable int is not assignable to immutable int
    static assert(!isAssignable!(immutable int));
}

/**
Returns `true` iff an rvalue of type `Rhs` can be assigned to a variable of
type `Lhs`.
*/
enum isRvalueAssignable(Lhs, Rhs = Lhs) = __traits(compiles, { lvalueOf!Lhs = rvalueOf!Rhs; });

///
@safe unittest
{
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

/**
Returns `true` iff an lvalue of type `Rhs` can be assigned to a variable of
type `Lhs`.
*/
enum isLvalueAssignable(Lhs, Rhs = Lhs) = __traits(compiles, { lvalueOf!Lhs = lvalueOf!Rhs; });

///
@safe unittest
{
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
    static assert(!isAssignable!(immutable int, int));
    static assert( isAssignable!(int, immutable int));

    static assert(!isAssignable!(inout int, int));
    static assert( isAssignable!(int, inout int));
    static assert(!isAssignable!(inout int));

    static assert( isAssignable!(shared int, int));
    static assert( isAssignable!(int, shared int));
    static assert( isAssignable!(shared int));

    static assert( isAssignable!(void[1], void[1]));

    struct S { @disable this(); this(int n){} }
    static assert( isAssignable!(S, S));

    struct S2 { this(int n){} }
    static assert( isAssignable!(S2, S2));
    static assert(!isAssignable!(S2, int));

    struct S3 { @disable void opAssign(); }
    static assert( isAssignable!(S3, S3));

    struct S3X { @disable void opAssign(S3X); }
    static assert(!isAssignable!(S3X, S3X));

    struct S4 { void opAssign(int); }
    static assert( isAssignable!(S4, S4));
    static assert( isAssignable!(S4, int));
    static assert( isAssignable!(S4, immutable int));

    struct S5 { @disable this(); @disable this(this); }
    // https://issues.dlang.org/show_bug.cgi?id=21210
    static assert(!isAssignable!S5);

    // `-preview=in` is enabled
    alias DScannerBug895 = int[256];
    static if (((in DScannerBug895 a) { return __traits(isRef, a); })(DScannerBug895.init))
    {
        struct S6 { void opAssign(in S5); }

        static assert(isRvalueAssignable!(S6, S5));
        static assert(isLvalueAssignable!(S6, S5));
        static assert(isAssignable!(S6, S5));
        static assert(isAssignable!(S6, immutable S5));
    }
    else
    {
        mixin(q{ struct S6 { void opAssign(scope const ref S5); } });

        static assert(!isRvalueAssignable!(S6, S5));
        static assert( isLvalueAssignable!(S6, S5));
        static assert(!isAssignable!(S6, S5));
        static assert( isLvalueAssignable!(S6, immutable S5));
    }
}


// Equivalent with TypeStruct::isAssignable in compiler code.
package template isBlitAssignable(T)
{
    static if (is(T == enum))
    {
        enum isBlitAssignable = isBlitAssignable!(OriginalType!T);
    }
    else static if (isStaticArray!T && is(T == E[n], E, size_t n))
    // Workaround for https://issues.dlang.org/show_bug.cgi?id=11499 : isStaticArray!T should not be necessary.
    {
        enum isBlitAssignable = isBlitAssignable!E;
    }
    else static if (is(T == struct) || is(T == union))
    {
        enum isBlitAssignable = isMutable!T &&
        {
            size_t offset = 0;
            bool assignable = true;
            foreach (i, F; FieldTypeTuple!T)
            {
                static if (i == 0)
                {
                }
                else
                {
                    if (T.tupleof[i].offsetof == offset)
                    {
                        if (assignable)
                            continue;
                    }
                    else
                    {
                        if (!assignable)
                            return false;
                    }
                }
                assignable = isBlitAssignable!(typeof(T.tupleof[i]));
                offset = T.tupleof[i].offsetof;
            }
            return assignable;
        }();
    }
    else
        enum isBlitAssignable = isMutable!T;
}

@safe unittest
{
    static assert( isBlitAssignable!int);
    static assert(!isBlitAssignable!(const int));

    class C{ const int i; }
    static assert( isBlitAssignable!C);

    struct S1{ int i; }
    struct S2{ const int i; }
    static assert( isBlitAssignable!S1);
    static assert(!isBlitAssignable!S2);

    struct S3X { union {       int x;       int y; } }
    struct S3Y { union {       int x; const int y; } }
    struct S3Z { union { const int x; const int y; } }
    static assert( isBlitAssignable!(S3X));
    static assert( isBlitAssignable!(S3Y));
    static assert(!isBlitAssignable!(S3Z));
    static assert(!isBlitAssignable!(const S3X));
    static assert(!isBlitAssignable!(inout S3Y));
    static assert(!isBlitAssignable!(immutable S3Z));
    static assert( isBlitAssignable!(S3X[3]));
    static assert( isBlitAssignable!(S3Y[3]));
    static assert(!isBlitAssignable!(S3Z[3]));
    enum ES3X : S3X { a = S3X() }
    enum ES3Y : S3Y { a = S3Y() }
    enum ES3Z : S3Z { a = S3Z() }
    static assert( isBlitAssignable!(ES3X));
    static assert( isBlitAssignable!(ES3Y));
    static assert(!isBlitAssignable!(ES3Z));
    static assert(!isBlitAssignable!(const ES3X));
    static assert(!isBlitAssignable!(inout ES3Y));
    static assert(!isBlitAssignable!(immutable ES3Z));
    static assert( isBlitAssignable!(ES3X[3]));
    static assert( isBlitAssignable!(ES3Y[3]));
    static assert(!isBlitAssignable!(ES3Z[3]));

    union U1X {       int x;       int y; }
    union U1Y {       int x; const int y; }
    union U1Z { const int x; const int y; }
    static assert( isBlitAssignable!(U1X));
    static assert( isBlitAssignable!(U1Y));
    static assert(!isBlitAssignable!(U1Z));
    static assert(!isBlitAssignable!(const U1X));
    static assert(!isBlitAssignable!(inout U1Y));
    static assert(!isBlitAssignable!(immutable U1Z));
    static assert( isBlitAssignable!(U1X[3]));
    static assert( isBlitAssignable!(U1Y[3]));
    static assert(!isBlitAssignable!(U1Z[3]));
    enum EU1X : U1X { a = U1X() }
    enum EU1Y : U1Y { a = U1Y() }
    enum EU1Z : U1Z { a = U1Z() }
    static assert( isBlitAssignable!(EU1X));
    static assert( isBlitAssignable!(EU1Y));
    static assert(!isBlitAssignable!(EU1Z));
    static assert(!isBlitAssignable!(const EU1X));
    static assert(!isBlitAssignable!(inout EU1Y));
    static assert(!isBlitAssignable!(immutable EU1Z));
    static assert( isBlitAssignable!(EU1X[3]));
    static assert( isBlitAssignable!(EU1Y[3]));
    static assert(!isBlitAssignable!(EU1Z[3]));

    struct SA
    {
        @property int[3] foo() { return [1,2,3]; }
        alias foo this;
        const int x;    // SA is not blit assignable
    }
    static assert(!isStaticArray!SA);
    static assert(!isBlitAssignable!(SA[3]));
}


/*
Works like `isImplicitlyConvertible`, except this cares only about storage
classes of the arguments.
 */
private template isStorageClassImplicitlyConvertible(From, To)
{
    alias Pointify(T) = void*;

    enum isStorageClassImplicitlyConvertible = is(
            ModifyTypePreservingTQ!(Pointify, From) :
            ModifyTypePreservingTQ!(Pointify,   To) );
}

@safe unittest
{
    static assert( isStorageClassImplicitlyConvertible!(          int, const int));
    static assert( isStorageClassImplicitlyConvertible!(immutable int, const int));

    static assert(!isStorageClassImplicitlyConvertible!(const int,           int));
    static assert(!isStorageClassImplicitlyConvertible!(const int, immutable int));
    static assert(!isStorageClassImplicitlyConvertible!(int, shared int));
    static assert(!isStorageClassImplicitlyConvertible!(shared int, int));
}


/**
Determines whether the function type `F` is covariant with `G`, i.e.,
functions of the type `F` can override ones of the type `G`.
 */
template isCovariantWith(F, G)
if (is(F == function) && is(G == function) ||
    is(F == delegate) && is(G == delegate) ||
    isFunctionPointer!F && isFunctionPointer!G)
{
    static if (is(F : G))
        enum isCovariantWith = true;
    else
    {
        alias Upr = F;
        alias Lwr = G;

        /*
         * Check for calling convention: require exact match.
         */
        template checkLinkage()
        {
            enum ok = functionLinkage!Upr == functionLinkage!Lwr;
        }
        /*
         * Check for variadic parameter: require exact match.
         */
        template checkVariadicity()
        {
            enum ok = variadicFunctionStyle!Upr == variadicFunctionStyle!Lwr;
        }
        /*
         * Check for function storage class:
         *  - overrider can have narrower storage class than base
         */
        template checkSTC()
        {
            // Note the order of arguments.  The convertion order Lwr -> Upr is
            // correct since Upr should be semantically 'narrower' than Lwr.
            enum ok = isStorageClassImplicitlyConvertible!(Lwr, Upr);
        }
        /*
         * Check for function attributes:
         *  - require exact match for ref and @property
         *  - overrider can add pure and nothrow, but can't remove them
         *  - @safe and @trusted are covariant with each other, unremovable
         */
        template checkAttributes()
        {
            alias FA = FunctionAttribute;
            enum uprAtts = functionAttributes!Upr;
            enum lwrAtts = functionAttributes!Lwr;
            //
            enum wantExact = FA.ref_ | FA.property;
            enum safety = FA.safe | FA.trusted;
            enum ok =
                (  (uprAtts & wantExact)   == (lwrAtts & wantExact)) &&
                (  (uprAtts & FA.pure_   ) >= (lwrAtts & FA.pure_   )) &&
                (  (uprAtts & FA.nothrow_) >= (lwrAtts & FA.nothrow_)) &&
                (!!(uprAtts & safety    )  >= !!(lwrAtts & safety    )) ;
        }
        /*
         * Check for return type: usual implicit convertion.
         */
        template checkReturnType()
        {
            enum ok = is(ReturnType!Upr : ReturnType!Lwr);
        }
        /*
         * Check for parameters:
         *  - require exact match for types
         *    (cf. https://issues.dlang.org/show_bug.cgi?id=3075)
         *  - require exact match for in, out, ref and lazy
         *  - overrider can add scope, but can't remove
         */
        template checkParameters()
        {
            alias STC = ParameterStorageClass;
            alias UprParams = Parameters!Upr;
            alias LwrParams = Parameters!Lwr;
            alias UprPSTCs  = ParameterStorageClassTuple!Upr;
            alias LwrPSTCs  = ParameterStorageClassTuple!Lwr;
            //
            template checkNext(size_t i)
            {
                static if (i < UprParams.length)
                {
                    enum uprStc = UprPSTCs[i];
                    enum lwrStc = LwrPSTCs[i];
                    //
                    enum wantExact = STC.out_ | STC.ref_ | STC.lazy_ | STC.return_;
                    enum ok =
                        ((uprStc & wantExact )  == (lwrStc & wantExact )) &&
                        ((uprStc & STC.scope_)  >= (lwrStc & STC.scope_)) &&
                        checkNext!(i + 1).ok;
                }
                else
                    enum ok = true; // done
            }
            static if (UprParams.length == LwrParams.length)
                enum ok = is(UprParams == LwrParams) && checkNext!(0).ok;
            else
                enum ok = false;
        }

        /* run all the checks */
        enum isCovariantWith =
            checkLinkage    !().ok &&
            checkVariadicity!().ok &&
            checkSTC        !().ok &&
            checkAttributes !().ok &&
            checkReturnType !().ok &&
            checkParameters !().ok ;
    }
}

///
@safe unittest
{
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

@safe unittest
{
    enum bool isCovariantWith(alias f, alias g) = .isCovariantWith!(typeof(f), typeof(g));

    // covariant return type
    interface I     {}
    interface J : I {}
    interface BaseA            {          const(I) test(int); }
    interface DerivA_1 : BaseA { override const(J) test(int); }
    interface DerivA_2 : BaseA { override       J  test(int); }
    static assert( isCovariantWith!(DerivA_1.test, BaseA.test));
    static assert( isCovariantWith!(DerivA_2.test, BaseA.test));
    static assert(!isCovariantWith!(BaseA.test, DerivA_1.test));
    static assert(!isCovariantWith!(BaseA.test, DerivA_2.test));
    static assert( isCovariantWith!(BaseA.test, BaseA.test));
    static assert( isCovariantWith!(DerivA_1.test, DerivA_1.test));
    static assert( isCovariantWith!(DerivA_2.test, DerivA_2.test));

     // function, function pointer and delegate
     J function() derived_function;
     I function() base_function;
     J delegate() derived_delegate;
     I delegate() base_delegate;
     static assert(.isCovariantWith!(typeof(derived_function), typeof(base_function)));
     static assert(.isCovariantWith!(typeof(*derived_function), typeof(*base_function)));
     static assert(.isCovariantWith!(typeof(derived_delegate), typeof(base_delegate)));

    // scope parameter
    interface BaseB            {          void test(      int*,       int*); }
    interface DerivB_1 : BaseB { override void test(scope int*,       int*); }
    interface DerivB_2 : BaseB { override void test(      int*, scope int*); }
    interface DerivB_3 : BaseB { override void test(scope int*, scope int*); }
    static assert( isCovariantWith!(DerivB_1.test, BaseB.test));
    static assert( isCovariantWith!(DerivB_2.test, BaseB.test));
    static assert( isCovariantWith!(DerivB_3.test, BaseB.test));
    static assert(!isCovariantWith!(BaseB.test, DerivB_1.test));
    static assert(!isCovariantWith!(BaseB.test, DerivB_2.test));
    static assert(!isCovariantWith!(BaseB.test, DerivB_3.test));

    // function storage class
    interface BaseC            {          void test()      ; }
    interface DerivC_1 : BaseC { override void test() const; }
    static assert( isCovariantWith!(DerivC_1.test, BaseC.test));
    static assert(!isCovariantWith!(BaseC.test, DerivC_1.test));

    // increasing safety
    interface BaseE            {          void test()         ; }
    interface DerivE_1 : BaseE { override void test() @safe   ; }
    interface DerivE_2 : BaseE { override void test() @trusted; }
    static assert( isCovariantWith!(DerivE_1.test, BaseE.test));
    static assert( isCovariantWith!(DerivE_2.test, BaseE.test));
    static assert(!isCovariantWith!(BaseE.test, DerivE_1.test));
    static assert(!isCovariantWith!(BaseE.test, DerivE_2.test));

    // @safe and @trusted
    interface BaseF
    {
        void test1() @safe;
        void test2() @trusted;
    }
    interface DerivF : BaseF
    {
        override void test1() @trusted;
        override void test2() @safe;
    }
    static assert( isCovariantWith!(DerivF.test1, BaseF.test1));
    static assert( isCovariantWith!(DerivF.test2, BaseF.test2));
}


// Needed for rvalueOf/lvalueOf because "inout on return means
// inout must be on a parameter as well"
private struct __InoutWorkaroundStruct{}

/**
Creates an lvalue or rvalue of type `T` for `typeof(...)` and
$(DDSUBLINK spec/traits, compiles, `__traits(compiles, ...)`) purposes. No actual value is returned.

Params:
    T = The type to transform

Note: Trying to use returned value will result in a
"Symbol Undefined" error at link time.
*/
@property T rvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);

/// ditto
@property ref T lvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);

// Note: can't put these unittests together as function overloads
// aren't allowed inside functions.
///
@system unittest
{
    static int f(int);
    static assert(is(typeof(f(rvalueOf!int)) == int));
}

///
@system unittest
{
    static bool f(ref int);
    static assert(is(typeof(f(lvalueOf!int)) == bool));
}

@system unittest
{
    void needLvalue(T)(ref T);
    static struct S { }
    int i;
    struct Nested { void f() { ++i; } }
    static foreach (T; AliasSeq!(int, immutable int, inout int, string, S, Nested, Object))
    {
        static assert(!__traits(compiles, needLvalue(rvalueOf!T)));
        static assert( __traits(compiles, needLvalue(lvalueOf!T)));
        static assert(is(typeof(rvalueOf!T) == T));
        static assert(is(typeof(lvalueOf!T) == T));
    }

    static assert(!__traits(compiles, rvalueOf!int = 1));
    static assert( __traits(compiles, lvalueOf!byte = 127));
    static assert(!__traits(compiles, lvalueOf!byte = 128));
}


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// SomethingTypeOf
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/*
 */
template BooleanTypeOf(T)
{
    static if (is(typeof(__traits(getMember, T.init, __traits(getAliasThis, T)[0])) AT) && !is(AT[] == AT))
        alias X = BooleanTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(immutable X == immutable bool))
    {
        alias BooleanTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not boolean type");
}

@safe unittest
{
    // unexpected failure, maybe dmd type-merging bug
    static foreach (T; AliasSeq!bool)
        static foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == BooleanTypeOf!(            Q!T  )));
            static assert( is(Q!T == BooleanTypeOf!( SubTypeOf!(Q!T) )));
        }

    static foreach (T; AliasSeq!(void, NumericTypeList, /*ImaginaryTypeList, ComplexTypeList,*/ CharTypeList))
        static foreach (Q; TypeQualifierList)
        {
            static assert(!is(BooleanTypeOf!(            Q!T  )), Q!T.stringof);
            static assert(!is(BooleanTypeOf!( SubTypeOf!(Q!T) )));
        }
}

@safe unittest
{
    struct B
    {
        bool val;
        alias val this;
    }
    struct S
    {
        B b;
        alias b this;
    }
    static assert(is(BooleanTypeOf!B == bool));
    static assert(is(BooleanTypeOf!S == bool));
}

/*
 */
template IntegralTypeOf(T)
{
    static if (is(typeof(__traits(getMember, T.init, __traits(getAliasThis, T)[0])) AT) && !is(AT[] == AT))
        alias X = IntegralTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (__traits(isIntegral, X) && __traits(isZeroInit, X) // Not char, wchar, or dchar.
        && !is(immutable X == immutable bool) && !is(X == __vector))
    {
        alias IntegralTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not an integral type");
}

@safe unittest
{
    static foreach (T; IntegralTypeList)
        static foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == IntegralTypeOf!(            Q!T  )));
            static assert( is(Q!T == IntegralTypeOf!( SubTypeOf!(Q!T) )));
        }

    static foreach (T; AliasSeq!(void, bool, FloatingPointTypeList,
                /*ImaginaryTypeList, ComplexTypeList,*/ CharTypeList))
        static foreach (Q; TypeQualifierList)
        {
            static assert(!is(IntegralTypeOf!(            Q!T  )));
            static assert(!is(IntegralTypeOf!( SubTypeOf!(Q!T) )));
        }
}

/*
 */
template FloatingPointTypeOf(T)
{
    static if (is(typeof(__traits(getMember, T.init, __traits(getAliasThis, T)[0])) AT) && !is(AT[] == AT))
        alias X = FloatingPointTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(immutable X == immutable U, U) && is(U == float) || is(U == double) || is(U == real))
    {
        alias FloatingPointTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not a floating point type");
}

@safe unittest
{
    static foreach (T; FloatingPointTypeList)
        static foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == FloatingPointTypeOf!(            Q!T  )));
            static assert( is(Q!T == FloatingPointTypeOf!( SubTypeOf!(Q!T) )));
        }

    static foreach (T; AliasSeq!(void, bool, IntegralTypeList, /*ImaginaryTypeList, ComplexTypeList,*/ CharTypeList))
        static foreach (Q; TypeQualifierList)
        {
            static assert(!is(FloatingPointTypeOf!(            Q!T  )));
            static assert(!is(FloatingPointTypeOf!( SubTypeOf!(Q!T) )));
        }
}

/*
 */
template NumericTypeOf(T)
{
    static if (is(IntegralTypeOf!T X) || is(FloatingPointTypeOf!T X))
    {
        alias NumericTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not a numeric type");
}

@safe unittest
{
    static foreach (T; NumericTypeList)
        static foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == NumericTypeOf!(            Q!T  )));
            static assert( is(Q!T == NumericTypeOf!( SubTypeOf!(Q!T) )));
        }

    static foreach (T; AliasSeq!(void, bool, CharTypeList, /*ImaginaryTypeList, ComplexTypeList*/))
        static foreach (Q; TypeQualifierList)
        {
            static assert(!is(NumericTypeOf!(            Q!T  )));
            static assert(!is(NumericTypeOf!( SubTypeOf!(Q!T) )));
        }
}

/*
 */
template UnsignedTypeOf(T)
{
    static if (is(IntegralTypeOf!T X) && __traits(isUnsigned, X))
        alias UnsignedTypeOf = X;
    else
        static assert(0, T.stringof~" is not an unsigned type.");
}

/*
 */
template SignedTypeOf(T)
{
    static if (is(IntegralTypeOf!T X) && !__traits(isUnsigned, X))
        alias SignedTypeOf = X;
    else static if (is(FloatingPointTypeOf!T X))
        alias SignedTypeOf = X;
    else
        static assert(0, T.stringof~" is not an signed type.");
}

/*
 */
template CharTypeOf(T)
{
    static if (is(typeof(__traits(getMember, T.init, __traits(getAliasThis, T)[0])) AT) && !is(AT[] == AT))
        alias X = CharTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(immutable X == immutable U, U) && is(U == char) || is(U == wchar) || is(U == dchar))
    {
        alias CharTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not a character type");
}

@safe unittest
{
    static foreach (T; CharTypeList)
        static foreach (Q; TypeQualifierList)
        {
            static assert( is(CharTypeOf!(            Q!T  )));
            static assert( is(CharTypeOf!( SubTypeOf!(Q!T) )));
        }

    static foreach (T; AliasSeq!(void, bool, NumericTypeList, /*ImaginaryTypeList, ComplexTypeList*/))
        static foreach (Q; TypeQualifierList)
        {
            static assert(!is(CharTypeOf!(            Q!T  )));
            static assert(!is(CharTypeOf!( SubTypeOf!(Q!T) )));
        }

    static foreach (T; AliasSeq!(string, wstring, dstring, char[4]))
        static foreach (Q; TypeQualifierList)
        {
            static assert(!is(CharTypeOf!(            Q!T  )));
            static assert(!is(CharTypeOf!( SubTypeOf!(Q!T) )));
        }
}

/*
 */
template StaticArrayTypeOf(T)
{
    static if (is(typeof(__traits(getMember, T.init, __traits(getAliasThis, T)[0])) AT) && !is(AT[] == AT))
        alias X = StaticArrayTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (__traits(isStaticArray, X))
        alias StaticArrayTypeOf = X;
    else
        static assert(0, T.stringof~" is not a static array type");
}

@safe unittest
{
    static foreach (T; AliasSeq!(bool, NumericTypeList, /*ImaginaryTypeList, ComplexTypeList*/))
        static foreach (Q; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
        {
            static assert(is( Q!(   T[1] ) == StaticArrayTypeOf!( Q!(              T[1]  ) ) ));

            static foreach (P; TypeQualifierList)
            { // SubTypeOf cannot have inout type
                static assert(is( Q!(P!(T[1])) == StaticArrayTypeOf!( Q!(SubTypeOf!(P!(T[1]))) ) ));
            }
        }

    static foreach (T; AliasSeq!void)
        static foreach (Q; AliasSeq!TypeQualifierList)
        {
            static assert(is( StaticArrayTypeOf!( Q!(void[1]) ) == Q!(void[1]) ));
        }
}

/*
 */
template DynamicArrayTypeOf(T)
{
    import core.internal.traits : _DynamicArrayTypeOf = DynamicArrayTypeOf;
    alias DynamicArrayTypeOf = _DynamicArrayTypeOf!T;
}

@safe unittest
{
    import std.meta : Alias;
    static foreach (T; AliasSeq!(/*void, */bool, NumericTypeList, /*ImaginaryTypeList, ComplexTypeList*/))
        static foreach (Q; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
        {
            static assert(is( Q!T[]  == DynamicArrayTypeOf!( Q!T[] ) ));
            static assert(is( Q!(T[])  == DynamicArrayTypeOf!( Q!(T[]) ) ));

            static foreach (P; AliasSeq!(Alias, ConstOf, ImmutableOf))
            {
                static assert(is( Q!(P!T[]) == DynamicArrayTypeOf!( Q!(SubTypeOf!(P!T[])) ) ));
                static assert(is( Q!(P!(T[])) == DynamicArrayTypeOf!( Q!(SubTypeOf!(P!(T[]))) ) ));
            }
        }

    static assert(!is(DynamicArrayTypeOf!(int[3])));
    static assert(!is(DynamicArrayTypeOf!(void[3])));
    static assert(!is(DynamicArrayTypeOf!(typeof(null))));
}

/*
 */
template ArrayTypeOf(T)
{
    static if (is(StaticArrayTypeOf!T X) || is(DynamicArrayTypeOf!T X))
    {
        alias ArrayTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not an array type");
}

/*
 * Converts strings and string-like types to the corresponding dynamic array of characters.
 * Params:
 * T = one of the following:
 * 1. dynamic arrays of `char`, `wchar`, or `dchar` that are implicitly convertible to `const`
 *    (`shared` is rejected)
 * 2. static arrays of `char`, `wchar`, or `dchar` that are implicitly convertible to `const`
 *    (`shared` is rejected)
 * 3. aggregates that use `alias this` to refer to a field that is (1), (2), or (3)
 *
 * Other cases are rejected with a compile time error.
 * `typeof(null)` is rejected.
 *
 * Returns:
 *  The result of `[]` applied to the qualified character type.
 */
template StringTypeOf(T)
{
    static if (is(T == typeof(null)))
    {
        // It is impossible to determine exact string type from typeof(null) -
        // it means that StringTypeOf!(typeof(null)) is undefined.
        // Then this behavior is convenient for template constraint.
        static assert(0, T.stringof~" is not a string type");
    }
    else static if (is(T : const char[]) || is(T : const wchar[]) || is(T : const dchar[]))
    {
        static if (is(T : U[], U))
            alias StringTypeOf = U[];
        else
            static assert(0);
    }
    else
        static assert(0, T.stringof~" is not a string type");
}

@safe unittest
{
    import std.meta : Alias;
    static foreach (T; CharTypeList)
        static foreach (Q; AliasSeq!(Alias, ConstOf, ImmutableOf, InoutOf))
        {
            static assert(is(Q!T[] == StringTypeOf!( Q!T[] )));

            static if (!__traits(isSame, Q, InoutOf))
            {{
                static assert(is(Q!T[] == StringTypeOf!( SubTypeOf!(Q!T[]) )));

                alias Str = Q!T[];
                struct C(S) { S val;  alias val this; }
                static assert(is(StringTypeOf!(C!Str) == Str));
            }}
        }

    static foreach (T; CharTypeList)
        static foreach (Q; AliasSeq!(SharedOf, SharedConstOf, SharedInoutOf))
        {
            static assert(!is(StringTypeOf!( Q!T[] )));
        }
}

@safe unittest
{
    static assert(is(StringTypeOf!(char[4]) == char[]));

    struct S
    {
        string s;
        alias s this;
    }

    struct T
    {
        S s;
        alias s this;
    }

    static assert(is(StringTypeOf!S == string));
    static assert(is(StringTypeOf!T == string));
}

/*
 */
template AssocArrayTypeOf(T)
{
    static if (is(typeof(__traits(getMember, T.init, __traits(getAliasThis, T)[0])) AT) && !is(AT[] == AT))
        alias X = AssocArrayTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (__traits(isAssociativeArray, X))
    {
        alias AssocArrayTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not an associative array type");
}

@safe unittest
{
    static foreach (T; AliasSeq!(int/*bool, CharTypeList, NumericTypeList, ImaginaryTypeList, ComplexTypeList*/))
        static foreach (P; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
            static foreach (Q; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
                static foreach (R; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
                {
                    static assert(is( P!(Q!T[R!T]) == AssocArrayTypeOf!(            P!(Q!T[R!T])  ) ));
                }

    static foreach (T; AliasSeq!(int/*bool, CharTypeList, NumericTypeList, ImaginaryTypeList, ComplexTypeList*/))
        static foreach (O; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
            static foreach (P; AliasSeq!TypeQualifierList)
                static foreach (Q; AliasSeq!TypeQualifierList)
                    static foreach (R; AliasSeq!TypeQualifierList)
                    {
                        static assert(is( O!(P!(Q!T[R!T])) == AssocArrayTypeOf!( O!(SubTypeOf!(P!(Q!T[R!T]))) ) ));
                    }
}

/*
 */
template BuiltinTypeOf(T)
{
    static if (is(T : void))
        alias BuiltinTypeOf = void;
    else
    {
        static if (is(typeof(__traits(getMember, T.init, __traits(getAliasThis, T)[0])) AT) && !is(AT[] == AT))
            alias X = BuiltinTypeOf!AT;
        else
            alias X = OriginalType!T;
        static if (__traits(isArithmetic, X) && !is(X == __vector) ||
                __traits(isStaticArray, X) || is(X == E[], E) ||
                __traits(isAssociativeArray, X) || is(X == typeof(null)))
            alias BuiltinTypeOf = X;
        else
            static assert(0);
    }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// isSomething
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
 * Detect whether `T` is a built-in boolean type or enum of boolean base type.
 */
enum bool isBoolean(T) = __traits(isUnsigned, T) && is(T : bool);

///
@safe unittest
{
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
    static struct S(T)
    {
        T t;
        alias t this;
    }
    static assert(!isIntegral!(S!bool));
}

/**
 * Detect whether `T` is a built-in integral type.
 * Integral types are `byte`, `ubyte`, `short`, `ushort`, `int`, `uint`, `long`, `ulong`, `cent`, `ucent`,
 * and enums with an integral type as its base type.
 * Params:
 *      T = type to test
 * Returns:
 *      `true` if `T` is an integral type
 * Note:
 *      this is not the same as $(LINK2 https://dlang.org/spec/traits.html#isIntegral, `__traits(isIntegral)`)
 */
template isIntegral(T)
{
    static if (!__traits(isIntegral, T))
        enum isIntegral = false;
    else static if (is(T U == enum))
        enum isIntegral = isIntegral!U;
    else
        enum isIntegral = __traits(isZeroInit, T) // Not char, wchar, or dchar.
            && !is(immutable T == immutable bool) && !is(T == __vector);
}

///
@safe unittest
{
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
    static foreach (T; IntegralTypeList)
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isIntegral!(Q!T));
            static assert(!isIntegral!(SubTypeOf!(Q!T)));
        }
    }

    static assert(!isIntegral!float);

    enum EU : uint { a = 0, b = 1, c = 2 }  // base type is unsigned
    // base type is signed (https://issues.dlang.org/show_bug.cgi?id=7909)
    enum EI : int { a = -1, b = 0, c = 1 }
    static assert(isIntegral!EU &&  isUnsigned!EU && !isSigned!EU);
    static assert(isIntegral!EI && !isUnsigned!EI &&  isSigned!EI);
}

/**
 * Detect whether `T` is a built-in floating point type.
 *
 * See also: $(DDSUBLINK spec/traits, isFloating, `__traits(isFloating, T)`)
 */
// is(T : real) to discount complex types
enum bool isFloatingPoint(T) = __traits(isFloating, T) && is(T : real);

///
@safe unittest
{
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
    enum EF : real { a = 1.414, b = 1.732, c = 2.236 }

    static foreach (T; AliasSeq!(FloatingPointTypeList, EF))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isFloatingPoint!(Q!T));
            static assert(!isFloatingPoint!(SubTypeOf!(Q!T)));
        }
    }
    static foreach (T; IntegralTypeList)
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert(!isFloatingPoint!(Q!T));
        }
    }
    static if (is(__vector(float[4])))
    {
        static assert(!isFloatingPoint!(__vector(float[4])));
    }
}

/**
 * Detect whether `T` is a built-in numeric type (integral or floating
 * point).
 */
template isNumeric(T)
{
    static if (!__traits(isArithmetic, T))
        enum isNumeric = false;
    else static if (__traits(isFloating, T))
        enum isNumeric = is(T : real); // Not __vector, imaginary, or complex.
    else static if (is(T U == enum))
        enum isNumeric = isNumeric!U;
    else
        enum isNumeric = __traits(isZeroInit, T) // Not char, wchar, or dchar.
            && !is(immutable T == immutable bool) && !is(T == __vector);
}

///
@safe unittest
{
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
    static foreach (T; AliasSeq!(NumericTypeList))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isNumeric!(Q!T));
            static assert(!isNumeric!(SubTypeOf!(Q!T)));
        }
    }

    static struct S(T)
    {
        T t;
        alias t this;
    }
    static assert(!isNumeric!(S!int));

    enum EChar : char { a = 0, }
    static assert(!isNumeric!EChar);

    static if (is(__vector(float[4])))
    {
        static assert(!isNumeric!(__vector(float[4])));
    }
    static if (is(__vector(int[4])))
    {
        static assert(!isNumeric!(__vector(int[4])));
    }

    static assert(!isNumeric!ifloat);
    static assert(!isNumeric!cfloat);
}

/**
 * Detect whether `T` is a scalar type (a built-in numeric, character or
 * boolean type).
 *
 * See also: $(DDSUBLINK spec/traits, isScalar, `__traits(isScalar, T)`)
 */
// is(T : real) to discount complex types
enum bool isScalarType(T) = __traits(isScalar, T) && is(T : real);

///
@safe unittest
{
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
    static struct S(T)
    {
        T t;
        alias t this;
    }
    static assert(!isScalarType!(S!int));
}

/**
 * Detect whether `T` is a basic type (scalar type or void).
 */
enum bool isBasicType(T) = isScalarType!T || is(immutable T == immutable void);

///
@safe unittest
{
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

/**
 * Detect whether `T` is a built-in unsigned numeric type.
 */
template isUnsigned(T)
{
    static if (!__traits(isUnsigned, T))
        enum isUnsigned = false;
    else static if (is(T U == enum))
        enum isUnsigned = isUnsigned!U;
    else
        enum isUnsigned = __traits(isZeroInit, T) // Not char, wchar, or dchar.
            && !is(immutable T == immutable bool) && !is(T == __vector);
}

///
@safe unittest
{
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
    static foreach (T; AliasSeq!(UnsignedIntTypeList))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isUnsigned!(Q!T));
            static assert(!isUnsigned!(SubTypeOf!(Q!T)));
        }
    }

    static struct S(T)
    {
        T t;
        alias t this;
    }
    static assert(!isUnsigned!(S!uint));

    enum EChar : char { a = 0, }
    static assert(!isUnsigned!EChar);

    static if (is(__vector(uint[4])))
    {
        static assert(!isUnsigned!(__vector(uint[4])));
    }
}

/**
 * Detect whether `T` is a built-in signed numeric type.
 */
enum bool isSigned(T) = __traits(isArithmetic, T) && !__traits(isUnsigned, T)
                                                  && is(T : real);

///
@safe unittest
{
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
    enum E { e1 = 0 }
    static assert(isSigned!E);

    enum Eubyte : ubyte { e1 = 0 }
    static assert(!isSigned!Eubyte);

    static foreach (T; AliasSeq!(SignedIntTypeList))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isSigned!(Q!T));
            static assert(!isSigned!(SubTypeOf!(Q!T)));
        }
    }

    static struct S(T)
    {
        T t;
        alias t this;
    }
    static assert(!isSigned!(S!uint));

    static if (is(__vector(int[4])))
    {
        static assert(!isSigned!(__vector(int[4])));
    }

    static assert(!isSigned!ifloat);
    static assert(!isSigned!cfloat);
}

// https://issues.dlang.org/show_bug.cgi?id=17196
@safe unittest
{
    static assert(isUnsigned!bool == false);
    static assert(isSigned!bool == false);
}

/**
 * Detect whether `T` is one of the built-in character types.
 *
 * The built-in char types are any of `char`, `wchar` or `dchar`, with
 * or without qualifiers.
 */
template isSomeChar(T)
{
    static if (!__traits(isUnsigned, T))
        enum isSomeChar = false;
    else static if (is(T U == enum))
        enum isSomeChar = isSomeChar!U;
    else
        enum isSomeChar = !__traits(isZeroInit, T);
}

///
@safe unittest
{
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
    enum EC : char { a = 'x', b = 'y' }

    static foreach (T; AliasSeq!(CharTypeList, EC))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isSomeChar!(            Q!T  ));
            static assert(!isSomeChar!( SubTypeOf!(Q!T) ));
        }
    }

    // alias-this types are not allowed
    static struct S(T)
    {
        T t;
        alias t this;
    }
    static assert(!isSomeChar!(S!char));
}

/**
Detect whether `T` is one of the built-in string types.

The built-in string types are `Char[]`, where `Char` is any of `char`,
`wchar` or `dchar`, with or without qualifiers.

Static arrays of characters (like `char[80]`) are not considered
built-in string types.
 */
enum bool isSomeString(T) = is(immutable T == immutable C[], C) && (is(C == char) || is(C == wchar) || is(C == dchar));

///
@safe unittest
{
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
    static foreach (T; AliasSeq!(char[], dchar[], string, wstring, dstring))
    {
        static assert( isSomeString!(           T ));
        static assert(!isSomeString!(SubTypeOf!(T)));
    }
    enum C : char { _ = 0 }
    static assert(!isSomeString!(C[]));
}

/**
 * Detect whether type `T` is a narrow string.
 *
 * All arrays that use char, wchar, and their qualified versions are narrow
 * strings. (Those include string and wstring).
 */
enum bool isNarrowString(T) = is(immutable T == immutable C[], C) && (is(C == char) || is(C == wchar));

///
@safe unittest
{
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
    import std.meta : Alias;
    static foreach (T; AliasSeq!(char[], string, wstring))
    {
        static foreach (Q; AliasSeq!(Alias, ConstOf, ImmutableOf)/*TypeQualifierList*/)
        {
            static assert( isNarrowString!(            Q!T  ));
            static assert(!isNarrowString!( SubTypeOf!(Q!T) ));
        }
    }

    static foreach (T; AliasSeq!(int, int[], byte[], dchar[], dstring, char[4]))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert(!isNarrowString!(            Q!T  ));
            static assert(!isNarrowString!( SubTypeOf!(Q!T) ));
        }
    }
    enum C : char { _ = 0 }
    static assert(!isNarrowString!(C[]));
}

/**
 * Detects whether `T` is a comparable type. Basic types and structs and
 * classes that implement opCmp are ordering comparable.
 */
enum bool isOrderingComparable(T) = is(typeof((ref T a) => a < a ? 1 : 0));

///
@safe unittest
{
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

/// ditto
enum bool isEqualityComparable(T) = is(typeof((ref T a) => a == a ? 1 : 0));

@safe unittest
{
    static assert(isEqualityComparable!int);
    static assert(isEqualityComparable!string);
    static assert(!isEqualityComparable!void);

    struct Foo {}
    static assert(isEqualityComparable!Foo);

    struct Bar
    {
        int a;
        auto opEquals(Bar b1) const { return a == b1.a; }
    }

    Bar b1 = Bar(5);
    Bar b2 = Bar(5);
    Bar b3 = Bar(7);
    static assert(isEqualityComparable!Bar);
    assert(b1 == b2);
    assert(b1 != b3);
}

/**
  $(RED Warning: This trait will be deprecated as soon as it is no longer used
                 in Phobos. For a function parameter to safely accept a type
                 that implicitly converts to string as a string, the conversion
                 needs to happen at the callsite; otherwise, the conversion is
                 done inside the function, and in many cases, that means that
                 local memory is sliced (e.g. if a static array is passed to
                 the function, then it's copied, and the resulting dynamic
                 array will be a slice of a local variable). So, if the
                 resulting string escapes the function, the string refers to
                 invalid memory, and accessing it would mean accessing invalid
                 memory. As such, the only safe way for a function to accept
                 types that implicitly convert to string is for the implicit
                 conversion to be done at the callsite, and that can only occur
                 if the parameter is explicitly typed as an array, whereas
                 using isConvertibleToString in a template constraint would
                 result in the conversion being done inside the function. As
                 such, isConvertibleToString is inherently unsafe and is going
                 to be deprecated.)

   Detect whether `T` is a struct, static array, or enum that is implicitly
   convertible to a string.
 */
template isConvertibleToString(T)
{
    enum isConvertibleToString =
        (isAggregateType!T || isStaticArray!T || is(T == enum))
        && is(StringTypeOf!T);
}

///
@safe unittest
{
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

// https://issues.dlang.org/show_bug.cgi?id=16573
@safe unittest
{
    enum I : int { foo = 1 }
    enum S : string { foo = "foo" }
    assert(!isConvertibleToString!I);
    assert(isConvertibleToString!S);
}

package template convertToString(T)
{
    static if (isConvertibleToString!T)
        alias convertToString = StringTypeOf!T;
    else
        alias convertToString = T;
}

/**
 * Detect whether type `T` is a string that will be autodecoded.
 *
 * Given a type `S` that is one of:
 * $(OL
 *  $(LI `const(char)[]`)
 *  $(LI `const(wchar)[]`)
 * )
 * Type `T` can be one of:
 * $(OL
 *    $(LI `S`)
 *    $(LI implicitly convertible to `T`)
 *    $(LI an enum with a base type `T`)
 *    $(LI an aggregate with a base type `T`)
 * )
 * with the proviso that `T` cannot be a static array.
 *
 * Params:
 *      T = type to be tested
 *
 * Returns:
 *      true if T represents a string that is subject to autodecoding
 *
 * See Also:
 *      $(LREF isNarrowString)
 */
template isAutodecodableString(T)
{
    import std.range.primitives : autodecodeStrings;

    enum isAutodecodableString = autodecodeStrings &&
        (is(T : const char[]) || is(T : const wchar[]))
        && !is(T : U[n], U, size_t n)
        && !is(immutable T : immutable noreturn[]);
}

///
@safe unittest
{
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

/**
 * Detect whether type `T` is a static array.
 *
 * See also: $(DDSUBLINK spec/traits, isStaticArray, `__traits(isStaticArray, T)`)
 */
enum bool isStaticArray(T) = __traits(isStaticArray, T);

///
@safe unittest
{
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
    static foreach (T; AliasSeq!(int[51], int[][2],
                           char[][int][11], immutable char[13u],
                           const(real)[1], const(real)[1][1], void[0]))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isStaticArray!(            Q!T  ));
            static assert(!isStaticArray!( SubTypeOf!(Q!T) ));
        }
    }

    //enum ESA : int[1] { a = [1], b = [2] }
    //static assert( isStaticArray!ESA);
}

/**
 * Detect whether type `T` is a dynamic array.
 */
template isDynamicArray(T)
{
    static if (is(T == U[], U))
        enum bool isDynamicArray = true;
    else static if (is(T U == enum))
        // BUG: isDynamicArray / isStaticArray considers enums
        // with appropriate base types as dynamic/static arrays
        // Retain old behaviour for now, see
        // https://github.com/dlang/phobos/pull/7574
        enum bool isDynamicArray = isDynamicArray!U;
    else
        enum bool isDynamicArray = false;
}

///
@safe unittest
{
    static assert( isDynamicArray!(int[]));
    static assert( isDynamicArray!(string));
    static assert( isDynamicArray!(long[3][]));

    static assert(!isDynamicArray!(int[5]));
    static assert(!isDynamicArray!(typeof(null)));
}

@safe unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(int[], char[], string, long[3][], double[string][]))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isDynamicArray!(            Q!T  ));
            static assert(!isDynamicArray!( SubTypeOf!(Q!T) ));
        }
    }

    static assert(!isDynamicArray!(int[5]));

    static struct AliasThis
    {
        int[] values;
        alias values this;
    }

    static assert(!isDynamicArray!AliasThis);

    // https://github.com/dlang/phobos/pull/7574/files#r464115492
    enum E : string
    {
        a = "a",
        b = "b",
    }
    static assert( isDynamicArray!E);
}

/**
 * Detect whether type `T` is an array (static or dynamic; for associative
 *  arrays see $(LREF isAssociativeArray)).
 */
enum bool isArray(T) = isStaticArray!T || isDynamicArray!T;

///
@safe unittest
{
    static assert( isArray!(int[]));
    static assert( isArray!(int[5]));
    static assert( isArray!(string));

    static assert(!isArray!uint);
    static assert(!isArray!(uint[uint]));
    static assert(!isArray!(typeof(null)));
}

@safe unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(int[], int[5], void[]))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isArray!(Q!T));
            static assert(!isArray!(SubTypeOf!(Q!T)));
        }
    }
}

/**
 * Detect whether `T` is an associative array type
 *
 * See also: $(DDSUBLINK spec/traits, isAssociativeArray, `__traits(isAssociativeArray, T)`)
 */
enum bool isAssociativeArray(T) = __traits(isAssociativeArray, T);

///
@safe unittest
{
    struct S;

    static assert( isAssociativeArray!(int[string]));
    static assert( isAssociativeArray!(S[S]));
    static assert(!isAssociativeArray!(string[]));
    static assert(!isAssociativeArray!S);
    static assert(!isAssociativeArray!(int[4]));
}

@safe unittest
{
    struct Foo
    {
        @property uint[] keys()   { return null; }
        @property uint[] values() { return null; }
    }

    static foreach (T; AliasSeq!(int[int], int[string], immutable(char[5])[int]))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isAssociativeArray!(Q!T));
            static assert(!isAssociativeArray!(SubTypeOf!(Q!T)));
        }
    }

    static assert(!isAssociativeArray!Foo);
    static assert(!isAssociativeArray!int);
    static assert(!isAssociativeArray!(int[]));
    static assert(!isAssociativeArray!(typeof(null)));

    //enum EAA : int[int] { a = [1:1], b = [2:2] }
    //static assert( isAssociativeArray!EAA);
}

/**
 * Detect whether type `T` is a builtin type.
 */
enum bool isBuiltinType(T) = is(BuiltinTypeOf!T) && !isAggregateType!T;

///
@safe unittest
{
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

/**
 * Detect whether type `T` is a SIMD vector type.
 */
enum bool isSIMDVector(T) = is(T : __vector(V[N]), V, size_t N);

///
@safe unittest
{
    static if (is(__vector(float[4])))
    {
        alias SimdVec = __vector(float[4]);
        static assert(isSIMDVector!(__vector(float[4])));
        static assert(isSIMDVector!SimdVec);
    }
    static assert(!isSIMDVector!uint);
    static assert(!isSIMDVector!(float[4]));
}

/**
 * Detect whether type `T` is a pointer.
 */
enum bool isPointer(T) = is(T == U*, U);

///
@safe unittest
{
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
    static foreach (T; AliasSeq!(int*, void*, char[]*))
    {
        static foreach (Q; TypeQualifierList)
        {
            static assert( isPointer!(Q!T));
            static assert(!isPointer!(SubTypeOf!(Q!T)));
        }
    }

    static assert(!isPointer!uint);
    static assert(!isPointer!(uint[uint]));
    static assert(!isPointer!(char[]));
    static assert(!isPointer!(typeof(null)));
}

/**
Returns the target type of a pointer.
*/
alias PointerTarget(T : T*) = T;

///
@safe unittest
{
    static assert(is(PointerTarget!(int*) == int));
    static assert(is(PointerTarget!(void*) == void));
}

/**
 * Detect whether type `T` is an aggregate type.
 */
enum bool isAggregateType(T) = is(T == struct) || is(T == union) ||
                               is(T == class) || is(T == interface);

///
@safe unittest
{
    class C;
    union U;
    struct S;
    interface I;

    static assert( isAggregateType!C);
    static assert( isAggregateType!U);
    static assert( isAggregateType!S);
    static assert( isAggregateType!I);
    static assert(!isAggregateType!void);
    static assert(!isAggregateType!string);
    static assert(!isAggregateType!(int[]));
    static assert(!isAggregateType!(C[string]));
    static assert(!isAggregateType!(void delegate(int)));
}

/**
 * Returns `true` if T can be iterated over using a `foreach` loop with
 * a single loop variable of automatically inferred type, regardless of how
 * the `foreach` loop is implemented.  This includes ranges, structs/classes
 * that define `opApply` with a single loop variable, and builtin dynamic,
 * static and associative arrays.
 */
enum bool isIterable(T) = is(typeof({ foreach (elem; T.init) {} }));

///
@safe unittest
{
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

/**
 * Returns true if T is not const or immutable.  Note that isMutable is true for
 * string, or immutable(char)[], because the 'head' is mutable.
 */
enum bool isMutable(T) = !is(T == const) && !is(T == immutable) && !is(T == inout);

///
@safe unittest
{
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

/**
 * Returns true if T is an instance of the template S.
 */
enum bool isInstanceOf(alias S, T) = is(T == S!Args, Args...);
/// ditto
template isInstanceOf(alias S, alias T)
{
    enum impl(alias T : S!Args, Args...) = true;
    enum impl(alias T) = false;
    enum isInstanceOf = impl!T;
}

///
@safe unittest
{
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

/**
 * To use `isInstanceOf` to check the identity of a template while inside of said
 * template, use $(LREF TemplateOf).
 */
@safe unittest
{
    static struct A(T = void)
    {
        // doesn't work as expected, only accepts A when T = void
        void func(B)(B b) if (isInstanceOf!(A, B)) {}

        // correct behavior
        void method(B)(B b) if (isInstanceOf!(TemplateOf!(A), B)) {}
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
    static void fun1(T)() { }
    static void fun2(T)() { }
    template templ1(T) { }
    template templ2(T) { }

    static assert(!isInstanceOf!(fun1, fun2!int));
    static assert(!isInstanceOf!(templ1, templ2!int));
}

/**
 * Check whether the tuple T is an expression tuple.
 * An expression tuple only contains expressions.
 *
 * See_Also: $(LREF isTypeTuple).
 */
template isExpressions(T...)
{
    static foreach (Ti; T)
    {
        static if (!is(typeof(isExpressions) == bool) && // not yet defined
                   (is(Ti) || !__traits(compiles, { auto ex = Ti; })))
        {
            enum isExpressions = false;
        }
    }
    static if (!is(typeof(isExpressions) == bool)) // if not yet defined
    {
        enum isExpressions = true;
    }
}

///
@safe unittest
{
    static assert(isExpressions!(1, 2.0, "a"));
    static assert(!isExpressions!(int, double, string));
    static assert(!isExpressions!(int, 2.0, "a"));
}

/**
 * Alternate name for $(LREF isExpressions), kept for legacy compatibility.
 */

alias isExpressionTuple = isExpressions;

@safe unittest
{
    void foo();
    static int bar() { return 42; }
    immutable aa = [ 1: -1 ];
    alias myint = int;

    static assert( isExpressionTuple!(42));
    static assert( isExpressionTuple!aa);
    static assert( isExpressionTuple!("cattywampus", 2.7, aa));
    static assert( isExpressionTuple!(bar()));

    static assert(!isExpressionTuple!isExpressionTuple);
    static assert(!isExpressionTuple!foo);
    static assert(!isExpressionTuple!( (a) { } ));
    static assert(!isExpressionTuple!int);
    static assert(!isExpressionTuple!myint);
}


/**
 * Check whether the tuple `T` is a type tuple.
 * A type tuple only contains types.
 *
 * See_Also: $(LREF isExpressions).
 */
enum isTypeTuple(T...) =
{
    static foreach (U; T)
        static if (!is(U))
            if (__ctfe)
                return false;
    return true;
}();

///
@safe unittest
{
    static assert(isTypeTuple!(int, float, string));
    static assert(!isTypeTuple!(1, 2.0, "a"));
    static assert(!isTypeTuple!(1, double, string));
}

@safe unittest
{
    class C {}
    void func(int) {}
    auto c = new C;
    enum CONST = 42;

    static assert( isTypeTuple!int);
    static assert( isTypeTuple!string);
    static assert( isTypeTuple!C);
    static assert( isTypeTuple!(typeof(func)));
    static assert( isTypeTuple!(int, char, double));

    static assert(!isTypeTuple!c);
    static assert(!isTypeTuple!isTypeTuple);
    static assert(!isTypeTuple!CONST);
}


/**
Detect whether symbol or type `T` is a function pointer.
 */
enum bool isFunctionPointer(alias T) = is(typeof(*T) == function);

///
@safe unittest
{
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

/**
Detect whether symbol or type `T` is a delegate.
*/
enum bool isDelegate(alias T) = is(typeof(T) == delegate) || is(T == delegate);

///
@safe unittest
{
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

/**
Detect whether symbol or type `T` is a function, a function pointer or a delegate.

Params:
    T = The type to check
Returns:
    A `bool`
 */
enum bool isSomeFunction(alias T) =
    is(T == return) ||
    is(typeof(T) == return) ||
    is(typeof(&T) == return); // @property

///
@safe unittest
{
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
    void nestedFunc() { }
    void nestedProp() @property { }
    static assert(isSomeFunction!nestedFunc);
    static assert(isSomeFunction!nestedProp);
    static assert(isSomeFunction!(real function(ref int)));
    static assert(isSomeFunction!(real delegate(ref int)));
    static assert(isSomeFunction!((int a) { return a; }));
    static assert(!isSomeFunction!isSomeFunction);
}

/**
Detect whether `T` is a callable object, which can be called with the
function call operator `$(LPAREN)...$(RPAREN)`.
 */
template isCallable(alias callable)
{
    static if (is(typeof(&callable.opCall) == delegate))
        // T is a object which has a member function opCall().
        enum bool isCallable = true;
    else static if (is(typeof(&callable.opCall) V : V*) && is(V == function))
        // T is a type which has a static member function opCall().
        enum bool isCallable = true;
    else static if (is(typeof(&callable.opCall!()) TemplateInstanceType))
    {
        enum bool isCallable = isCallable!TemplateInstanceType;
    }
    else static if (is(typeof(&callable!()) TemplateInstanceType))
    {
        enum bool isCallable = isCallable!TemplateInstanceType;
    }
    else
    {
        enum bool isCallable = isSomeFunction!callable;
    }
}

/// Functions, lambdas, and aggregate types with (static) opCall.
@safe unittest
{
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

/// Templates
@safe unittest
{
    void f()() { }
    T g(T = int)(T x) { return x; }
    struct S1 { static void opCall()() { } }
    struct S2 { static T opCall(T = int)(T x) {return x; } }

    static assert( isCallable!f);
    static assert( isCallable!g);
    static assert( isCallable!S1);
    static assert( isCallable!S2);
}

/// Overloaded functions and function templates.
@safe unittest
{
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


/**
Detect whether `S` is an abstract function.

See also: $(DDSUBLINK spec/traits, isAbstractFunction, `__traits(isAbstractFunction, S)`)
Params:
    S = The symbol to check
Returns:
    A `bool`
 */
enum isAbstractFunction(alias S) = __traits(isAbstractFunction, S);

///
@safe unittest
{
    struct S { void foo() { } }
    class C { void foo() { } }
    class AC { abstract void foo(); }
    static assert(!isAbstractFunction!(int));
    static assert(!isAbstractFunction!(S.foo));
    static assert(!isAbstractFunction!(C.foo));
    static assert( isAbstractFunction!(AC.foo));
}

/**
 * Detect whether `S` is a final function.
 *
 * See also: $(DDSUBLINK spec/traits, isFinalFunction, `__traits(isFinalFunction, S)`)
 */
enum isFinalFunction(alias S) = __traits(isFinalFunction, S);

///
@safe unittest
{
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

/**
Determines if `f` is a function that requires a context pointer.

Params:
    f = The type to check
Returns
    A `bool`
*/
template isNestedFunction(alias f)
{
    enum isNestedFunction = __traits(isNested, f) && isSomeFunction!(f);
}

///
@safe unittest
{
    static void f() {}
    static void fun()
    {
        int i;
        int f() { return i; }

        static assert(isNestedFunction!(f));
    }

    static assert(!isNestedFunction!f);
}

// https://issues.dlang.org/show_bug.cgi?id=18669
@safe unittest
{
    static class Outer
    {
        class Inner
        {
        }
    }
    int i;
    struct SS
    {
        int bar() { return i; }
    }
    static assert(!isNestedFunction!(Outer.Inner));
    static assert(!isNestedFunction!(SS));
}

/**
 * Detect whether `S` is an abstract class.
 *
 * See also: $(DDSUBLINK spec/traits, isAbstractClass, `__traits(isAbstractClass, S)`)
 */
enum isAbstractClass(alias S) = __traits(isAbstractClass, S);

///
@safe unittest
{
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

/**
 * Detect whether `S` is a final class.
 *
 * See also: $(DDSUBLINK spec/traits, isFinalClass, `__traits(isFinalClass, S)`)
 */
enum isFinalClass(alias S) = __traits(isFinalClass, S);

///
@safe unittest
{
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

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// General Types
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

version (StdDdoc)
{
    /**
       Removes `const`, `inout` and `immutable` qualifiers, if any, from type `T`.
    */
    template Unconst(T)
    {
        import core.internal.traits : CoreUnconst = Unconst;
        alias Unconst = CoreUnconst!(T);
    }
}
else
{
    import core.internal.traits : CoreUnconst = Unconst;
    alias Unconst = CoreUnconst;
}

///
@safe unittest
{
    static assert(is(Unconst!int == int));
    static assert(is(Unconst!(const int) == int));
    static assert(is(Unconst!(immutable int) == int));
    static assert(is(Unconst!(shared int) == shared int));
    static assert(is(Unconst!(shared(const int)) == shared int));
}

@safe unittest
{
    static assert(is(Unconst!(                   int) == int));
    static assert(is(Unconst!(             const int) == int));
    static assert(is(Unconst!(       inout       int) == int));
    static assert(is(Unconst!(       inout const int) == int));
    static assert(is(Unconst!(shared             int) == shared int));
    static assert(is(Unconst!(shared       const int) == shared int));
    static assert(is(Unconst!(shared inout       int) == shared int));
    static assert(is(Unconst!(shared inout const int) == shared int));
    static assert(is(Unconst!(         immutable int) == int));

    alias ImmIntArr = immutable(int[]);
    static assert(is(Unconst!ImmIntArr == immutable(int)[]));
}

version (StdDdoc)
{
    /**
       Removes all qualifiers, if any, from type `T`.
    */
    template Unqual(T)
    {
        import core.internal.traits : CoreUnqual = Unqual;
        alias Unqual = CoreUnqual!(T);
    }
}
else
{
    import core.internal.traits : CoreUnqual = Unqual;
    alias Unqual = CoreUnqual;
}

///
@safe unittest
{
    static assert(is(Unqual!int == int));
    static assert(is(Unqual!(const int) == int));
    static assert(is(Unqual!(immutable int) == int));
    static assert(is(Unqual!(shared int) == int));
    static assert(is(Unqual!(shared(const int)) == int));
}

@safe unittest
{
    static assert(is(Unqual!(                   int) == int));
    static assert(is(Unqual!(             const int) == int));
    static assert(is(Unqual!(       inout       int) == int));
    static assert(is(Unqual!(       inout const int) == int));
    static assert(is(Unqual!(shared             int) == int));
    static assert(is(Unqual!(shared       const int) == int));
    static assert(is(Unqual!(shared inout       int) == int));
    static assert(is(Unqual!(shared inout const int) == int));
    static assert(is(Unqual!(         immutable int) == int));

    alias ImmIntArr = immutable(int[]);
    static assert(is(Unqual!ImmIntArr == immutable(int)[]));
}

// [For internal use]
package template ModifyTypePreservingTQ(alias Modifier, T)
{
    import core.internal.traits : _ModifyTypePreservingTQ = ModifyTypePreservingTQ;
    alias ModifyTypePreservingTQ = _ModifyTypePreservingTQ!(Modifier, T);
}

/**
 * Copies type qualifiers from `FromType` to `ToType`.
 *
 * Supported type qualifiers:
 * $(UL
 *     $(LI `const`)
 *     $(LI `inout`)
 *     $(LI `immutable`)
 *     $(LI `shared`)
 * )
 */
template CopyTypeQualifiers(FromType, ToType)
{
    alias T(U) = ToType;
    alias CopyTypeQualifiers = ModifyTypePreservingTQ!(T, FromType);
}

///
@safe unittest
{
    static assert(is(CopyTypeQualifiers!(inout const real, int) == inout const int));
}

@safe unittest
{
    static assert(is(CopyTypeQualifiers!(                   real, int) ==                    int));
    static assert(is(CopyTypeQualifiers!(             const real, int) ==              const int));
    static assert(is(CopyTypeQualifiers!(       inout       real, int) ==        inout       int));
    static assert(is(CopyTypeQualifiers!(       inout const real, int) ==        inout const int));
    static assert(is(CopyTypeQualifiers!(shared             real, int) == shared             int));
    static assert(is(CopyTypeQualifiers!(shared       const real, int) == shared       const int));
    static assert(is(CopyTypeQualifiers!(shared inout       real, int) == shared inout       int));
    static assert(is(CopyTypeQualifiers!(shared inout const real, int) == shared inout const int));
    static assert(is(CopyTypeQualifiers!(         immutable real, int) ==          immutable int));
}

/**
Returns the type of `ToType` with the "constness" of `FromType`. A type's $(B constness)
refers to whether it is `const`, `immutable`, or `inout`. If `FromType` has no constness, the
returned type will be the same as `ToType`.
*/
template CopyConstness(FromType, ToType)
{
    alias Unshared(T) = T;
    alias Unshared(T: shared U, U) = U;

    alias CopyConstness = Unshared!(CopyTypeQualifiers!(FromType, ToType));
}

///
@safe unittest
{
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
    struct Test
    {
        void method1() {}
        void method2() const {}
        void method3() immutable {}
    }

    assert(is(CopyConstness!(typeof(Test.method1), real) == real));

    assert(is(CopyConstness!(typeof(Test.method2), byte) == const(byte)));

    assert(is(CopyConstness!(typeof(Test.method3), string) == immutable(string)));
}

@safe unittest
{
    assert(is(CopyConstness!(inout(int)[], int[]) == int[]));
    assert(is(CopyConstness!(inout(int[]), int[]) == inout(int[])));
}

@safe unittest
{
    static assert(is(CopyConstness!(                   int, real) ==             real));
    static assert(is(CopyConstness!(const              int, real) ==       const real));
    static assert(is(CopyConstness!(inout              int, real) ==       inout real));
    static assert(is(CopyConstness!(inout const        int, real) == inout const real));
    static assert(is(CopyConstness!(shared             int, real) ==             real));
    static assert(is(CopyConstness!(shared const       int, real) ==       const real));
    static assert(is(CopyConstness!(shared inout       int, real) == inout       real));
    static assert(is(CopyConstness!(shared inout const int, real) == inout const real));
    static assert(is(CopyConstness!(immutable          int, real) ==   immutable real));
}

/**
Returns the inferred type of the loop variable when a variable of type T
is iterated over using a `foreach` loop with a single loop variable and
automatically inferred return type.  Note that this may not be the same as
`std.range.ElementType!Range` in the case of narrow strings, or if T
has both opApply and a range interface.
*/
template ForeachType(T)
{
    alias ForeachType = typeof(
    (inout int x = 0)
    {
        foreach (elem; T.init)
        {
            return elem;
        }
        assert(0);
    }());
}

///
@safe unittest
{
    static assert(is(ForeachType!(uint[]) == uint));
    static assert(is(ForeachType!string == immutable(char)));
    static assert(is(ForeachType!(string[string]) == string));
    static assert(is(ForeachType!(inout(int)[]) == inout(int)));
}


/**
 * Strips off all `enum`s from type `T`.
 */
template OriginalType(T)
{
    import core.internal.traits : _OriginalType = OriginalType;
    alias OriginalType = _OriginalType!T;
}

///
@safe unittest
{
    enum E : real { a = 0 } // NOTE: explicit initialization to 0 required during Enum init deprecation cycle
    enum F : E    { a = E.a }
    alias G = const(F);
    static assert(is(OriginalType!E == real));
    static assert(is(OriginalType!F == real));
    static assert(is(OriginalType!G == const real));
}

/**
 * Get the Key type of an Associative Array.
 */
alias KeyType(V : V[K], K) = K;

///
@safe unittest
{
    alias Hash = int[string];
    static assert(is(KeyType!Hash == string));
    static assert(is(ValueType!Hash == int));
    KeyType!Hash str = "a"; // str is declared as string
    ValueType!Hash num = 1; // num is declared as int
}

/**
 * Get the Value type of an Associative Array.
 */
alias ValueType(V : V[K], K) = V;

///
@safe unittest
{
    alias Hash = int[string];
    static assert(is(KeyType!Hash == string));
    static assert(is(ValueType!Hash == int));
    KeyType!Hash str = "a"; // str is declared as string
    ValueType!Hash num = 1; // num is declared as int
}

/**
Params:
    T = A built in integral or vector type.

Returns:
    The corresponding unsigned numeric type for `T` with the
    same type qualifiers.

    If `T` is not a integral or vector, a compile-time error is given.
 */
template Unsigned(T)
{
    template Impl(T)
    {
        static if (is(T : __vector(V[N]), V, size_t N))
            alias Impl = __vector(Impl!V[N]);
        else static if (isUnsigned!T)
            alias Impl = T;
        else static if (isSigned!T && !isFloatingPoint!T)
        {
            static if (is(T == byte )) alias Impl = ubyte;
            static if (is(T == short)) alias Impl = ushort;
            static if (is(T == int  )) alias Impl = uint;
            static if (is(T == long )) alias Impl = ulong;
            static if (is(ucent) && is(T == cent )) alias Impl = ucent;
        }
        else
            static assert(false, "Type " ~ T.stringof ~
                                 " does not have an Unsigned counterpart");
    }

    alias Unsigned = ModifyTypePreservingTQ!(Impl, OriginalType!T);
}

///
@safe unittest
{
    static assert(is(Unsigned!(int) == uint));
    static assert(is(Unsigned!(long) == ulong));
    static assert(is(Unsigned!(const short) == const ushort));
    static assert(is(Unsigned!(immutable byte) == immutable ubyte));
    static assert(is(Unsigned!(inout int) == inout uint));
}


/// Unsigned types are forwarded
@safe unittest
{
    static assert(is(Unsigned!(uint) == uint));
    static assert(is(Unsigned!(const uint) == const uint));

    static assert(is(Unsigned!(ubyte) == ubyte));
    static assert(is(Unsigned!(immutable uint) == immutable uint));
}

@safe unittest
{
    alias U1 = Unsigned!int;
    alias U2 = Unsigned!(const(int));
    alias U3 = Unsigned!(immutable(int));
    static assert(is(U1 == uint));
    static assert(is(U2 == const(uint)));
    static assert(is(U3 == immutable(uint)));
    static if (is(__vector(int[4])) && is(__vector(uint[4])))
    {
        alias UV1 = Unsigned!(__vector(int[4]));
        alias UV2 = Unsigned!(const(__vector(int[4])));
        static assert(is(UV1 == __vector(uint[4])));
        static assert(is(UV2 == const(__vector(uint[4]))));
    }
    //struct S {}
    //alias U2 = Unsigned!S;
    //alias U3 = Unsigned!double;
    static if (is(ucent))
    {
        alias U4 = Unsigned!cent;
        alias U5 = Unsigned!(const(cent));
        alias U6 = Unsigned!(immutable(cent));
        static assert(is(U4 == ucent));
        static assert(is(U5 == const(ucent)));
        static assert(is(U6 == immutable(ucent)));
    }
}

/**
Returns the largest type, i.e. T such that T.sizeof is the largest.  If more
than one type is of the same size, the leftmost argument of these in will be
returned.
*/
template Largest(T...)
if (T.length >= 1)
{
    alias Largest = T[0];
    static foreach (U; T[1 .. $])
        Largest = Select!(U.sizeof > Largest.sizeof, U, Largest);
}

///
@safe unittest
{
    static assert(is(Largest!(uint, ubyte, ushort, real) == real));
    static assert(is(Largest!(ulong, double) == ulong));
    static assert(is(Largest!(double, ulong) == double));
    static assert(is(Largest!(uint, byte, double, short) == double));
    static if (is(ucent))
        static assert(is(Largest!(uint, ubyte, ucent, ushort) == ucent));
}

/**
Returns the corresponding signed type for T. T must be a numeric integral type,
otherwise a compile-time error occurs.
 */
template Signed(T)
{
    template Impl(T)
    {
        static if (is(T : __vector(V[N]), V, size_t N))
            alias Impl = __vector(Impl!V[N]);
        else static if (isSigned!T)
            alias Impl = T;
        else static if (isUnsigned!T)
        {
            static if (is(T == ubyte )) alias Impl = byte;
            static if (is(T == ushort)) alias Impl = short;
            static if (is(T == uint  )) alias Impl = int;
            static if (is(T == ulong )) alias Impl = long;
            static if (is(ucent) && is(T == ucent )) alias Impl = cent;
        }
        else
            static assert(false, "Type " ~ T.stringof ~
                                 " does not have an Signed counterpart");
    }

    alias Signed = ModifyTypePreservingTQ!(Impl, OriginalType!T);
}

///
@safe unittest
{
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
    static assert(is(Signed!float == float));
    static if (is(__vector(int[4])) && is(__vector(uint[4])))
    {
        alias SV1 = Signed!(__vector(uint[4]));
        alias SV2 = Signed!(const(__vector(uint[4])));
        static assert(is(SV1 == __vector(int[4])));
        static assert(is(SV2 == const(__vector(int[4]))));
    }
}


/**
Returns the most negative value of the numeric type T.
*/
template mostNegative(T)
if (isNumeric!T || isSomeChar!T || isBoolean!T)
{
    static if (is(typeof(T.min_normal)))
        enum mostNegative = -T.max;
    else static if (T.min == 0)
        enum byte mostNegative = 0;
    else
        enum mostNegative = T.min;
}

///
@safe unittest
{
    static assert(mostNegative!float == -float.max);
    static assert(mostNegative!double == -double.max);
    static assert(mostNegative!real == -real.max);
    static assert(mostNegative!bool == false);
}

///
@safe unittest
{
    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(bool, byte, short, int, long))
        static assert(mostNegative!T == T.min);

    static foreach (T; AliasSeq!(ubyte, ushort, uint, ulong, char, wchar, dchar))
        static assert(mostNegative!T == 0);
}

/**
Get the type that a scalar type `T` will $(LINK2 $(ROOT_DIR)spec/type.html#integer-promotions, promote)
to in multi-term arithmetic expressions.
*/
template Promoted(T)
if (isScalarType!T)
{
    alias Promoted = CopyTypeQualifiers!(T, typeof(T.init + T.init));
}

///
@safe unittest
{
    ubyte a = 3, b = 5;
    static assert(is(typeof(a * b) == Promoted!ubyte));
    static assert(is(Promoted!ubyte == int));

    static assert(is(Promoted!(shared(bool)) == shared(int)));
    static assert(is(Promoted!(const(int)) == const(int)));
    static assert(is(Promoted!double == double));
}

@safe unittest
{
    // promote to int:
    static foreach (T; AliasSeq!(bool, byte, ubyte, short, ushort, char, wchar))
    {
        static assert(is(Promoted!T == int));
        static assert(is(Promoted!(shared(const T)) == shared(const int)));
    }

    // already promoted:
    static foreach (T; AliasSeq!(int, uint, long, ulong, float, double, real))
    {
        static assert(is(Promoted!T == T));
        static assert(is(Promoted!(immutable(T)) == immutable(T)));
    }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Misc.
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
Returns the mangled name of symbol or type `sth`.

`mangledName` is the same as builtin `.mangleof` property, but
might be more convenient in generic code, e.g. as a template argument
when invoking staticMap.
 */
enum mangledName(alias sth) = sth.mangleof;

///
@safe unittest
{
    import std.meta : AliasSeq;
    alias TL = staticMap!(mangledName, int, const int, immutable int);
    static assert(TL == AliasSeq!("i", "xi", "yi"));
}

version (StdUnittest) private void freeFunc(string);

@safe unittest
{
    class C { int value() @property { return 0; } }
    static assert(mangledName!int == int.mangleof);
    static assert(mangledName!C == C.mangleof);
    static assert(mangledName!(C.value) == C.value.mangleof);
    static assert(mangledName!(C.value)[$ - 12 .. $] == "5valueMFNdZi");
    static assert(mangledName!mangledName == "3std6traits11mangledName");
    static assert(mangledName!freeFunc == "_D3std6traits8freeFuncFAyaZv");
    int x;
    // https://issues.dlang.org/show_bug.cgi?id=9148
  static if (is(typeof({ return x; }) : int delegate() pure))
    static assert(mangledName!((int a) { return a+x; }) == "DFNaNbNiNfiZi");  // pure nothrow @safe @nogc
  else
    static assert(mangledName!((int a) { return a+x; }) == "DFNbNiNfiZi");  // nothrow @safe @nnogc
}

@system unittest
{
    // @system due to demangle
    // Test for https://issues.dlang.org/show_bug.cgi?id=5718
    import std.demangle : demangle;
    int foo;
    auto foo_demangled = demangle(mangledName!foo);
    assert(foo_demangled[0 .. 4] == "int " && foo_demangled[$-3 .. $] == "foo",
        foo_demangled);

    void bar();
    auto bar_demangled = demangle(mangledName!bar);
    assert(bar_demangled[0 .. 5] == "void " && bar_demangled[$-5 .. $] == "bar()");
}



// XXX Select & select should go to another module. (functional or algorithm?)

/**
Aliases itself to `T[0]` if the boolean `condition` is `true`
and to `T[1]` otherwise.
 */
template Select(bool condition, T...)
if (T.length == 2)
{
    import std.meta : Alias;
    alias Select = Alias!(T[!condition]);
}

///
@safe unittest
{
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

/**
Select one of two functions to run via template parameter.

Params:
    cond = A `bool` which determines which function is run
    a = The first function
    b = The second function

Returns:
    `a` without evaluating `b` if `cond` is `true`.
    Otherwise, returns `b` without evaluating `a`.
 */
A select(bool cond : true, A, B)(A a, lazy B b) { return a; }
/// Ditto
B select(bool cond : false, A, B)(lazy A a, B b) { return b; }

///
@safe unittest
{
    real run() { return 0; }
    int fail() { assert(0); }
    auto a = select!true(run(), fail());
    auto b = select!false(fail(), run());
    static assert(is(typeof(a) == real));
    static assert(is(typeof(b) == real));
}

/++
    Determine if a symbol has a given
    $(DDSUBLINK spec/attribute, uda, user-defined attribute).

    See_Also:
        $(LREF getUDAs)
  +/
enum hasUDA(alias symbol, alias attribute) = getUDAs!(symbol, attribute).length != 0;

///
@safe unittest
{
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

/++
    Gets the matching $(DDSUBLINK spec/attribute, uda, user-defined attributes)
    from the given symbol.

    If the UDA is a type, then any UDAs of the same type on the symbol will
    match. If the UDA is a template for a type, then any UDA which is an
    instantiation of that template will match. And if the UDA is a value,
    then any UDAs on the symbol which are equal to that value will match.

    See_Also:
        $(LREF hasUDA)
  +/
template getUDAs(alias symbol, alias attribute)
{
    import std.meta : Filter;

    alias getUDAs = Filter!(isDesiredUDA!attribute, __traits(getAttributes, symbol));
}

///
@safe unittest
{
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

private template isDesiredUDA(alias attribute)
{
    template isDesiredUDA(alias toCheck)
    {
        static if (is(typeof(attribute)) && !__traits(isTemplate, attribute))
        {
            static if (__traits(compiles, toCheck == attribute))
                enum isDesiredUDA = toCheck == attribute;
            else
                enum isDesiredUDA = false;
        }
        else static if (is(typeof(toCheck)))
        {
            static if (__traits(isTemplate, attribute))
                enum isDesiredUDA =  isInstanceOf!(attribute, typeof(toCheck));
            else
                enum isDesiredUDA = is(typeof(toCheck) == attribute);
        }
        else static if (__traits(isTemplate, attribute))
            enum isDesiredUDA = isInstanceOf!(attribute, toCheck);
        else
            enum isDesiredUDA = is(toCheck == attribute);
    }
}

/**
Params:
    symbol = The aggregate type or module to search
    attribute = The user-defined attribute to search for

Returns:
    All symbols within `symbol` that have the given UDA `attribute`.

Note:
    This is not recursive; it will not search for symbols within symbols such as
    nested structs or unions.
 */
template getSymbolsByUDA(alias symbol, alias attribute)
{
    alias membersWithUDA = getSymbolsByUDAImpl!(symbol, attribute, __traits(allMembers, symbol));

    // if the symbol itself has the UDA, tack it on to the front of the list
    static if (hasUDA!(symbol, attribute))
        alias getSymbolsByUDA = AliasSeq!(symbol, membersWithUDA);
    else
        alias getSymbolsByUDA = membersWithUDA;
}

///
@safe unittest
{
    enum Attr;
    struct A
    {
        @Attr int a;
        int b;
    }

    static assert(getSymbolsByUDA!(A, Attr).length == 1);
    static assert(hasUDA!(getSymbolsByUDA!(A, Attr)[0], Attr));
}

///
@safe unittest
{
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

/// Finds multiple attributes
@safe unittest
{
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

/// Checks for UDAs on the aggregate symbol itself
@safe unittest
{
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

/// Finds nothing if there is no member with specific UDA
@safe unittest
{
    static struct UDA { string name; }

    static struct D
    {
        int x;
    }

    static assert(getSymbolsByUDA!(D, UDA).length == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=18314
@safe unittest
{
    enum attr1;
    enum attr2;

    struct A
    {
        @attr1
        int n;
        // Removed due to https://issues.dlang.org/show_bug.cgi?id=16206
        //@attr1
        //void foo()(string){}
        @attr1
        void foo();
        @attr2
        void foo(int a);
    }

    static assert(getSymbolsByUDA!(A, attr1).length == 2);
    static assert(getSymbolsByUDA!(A, attr2).length == 1);
}

// getSymbolsByUDA fails if type has private members
// https://issues.dlang.org/show_bug.cgi?id=15335
@safe unittest
{
    // HasPrivateMembers has, well, private members, one of which has a UDA.
    import std.internal.test.uda : Attr, HasPrivateMembers;
    // Trying access to private member from another file therefore we do not have access
    // for this otherwise we get deprecation warning - not visible from module
    // This line is commented because `__traits(getMember)` should also consider
    // private members; this is not currently the case, but the PR that
    // fixes `__traits(getMember)` is blocked by this specific test.
    //static assert(getSymbolsByUDA!(HasPrivateMembers, Attr).length == 1);
    static assert(hasUDA!(getSymbolsByUDA!(HasPrivateMembers, Attr)[0], Attr));
}

// getSymbolsByUDA works with structs but fails with classes
// https://issues.dlang.org/show_bug.cgi?id=16387
@safe unittest
{
    enum Attr;
    class A
    {
        @Attr uint a;
    }

    alias res = getSymbolsByUDA!(A, Attr);
    static assert(res.length == 1);
    static assert(res[0].stringof == "a");
}

// getSymbolsByUDA fails on AliasSeq members
// https://issues.dlang.org/show_bug.cgi?id=18884
@safe unittest
{
    struct X
    {
        alias A = AliasSeq!(ulong, uint);
    }

    static assert(is(getSymbolsByUDA!(X, X) == AliasSeq!()));
}

// https://issues.dlang.org/show_bug.cgi?id=23776
@safe pure nothrow @nogc unittest
{
    struct T
    {
        struct Tag {}
        @Tag struct MyStructA {}
        @Tag struct MyStructB {}
        @Tag struct MyStructC {}
    }
    alias tcomponents = getSymbolsByUDA!(T, T.Tag);
    static assert(tcomponents.length > 0);

    struct X
    {
        struct Tag {}
        @Tag enum MyEnumA;
        @Tag enum MyEnumB;
        @Tag enum MyEnumC;
    }
    alias xcomponents = getSymbolsByUDA!(X, X.Tag);
    static assert(xcomponents.length > 0);
}

// getSymbolsByUDA produces wrong result if one of the symbols having the UDA is a function
// https://issues.dlang.org/show_bug.cgi?id=18624
@safe unittest
{
    enum Attr;
    struct A
    {
        @Attr void a();
        @Attr void a(int n);
              void b();
        @Attr void c();
    }

    alias ola = __traits(getOverloads, A, "a");
    static assert(__traits(isSame, getSymbolsByUDA!(A, Attr),
        AliasSeq!(ola[0], ola[1], A.c)));
}

// getSymbolsByUDA no longer works on modules
// https://issues.dlang.org/show_bug.cgi?id=20054
version (StdUnittest)
{
    @("Issue20054")
    void issue20054() {}
    static assert(__traits(compiles, getSymbolsByUDA!(mixin(__MODULE__), "Issue20054")));
}

private template isAliasSeq(Args...)
{
    static if (Args.length != 1)
        enum isAliasSeq = true;
    else
        enum isAliasSeq = false;
}

private template getSymbolsByUDAImpl(alias symbol, alias attribute, names...)
{
    import std.meta : Alias, AliasSeq, Filter;
    static if (names.length == 0)
    {
        alias getSymbolsByUDAImpl = AliasSeq!();
    }
    else
    {
        alias tail = getSymbolsByUDAImpl!(symbol, attribute, names[1 .. $]);

        // Filtering inaccessible members.
        static if (!__traits(compiles, __traits(getMember, symbol, names[0])))
        {
            alias getSymbolsByUDAImpl = tail;
        }
        else
        {
            alias member = __traits(getMember, symbol, names[0]);

            // Filtering not compiled members such as alias of basic types.
            static if (isAliasSeq!member ||
                       (isType!member && !isAggregateType!member && !is(member == enum)))
            {
                alias getSymbolsByUDAImpl = tail;
            }
            // If a symbol is overloaded, get UDAs for each overload (including templated overlaods).
            else static if (__traits(getOverloads, symbol, names[0], true).length > 0)
            {
                enum hasSpecificUDA(alias member) = hasUDA!(member, attribute);
                alias overloadsWithUDA = Filter!(hasSpecificUDA, __traits(getOverloads, symbol, names[0]));
                alias getSymbolsByUDAImpl = AliasSeq!(overloadsWithUDA, tail);
            }
            else static if (hasUDA!(member, attribute))
            {
                alias getSymbolsByUDAImpl = AliasSeq!(member, tail);
            }
            else
            {
                alias getSymbolsByUDAImpl = tail;
            }
        }
    }
}

/**
   Returns: `true` iff all types `Ts` are the same.
*/
enum bool allSameType(Ts...) =
{
    static foreach (T; Ts[Ts.length > 1 .. $])
        static if (!is(Ts[0] == T))
            if (__ctfe)  // Dodge the "statement is unreachable" warning
                return false;
    return true;
}();

///
@safe unittest
{
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

/**
   Returns: `true` iff the type `T` can be tested in an $(D
   if)-expression, that is if $(D if (pred(T.init)) {}) is compilable.
*/
enum ifTestable(T, alias pred = a => a) = __traits(compiles, { if (pred(T.init)) {} });

///
@safe unittest
{
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
    import std.meta : AliasSeq, allSatisfy;
    static assert(allSatisfy!(ifTestable, AliasSeq!(bool, int, float, double, string)));
    struct BoolWrapper { bool value; }
    static assert(!ifTestable!(bool, a => BoolWrapper(a)));
}

/**
 * Detect whether `X` is a type. Analogous to `is(X)`. This is useful when used
 * in conjunction with other templates, e.g. `allSatisfy!(isType, X)`.
 *
 * Returns:
 *      `true` if `X` is a type, `false` otherwise
 */
enum isType(alias X) = is(X);

///
@safe unittest
{
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

/**
 * Detect whether symbol or type `X` is a function. This is different that finding
 * if a symbol is callable or satisfying `is(X == function)`, it finds
 * specifically if the symbol represents a normal function declaration, i.e.
 * not a delegate or a function pointer.
 *
 * Returns:
 *     `true` if `X` is a function, `false` otherwise
 *
 * See_Also:
 *     Use $(LREF isFunctionPointer) or $(LREF isDelegate) for detecting those types
 *     respectively.
 */
template isFunction(alias X)
{
    static if (is(typeof(&X) U : U*) && is(U == function) ||
               is(typeof(&X) U == delegate))
    {
        // x is a (nested) function symbol.
        enum isFunction = true;
    }
    else static if (is(X T))
    {
        // x is a type.  Take the type of it and examine.
        enum isFunction = is(T == function);
    }
    else
        enum isFunction = false;
}

///
@safe unittest
{
    static void func(){}
    static assert(isFunction!func);

    struct S
    {
        void func(){}
    }
    static assert(isFunction!(S.func));
}

/**
 * Detect whether `X` is a final method or class.
 *
 * Returns:
 *     `true` if `X` is final, `false` otherwise
 */
template isFinal(alias X)
{
    static if (is(X == class))
        enum isFinal = __traits(isFinalClass, X);
    else static if (isFunction!X)
        enum isFinal = __traits(isFinalFunction, X);
    else
        enum isFinal = false;
}

///
@safe unittest
{
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

/++
 + Determines whether the type `S` can be copied.
 + If a type cannot be copied, then code such as `MyStruct x; auto y = x;` will fail to compile.
 + Copying for structs can be disabled by using `@disable this(this)`.
 +
 + See also: $(DDSUBLINK spec/traits, isCopyable, `__traits(isCopyable, S)`)
 + Params:
 +  S = The type to check.
 +
 + Returns:
 +  `true` if `S` can be copied. `false` otherwise.
 +/
enum isCopyable(S) = __traits(isCopyable, S);

///
@safe unittest
{
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

/**
 * The parameter type deduced by IFTI when an expression of type T is passed as
 * an argument to a template function.
 *
 * For all types other than pointer and slice types, `DeducedParameterType!T`
 * is the same as `T`. For pointer and slice types, it is `T` with the
 * outer-most layer of qualifiers dropped.
 */
package(std) template DeducedParameterType(T)
{
    static if (is(T == U*, U) || is(T == U[], U))
        alias DeducedParameterType = Unqual!T;
    else
        alias DeducedParameterType = T;
}

@safe unittest
{
    static assert(is(DeducedParameterType!(const(int)) == const(int)));
    static assert(is(DeducedParameterType!(const(int[2])) == const(int[2])));

    static assert(is(DeducedParameterType!(const(int*)) == const(int)*));
    static assert(is(DeducedParameterType!(const(int[])) == const(int)[]));
}

@safe unittest
{
    static struct NoCopy
    {
        @disable this(this);
    }

    static assert(is(DeducedParameterType!NoCopy == NoCopy));
}

@safe unittest
{
    static assert(is(DeducedParameterType!(inout(int[])) == inout(int)[]));
}

private auto dip1000Test(int x) {return *&x;}
// We don't use isSafe, because betterC client code needs to instantiate
// core.internal.array.comparison.__cmp in the client side. isSafe uses
// __cmp of two strings, so using it would instantate that here instead. That
// won't do because betterC compilations do not link the Phobos binary in.
package(std) enum dip1000Enabled
    = is(typeof(&dip1000Test) : int function(int) @safe);

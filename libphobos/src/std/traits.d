// Written in the D programming language.

/**
 * Templates which extract information about types and symbols at compile time.
 *
 * $(SCRIPT inhibitQuickIndex = 1;)
 *
 * $(DIVC quickindex,
 * $(BOOKTABLE ,
 * $(TR $(TH Category) $(TH Templates))
 * $(TR $(TD Symbol Name _traits) $(TD
 *           $(LREF fullyQualifiedName)
 *           $(LREF moduleName)
 *           $(LREF packageName)
 * ))
 * $(TR $(TD Function _traits) $(TD
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
 * $(TR $(TD Aggregate Type _traits) $(TD
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
 *           $(LREF ImplicitConversionTargets)
 *           $(LREF CopyTypeQualifiers)
 *           $(LREF CopyConstness)
 *           $(LREF isAssignable)
 *           $(LREF isCovariantWith)
 *           $(LREF isImplicitlyConvertible)
 * ))
 * $(TR $(TD SomethingTypeOf) $(TD
 *           $(LREF rvalueOf)
 *           $(LREF lvalueOf)
 *           $(LREF InoutOf)
 *           $(LREF ConstOf)
 *           $(LREF SharedOf)
 *           $(LREF SharedInoutOf)
 *           $(LREF SharedConstOf)
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
 *           $(LREF Unqual)
 *           $(LREF Unsigned)
 *           $(LREF ValueType)
 *           $(LREF Promoted)
 * ))
 * $(TR $(TD Misc) $(TD
 *           $(LREF mangledName)
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
 * Copyright: Copyright Digital Mars 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright),
 *            Tomasz Stachowiak ($(D isExpressions)),
 *            $(HTTP erdani.org, Andrei Alexandrescu),
 *            Shin Fujishiro,
 *            $(HTTP octarineparrot.com, Robert Clipsham),
 *            $(HTTP klickverbot.at, David Nadlinger),
 *            Kenji Hara,
 *            Shoichi Kato
 * Source:    $(PHOBOSSRC std/_traits.d)
 */
/*          Copyright Digital Mars 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.traits;

import std.meta : AliasSeq, allSatisfy;
import std.functional : unaryFun;

// Legacy inheritance from std.typetuple
// See also: https://github.com/dlang/phobos/pull/5484#discussion_r122602797
import std.meta : staticMapMeta = staticMap;
// TODO: find a way to trigger deprecation warnings
//deprecated("staticMap is part of std.meta: Please import std.meta")
alias staticMap = staticMapMeta;

///////////////////////////////////////////////////////////////////////////////
// Functions
///////////////////////////////////////////////////////////////////////////////

// Petit demangler
// (this or similar thing will eventually go to std.demangle if necessary
//  ctfe stuffs are available)
private
{
    struct Demangle(T)
    {
        T       value;  // extracted information
        string  rest;
    }

    /* Demangles mstr as the storage class part of Argument. */
    Demangle!uint demangleParameterStorageClass(string mstr)
    {
        uint pstc = 0; // parameter storage class

        // Argument --> Argument2 | M Argument2
        if (mstr.length > 0 && mstr[0] == 'M')
        {
            pstc |= ParameterStorageClass.scope_;
            mstr  = mstr[1 .. $];
        }

        // Argument2 --> Type | J Type | K Type | L Type
        ParameterStorageClass stc2;

        switch (mstr.length ? mstr[0] : char.init)
        {
            case 'J': stc2 = ParameterStorageClass.out_;  break;
            case 'K': stc2 = ParameterStorageClass.ref_;  break;
            case 'L': stc2 = ParameterStorageClass.lazy_; break;
            case 'N': if (mstr.length >= 2 && mstr[1] == 'k')
                        stc2 = ParameterStorageClass.return_;
                      break;
            default : break;
        }
        if (stc2 != ParameterStorageClass.init)
        {
            pstc |= stc2;
            mstr  = mstr[1 .. $];
            if (stc2 & ParameterStorageClass.return_)
                mstr  = mstr[1 .. $];
        }

        return Demangle!uint(pstc, mstr);
    }

    /* Demangles mstr as FuncAttrs. */
    Demangle!uint demangleFunctionAttributes(string mstr)
    {
        immutable LOOKUP_ATTRIBUTE =
        [
            'a': FunctionAttribute.pure_,
            'b': FunctionAttribute.nothrow_,
            'c': FunctionAttribute.ref_,
            'd': FunctionAttribute.property,
            'e': FunctionAttribute.trusted,
            'f': FunctionAttribute.safe,
            'i': FunctionAttribute.nogc,
            'j': FunctionAttribute.return_,
            'l': FunctionAttribute.scope_
        ];
        uint atts = 0;

        // FuncAttrs --> FuncAttr | FuncAttr FuncAttrs
        // FuncAttr  --> empty | Na | Nb | Nc | Nd | Ne | Nf | Ni | Nj
        // except 'Ng' == inout, because it is a qualifier of function type
        while (mstr.length >= 2 && mstr[0] == 'N' && mstr[1] != 'g' && mstr[1] != 'k')
        {
            if (FunctionAttribute att = LOOKUP_ATTRIBUTE[ mstr[1] ])
            {
                atts |= att;
                mstr  = mstr[2 .. $];
            }
            else assert(0);
        }
        return Demangle!uint(atts, mstr);
    }

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

package
{
    // Add the mutable qualifier to the given type T.
    template MutableOf(T)     { alias MutableOf     =              T  ; }
}

/// Add the inout qualifier to the given type T.
template InoutOf(T)       { alias InoutOf       =        inout(T) ; }
/// Add the const qualifier to the given type T.
template ConstOf(T)       { alias ConstOf       =        const(T) ; }
/// Add the shared qualifier to the given type T.
template SharedOf(T)      { alias SharedOf      =       shared(T) ; }
/// Add the shared and inout qualifiers to the given type T.
template SharedInoutOf(T) { alias SharedInoutOf = shared(inout(T)); }
/// Add the shared and const qualifiers to the given type T.
template SharedConstOf(T) { alias SharedConstOf = shared(const(T)); }
/// Add the immutable qualifier to the given type T.
template ImmutableOf(T)   { alias ImmutableOf   =    immutable(T) ; }

@safe unittest
{
    static assert(is(    MutableOf!int ==              int));
    static assert(is(      InoutOf!int ==        inout int));
    static assert(is(      ConstOf!int ==        const int));
    static assert(is(     SharedOf!int == shared       int));
    static assert(is(SharedInoutOf!int == shared inout int));
    static assert(is(SharedConstOf!int == shared const int));
    static assert(is(  ImmutableOf!int ==    immutable int));
}

/// Get qualifier template from the given type T
template QualifierOf(T)
{
         static if (is(T == shared(const U), U)) alias QualifierOf = SharedConstOf;
    else static if (is(T ==        const U , U)) alias QualifierOf = ConstOf;
    else static if (is(T == shared(inout U), U)) alias QualifierOf = SharedInoutOf;
    else static if (is(T ==        inout U , U)) alias QualifierOf = InoutOf;
    else static if (is(T ==    immutable U , U)) alias QualifierOf = ImmutableOf;
    else static if (is(T ==       shared U , U)) alias QualifierOf = SharedOf;
    else                                         alias QualifierOf = MutableOf;
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

version (unittest)
{
    alias TypeQualifierList = AliasSeq!(MutableOf, ConstOf, SharedOf, SharedConstOf, ImmutableOf);

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

    static if (__traits(compiles, parentOf!T))
        enum parent = packageName!(parentOf!T);
    else
        enum string parent = null;

    static if (T.stringof.startsWith("package "))
        enum packageName = (parent.length ? parent ~ '.' : "") ~ T.stringof[8 .. $];
    else static if (parent)
        enum packageName = parent;
    else
        static assert(false, T.stringof ~ " has no parent");
}

///
@safe unittest
{
    import std.traits;
    static assert(packageName!packageName == "std");
}

@safe unittest
{
    import std.array;

    // Commented out because of dmd @@@BUG8922@@@
    // static assert(packageName!std == "std");  // this package (currently: "std.std")
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

version (none) version (unittest) //Please uncomment me when changing packageName to test global imports
{
    import core.sync.barrier;  // global import
    static assert(packageName!core == "core");
    static assert(packageName!(core.sync) == "core.sync");
    static assert(packageName!Barrier == "core.sync");
}

/**
 * Get the module name (including package) for the given symbol.
 */
template moduleName(alias T)
{
    import std.algorithm.searching : startsWith;

    static assert(!T.stringof.startsWith("package "), "cannot get the module name for a package");

    static if (T.stringof.startsWith("module "))
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
    import std.traits;
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

version (none) version (unittest) //Please uncomment me when changing moduleName to test global imports
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
template fullyQualifiedName(T...)
    if (T.length == 1)
{

    static if (is(T))
        enum fullyQualifiedName = fqnType!(T[0], false, false, false, false);
    else
        enum fullyQualifiedName = fqnSym!(T[0]);
}

///
@safe unittest
{
    static assert(fullyQualifiedName!fullyQualifiedName == "std.traits.fullyQualifiedName");
}

version (unittest)
{
    // Used for both fqnType and fqnSym unittests
    private struct QualifiedNameTests
    {
        struct Inner
        {
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
    import std.format : format;

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
        alias PSC = ParameterStorageClass;

        return format("%s%s%s%s%s",
            psc & PSC.scope_ ? "scope " : "",
            psc & PSC.return_ ? "return " : "",
            psc & PSC.out_ ? "out " : "",
            psc & PSC.ref_ ? "ref " : "",
            psc & PSC.lazy_ ? "lazy " : ""
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
                map!(a => format("%s%s", a[0], a[1]))(
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
            return format("extern(%s) ", linkage);
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
            return format("%s%s%s%s%s%s%s%s",
                 attrs & FA.pure_ ? " pure" : "",
                 attrs & FA.nothrow_ ? " nothrow" : "",
                 attrs & FA.ref_ ? " ref" : "",
                 attrs & FA.property ? " @property" : "",
                 attrs & FA.trusted ? " @trusted" : "",
                 attrs & FA.safe ? " @safe" : "",
                 attrs & FA.nogc ? " @nogc" : "",
                 attrs & FA.return_ ? " return" : ""
            );
    }

    string addQualifiers(string typeString,
        bool addConst, bool addImmutable, bool addShared, bool addInout)
    {
        auto result = typeString;
        if (addShared)
        {
            result = format("shared(%s)", result);
        }
        if (addConst || addImmutable || addInout)
        {
            result = format("%s(%s)",
                addConst ? "const" :
                    addImmutable ? "immutable" : "inout",
                result
            );
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
        enum fqnType = chain!(
            format("%s[%s]", fqnType!(typeof(T.init[0]), qualifiers), T.length)
        );
    }
    else static if (isArray!T)
    {
        enum fqnType = chain!(
            format("%s[]", fqnType!(typeof(T.init[0]), qualifiers))
        );
    }
    else static if (isAssociativeArray!T)
    {
        enum fqnType = chain!(
            format("%s[%s]", fqnType!(ValueType!T, qualifiers), fqnType!(KeyType!T, noQualifiers))
        );
    }
    else static if (isSomeFunction!T)
    {
        static if (is(T F == delegate))
        {
            enum qualifierString = format("%s%s",
                is(F == shared) ? " shared" : "",
                is(F == inout) ? " inout" :
                is(F == immutable) ? " immutable" :
                is(F == const) ? " const" : ""
            );
            enum formatStr = "%s%s delegate(%s)%s%s";
            enum fqnType = chain!(
                format(formatStr, linkageString!T, fqnType!(ReturnType!T, noQualifiers),
                    parametersTypeString!(T), functionAttributeString!T, qualifierString)
            );
        }
        else
        {
            static if (isFunctionPointer!T)
                enum formatStr = "%s%s function(%s)%s";
            else
                enum formatStr = "%s%s(%s)%s";

            enum fqnType = chain!(
                format(formatStr, linkageString!T, fqnType!(ReturnType!T, noQualifiers),
                    parametersTypeString!(T), functionAttributeString!T)
            );
        }
    }
    else static if (isPointer!T)
    {
        enum fqnType = chain!(
            format("%s*", fqnType!(PointerTarget!T, qualifiers))
        );
    }
    else static if (is(T : __vector(V[N]), V, size_t N))
    {
        enum fqnType = chain!(
            format("__vector(%s[%s])", fqnType!(V, qualifiers), N)
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
 * or a class with an $(D opCall). Please note that $(D_KEYWORD ref)
 * is not part of a type, but the attribute of the function
 * (see template $(LREF functionAttributes)).
 */
template ReturnType(func...)
    if (func.length == 1 && isCallable!func)
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
to function, a delegate, a struct with an $(D opCall), a pointer to a
struct with an $(D opCall), or a class with an $(D opCall).
*/
template Parameters(func...)
    if (func.length == 1 && isCallable!func)
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
Returns the number of arguments of function $(D func).
arity is undefined for variadic functions.
*/
template arity(alias func)
    if ( isCallable!func && variadicFunctionStyle!func == Variadic.no )
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
    none    = 0,
    scope_  = 1,    /// ditto
    out_    = 2,    /// ditto
    ref_    = 4,    /// ditto
    lazy_   = 8,    /// ditto
    return_ = 0x10, /// ditto
}

/// ditto
template ParameterStorageClassTuple(func...)
    if (func.length == 1 && isCallable!func)
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

    void func(ref int ctx, out real result, real param)
    {
    }
    alias pstc = ParameterStorageClassTuple!func;
    static assert(pstc.length == 3); // three parameters
    static assert(pstc[0] == STC.ref_);
    static assert(pstc[1] == STC.out_);
    static assert(pstc[2] == STC.none);
}

/*****************
 * Convert string tuple Attribs to ParameterStorageClass bits
 * Params:
 *      Attribs = string tuple
 * Returns:
 *      ParameterStorageClass bits
 */
template extractParameterStorageClassFlags(Attribs...)
{
    enum ParameterStorageClass extractParameterStorageClassFlags = ()
    {
        auto result = ParameterStorageClass.none;
        static if (Attribs.length > 0)
        {
            foreach (attrib; [Attribs])
            {
                final switch (attrib) with (ParameterStorageClass)
                {
                    case "scope":  result |= scope_;  break;
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

    // Bugzilla 9317
    static inout(int) func(inout int param) { return param; }
    static assert(ParameterStorageClassTuple!(typeof(func))[0] == STC.none);
}

@safe unittest
{
    // Bugzilla 14253
    static struct Foo {
        ref Foo opAssign(ref Foo rhs) return { return this; }
    }

    alias tup = ParameterStorageClassTuple!(__traits(getOverloads, Foo, "opAssign")[0]);
}


/**
Get, as a tuple, the identifiers of the parameters to a function symbol.
 */
template ParameterIdentifierTuple(func...)
    if (func.length == 1 && isCallable!func)
{
    static if (is(FunctionTypeOf!func PT == __parameters))
    {
        template Get(size_t i)
        {
            static if (!isFunctionPointer!func && !isDelegate!func
                       // Unnamed parameters yield CT error.
                       && is(typeof(__traits(identifier, PT[i .. i+1]))))
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
        static assert(0, func[0].stringof ~ "is not a function");

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
If a parameter doesn't have the default value, $(D void) is returned instead.
 */
template ParameterDefaults(func...)
    if (func.length == 1 && isCallable!func)
{
    alias param_names = ParameterIdentifierTuple!func;
    static if (is(FunctionTypeOf!(func[0]) PT == __parameters))
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
                // workaround scope escape check, see
                // https://issues.dlang.org/show_bug.cgi?id=16582
                // should use return scope once available
                enum get = (PT[i .. i+1] " ~ args ~ ") @trusted
                {
                    // If the parameter is lazy, we force it to be evaluated
                    // like this.
                    auto " ~ val ~ " = " ~ args ~ "[0];
                    auto " ~ ptr ~ " = &" ~ val ~ ";
                        // workaround Bugzilla 16582
                    return *" ~ ptr ~ ";
                };
            ");
            static if (is(typeof(get())))
                enum Get = get();
            else
                alias Get = void;
                // If default arg doesn't exist, returns void instead.
        }
    }
    else
    {
        static assert(0, func[0].stringof ~ "is not a function");

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

@safe unittest // issue 17192
{
    static void func(int i, int PT, int __pd_value, int __pd_val, int __args,
        int name, int args, int val, int ptr, int args_, int val_, int ptr_)
    {
    }
    alias Voids = ParameterDefaults!func;
    static assert(Voids.length == 12);
    foreach (V; Voids) static assert(is(V == void));
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

    // bug 10800 - property functions return empty string
    @property void foo(int x = 3) { }
    static assert(PDVT!foo.length == 1);
    static assert(PDVT!foo[0] == 3);
    static assert(is(typeof(PDVT!foo) == typeof(AliasSeq!(3))));

    struct Colour
    {
        ubyte a,r,g,b;

        static immutable Colour white = Colour(255,255,255,255);
    }
    void bug8106(Colour c = Colour.white) {}
    //pragma(msg, PDVT!bug8106);
    static assert(PDVT!bug8106[0] == Colour.white);
    void bug16582(scope int* val = null) {}
    static assert(PDVT!bug16582[0] is null);
}


/**
Returns the FunctionAttribute mask for function $(D func).

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
}

/// ditto
template functionAttributes(func...)
    if (func.length == 1 && isCallable!func)
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
    import std.traits : functionAttributes, FunctionAttribute;

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

    foreach (attrib; Attribs)
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
        foreach (attribute; args[1 .. $])
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
$(D true) if $(D func) is $(D @safe) or $(D @trusted).
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
    @safe static safeFunc() {}
    @trusted static trustedFunc() {}
    @system static systemFunc() {}

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
$(D true) if $(D func) is $(D @system).
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
    @safe static safeFunc() {}
    @trusted static trustedFunc() {}
    @system static systemFunc() {}

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
    one of the strings "D", "C", "Windows", or "Objective-C"
*/
template functionLinkage(func...)
    if (func.length == 1 && isCallable!func)
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
    no,       /// Function is not variadic.
    c,        /// Function is a _C-style variadic function, which uses
              /// core.stdc.stdarg
              /// Function is a _D-style variadic function, which uses
    d,        /// __argptr and __arguments.
    typesafe, /// Function is a typesafe variadic function.
}

/// ditto
template variadicFunctionStyle(func...)
    if (func.length == 1 && isCallable!func)
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

    extern(C) int printf(in char*, ...);
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
Get the function type from a callable object $(D func).

Using builtin $(D typeof) on a property function yields the types of the
property value, not of the property function itself.  Still,
$(D FunctionTypeOf) is able to obtain function types of properties.

Note:
Do not confuse function types with function pointer types; function types are
usually used for compile-time reflection purposes.
 */
template FunctionTypeOf(func...)
    if (func.length == 1 && isCallable!func)
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

    interface Overloads
    {
        void test(string);
        real test(real);
        int  test(int);
        int  test() @property;
    }
    alias ov = AliasSeq!(__traits(getVirtualFunctions, Overloads, "test"));
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
}

version (unittest)
{
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
    foreach (BaseT; AliasSeq!(typeof(&sc), typeof(&novar), typeof(&cstyle),
        typeof(&dstyle), typeof(&typesafe)))
    {
        foreach (T; AliasSeq!(BaseT, FunctionTypeOf!BaseT))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            enum linkage = functionLinkage!T;
            enum attrs = functionAttributes!T;

            static assert(is(SetFunctionAttributes!(T, linkage, attrs) == T),
                "Identity check failed for: " ~ T.stringof);

            // Check that all linkage types work (D-style variadics require D linkage).
            static if (variadicFunctionStyle!T != Variadic.d)
            {
                foreach (newLinkage; AliasSeq!("D", "C", "Windows", "C++"))
                {
                    alias New = SetFunctionAttributes!(T, newLinkage, attrs);
                    static assert(functionLinkage!New == newLinkage,
                        "Linkage test failed for: " ~ T.stringof ~ ", " ~ newLinkage ~
                        " (got " ~ New.stringof ~ ")");
                }
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
        }();
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
    import std.meta : staticIndexOf;

    static if (is(typeof(T.outer)))
        enum isInnerClass = __traits(isSame, typeof(T.outer), __traits(parent, T))
                         && (staticIndexOf!(__traits(allMembers, T), "outer") == -1);
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
Determines whether $(D T) has its own context pointer.
$(D T) must be either $(D class), $(D struct), or $(D union).
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
Determines whether $(D T) or any of its representation types
have a context pointer.
*/
template hasNested(T)
{
    import std.meta : anySatisfy, Filter;

    static if (isStaticArray!T && T.length)
        enum hasNested = hasNested!(typeof(T.init[0]));
    else static if (is(T == class) || is(T == struct) || is(T == union))
    {
        // prevent infinite recursion for class with member of same type
        enum notSame(U) = !is(Unqual!T == Unqual!U);
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
 * If $(D T) isn't a struct, class, or union returns a tuple
 * with one element $(D T).
 */
template Fields(T)
{
    static if (is(T == struct) || is(T == union))
        alias Fields = typeof(T.tupleof[0 .. $ - isNested!T]);
    else static if (is(T == class))
        alias Fields = typeof(T.tupleof);
    else
        alias Fields = AliasSeq!T;
}

///
@safe unittest
{
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
}


//Required for FieldNameTuple
private enum NameOf(alias T) = T.stringof;

/**
 * Get as an expression tuple the names of the fields of a struct, class, or
 * union. This consists of the fields that take up memory space, excluding the
 * hidden fields like the virtual function table pointer or a context pointer
 * for nested types. If $(D T) isn't a struct, class, or union returns an
 * expression tuple with an empty string.
 */
template FieldNameTuple(T)
{
    import std.meta : staticMap;
    static if (is(T == struct) || is(T == union))
        alias FieldNameTuple = staticMap!(NameOf, T.tupleof[0 .. $ - isNested!T]);
    else static if (is(T == class))
        alias FieldNameTuple = staticMap!(NameOf, T.tupleof);
    else
        alias FieldNameTuple = AliasSeq!"";
}

///
@safe unittest
{
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

    int i;

    struct NestedStruct1 { void f() { ++i; } }
    static assert(is(FieldNameTuple!NestedStruct1 == AliasSeq!()));

    struct NestedStruct2 { int a; void f() { ++i; } }
    static assert(FieldNameTuple!NestedStruct2 == AliasSeq!"a");

    class NestedClass { int a; void f() { ++i; } }
    static assert(FieldNameTuple!NestedClass == AliasSeq!"a");
}


/***
Get the primitive types of the fields of a struct or class, in
topological order.
*/
template RepresentationTypeTuple(T)
{
    template Impl(T...)
    {
        static if (T.length == 0)
        {
            alias Impl = AliasSeq!();
        }
        else
        {
            import std.typecons : Rebindable;

            static if (is(T[0] R: Rebindable!R))
            {
                alias Impl = Impl!(Impl!R, T[1 .. $]);
            }
            else  static if (is(T[0] == struct) || is(T[0] == union))
            {
                // @@@BUG@@@ this should work
                //alias .RepresentationTypes!(T[0].tupleof)
                //    RepresentationTypes;
                alias Impl = Impl!(FieldTypeTuple!(T[0]), T[1 .. $]);
            }
            else
            {
                alias Impl = AliasSeq!(T[0], Impl!(T[1 .. $]));
            }
        }
    }

    static if (is(T == struct) || is(T == union) || is(T == class))
    {
        alias RepresentationTypeTuple = Impl!(FieldTypeTuple!T);
    }
    else
    {
        alias RepresentationTypeTuple = Impl!T;
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

    /* Issue 6642 */
    import std.typecons : Rebindable;

    struct S5 { int a; Rebindable!(immutable Object) b; }
    alias R2 = RepresentationTypeTuple!S5;
    static assert(R2.length == 2 && is(R2[0] == int) && is(R2[1] == immutable(Object)));
}

/*
Statically evaluates to $(D true) if and only if $(D T)'s
representation contains at least one field of pointer or array type.
Members of class types are not considered raw pointers. Pointers to
immutable objects are not considered raw aliasing.
*/
private template hasRawAliasing(T...)
{
    template Impl(T...)
    {
        static if (T.length == 0)
        {
            enum Impl = false;
        }
        else
        {
            static if (is(T[0] foo : U*, U) && !isFunctionPointer!(T[0]))
                enum has = !is(U == immutable);
            else static if (is(T[0] foo : U[], U) && !isStaticArray!(T[0]))
                enum has = !is(U == immutable);
            else static if (isAssociativeArray!(T[0]))
                enum has = !is(T[0] == immutable);
            else
                enum has = false;

            enum Impl = has || Impl!(T[1 .. $]);
        }
    }

    enum hasRawAliasing = Impl!(RepresentationTypeTuple!T);
}

///
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

/*
Statically evaluates to $(D true) if and only if $(D T)'s
representation contains at least one non-shared field of pointer or
array type.  Members of class types are not considered raw pointers.
Pointers to immutable objects are not considered raw aliasing.
*/
private template hasRawUnsharedAliasing(T...)
{
    template Impl(T...)
    {
        static if (T.length == 0)
        {
            enum Impl = false;
        }
        else
        {
            static if (is(T[0] foo : U*, U) && !isFunctionPointer!(T[0]))
                enum has = !is(U == immutable) && !is(U == shared);
            else static if (is(T[0] foo : U[], U) && !isStaticArray!(T[0]))
                enum has = !is(U == immutable) && !is(U == shared);
            else static if (isAssociativeArray!(T[0]))
                enum has = !is(T[0] == immutable) && !is(T[0] == shared);
            else
                enum has = false;

            enum Impl = has || Impl!(T[1 .. $]);
        }
    }

    enum hasRawUnsharedAliasing = Impl!(RepresentationTypeTuple!T);
}

///
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

/*
Statically evaluates to $(D true) if and only if $(D T)'s
representation includes at least one non-immutable object reference.
*/

private template hasObjects(T...)
{
    static if (T.length == 0)
    {
        enum hasObjects = false;
    }
    else static if (is(T[0] == struct))
    {
        enum hasObjects = hasObjects!(
            RepresentationTypeTuple!(T[0]), T[1 .. $]);
    }
    else
    {
        enum hasObjects = ((is(T[0] == class) || is(T[0] == interface))
            && !is(T[0] == immutable)) || hasObjects!(T[1 .. $]);
    }
}

/*
Statically evaluates to $(D true) if and only if $(D T)'s
representation includes at least one non-immutable non-shared object
reference.
*/
private template hasUnsharedObjects(T...)
{
    static if (T.length == 0)
    {
        enum hasUnsharedObjects = false;
    }
    else static if (is(T[0] == struct))
    {
        enum hasUnsharedObjects = hasUnsharedObjects!(
            RepresentationTypeTuple!(T[0]), T[1 .. $]);
    }
    else
    {
        enum hasUnsharedObjects = ((is(T[0] == class) || is(T[0] == interface)) &&
                                !is(T[0] == immutable) && !is(T[0] == shared)) ||
            hasUnsharedObjects!(T[1 .. $]);
    }
}

/**
Returns $(D true) if and only if $(D T)'s representation includes at
least one of the following: $(OL $(LI a raw pointer $(D U*) and $(D U)
is not immutable;) $(LI an array $(D U[]) and $(D U) is not
immutable;) $(LI a reference to a class or interface type $(D C) and $(D C) is
not immutable.) $(LI an associative array that is not immutable.)
$(LI a delegate.))
*/
template hasAliasing(T...)
{
    import std.meta : anySatisfy;
    import std.typecons : Rebindable;

    static if (T.length && is(T[0] : Rebindable!R, R))
    {
        enum hasAliasing = hasAliasing!(R, T[1 .. $]);
    }
    else
    {
        template isAliasingDelegate(T)
        {
            enum isAliasingDelegate = isDelegate!T
                                  && !is(T == immutable)
                                  && !is(FunctionTypeOf!T == immutable);
        }
        enum hasAliasing = hasRawAliasing!T || hasObjects!T ||
            anySatisfy!(isAliasingDelegate, T, RepresentationTypeTuple!T);
    }
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
}
/**
Returns $(D true) if and only if $(D T)'s representation includes at
least one of the following: $(OL $(LI a raw pointer $(D U*);) $(LI an
array $(D U[]);) $(LI a reference to a class type $(D C).)
$(LI an associative array.) $(LI a delegate.))
 */
template hasIndirections(T)
{
    import std.meta : anySatisfy;
    static if (is(T == struct) || is(T == union))
        enum hasIndirections = anySatisfy!(.hasIndirections, FieldTypeTuple!T);
    else static if (isStaticArray!T && is(T : E[N], E, size_t N))
        enum hasIndirections = is(E == void) ? true : hasIndirections!E;
    else static if (isFunctionPointer!T)
        enum hasIndirections = false;
    else
        enum hasIndirections = isPointer!T || isDelegate!T || isDynamicArray!T ||
            isAssociativeArray!T || is (T == class) || is(T == interface);
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
}

@safe unittest //12000
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
Returns $(D true) if and only if $(D T)'s representation includes at
least one of the following: $(OL $(LI a raw pointer $(D U*) and $(D U)
is not immutable or shared;) $(LI an array $(D U[]) and $(D U) is not
immutable or shared;) $(LI a reference to a class type $(D C) and
$(D C) is not immutable or shared.) $(LI an associative array that is not
immutable or shared.) $(LI a delegate that is not shared.))
*/

template hasUnsharedAliasing(T...)
{
    import std.meta : anySatisfy;
    import std.typecons : Rebindable;

    static if (!T.length)
    {
        enum hasUnsharedAliasing = false;
    }
    else static if (is(T[0] R: Rebindable!R))
    {
        enum hasUnsharedAliasing = hasUnsharedAliasing!R;
    }
    else
    {
        template unsharedDelegate(T)
        {
            enum bool unsharedDelegate = isDelegate!T
                                     && !is(T == shared)
                                     && !is(T == shared)
                                     && !is(T == immutable)
                                     && !is(FunctionTypeOf!T == shared)
                                     && !is(FunctionTypeOf!T == immutable);
        }

        enum hasUnsharedAliasing =
            hasRawUnsharedAliasing!(T[0]) ||
            anySatisfy!(unsharedDelegate, RepresentationTypeTuple!(T[0])) ||
            hasUnsharedObjects!(T[0]) ||
            hasUnsharedAliasing!(T[1..$]);
    }
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
    /* Issue 6642 */
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

    /* Issue 6979 */
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
}

/**
 True if $(D S) or any type embedded directly in the representation of $(D S)
 defines an elaborate copy constructor. Elaborate copy constructors are
 introduced by defining $(D this(this)) for a $(D struct).

 Classes and unions never have elaborate copy constructors.
 */
template hasElaborateCopyConstructor(S)
{
    import std.meta : anySatisfy;
    static if (isStaticArray!S && S.length)
    {
        enum bool hasElaborateCopyConstructor = hasElaborateCopyConstructor!(typeof(S.init[0]));
    }
    else static if (is(S == struct))
    {
        enum hasElaborateCopyConstructor = hasMember!(S, "__postblit")
            || anySatisfy!(.hasElaborateCopyConstructor, FieldTypeTuple!S);
    }
    else
    {
        enum bool hasElaborateCopyConstructor = false;
    }
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
   True if $(D S) or any type directly embedded in the representation of $(D S)
   defines an elaborate assignment. Elaborate assignments are introduced by
   defining $(D opAssign(typeof(this))) or $(D opAssign(ref typeof(this)))
   for a $(D struct) or when there is a compiler-generated $(D opAssign).

   A type $(D S) gets compiler-generated $(D opAssign) in case it has
   an elaborate copy constructor or elaborate destructor.

   Classes and unions never have elaborate assignments.

   Note: Structs with (possibly nested) postblit operator(s) will have a
   hidden yet elaborate compiler generated assignment operator (unless
   explicitly disabled).
 */
template hasElaborateAssign(S)
{
    import std.meta : anySatisfy;
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

/**
   True if $(D S) or any type directly embedded in the representation
   of $(D S) defines an elaborate destructor. Elaborate destructors
   are introduced by defining $(D ~this()) for a $(D
   struct).

   Classes and unions never have elaborate destructors, even
   though classes may define $(D ~this()).
 */
template hasElaborateDestructor(S)
{
    import std.meta : anySatisfy;
    static if (isStaticArray!S && S.length)
    {
        enum bool hasElaborateDestructor = hasElaborateDestructor!(typeof(S.init[0]));
    }
    else static if (is(S == struct))
    {
        enum hasElaborateDestructor = hasMember!(S, "__dtor")
            || anySatisfy!(.hasElaborateDestructor, FieldTypeTuple!S);
    }
    else
    {
        enum bool hasElaborateDestructor = false;
    }
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

package alias Identity(alias A) = A;

/**
   Yields $(D true) if and only if $(D T) is an aggregate that defines
   a symbol called $(D name).
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
    // 8321
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
 *     T = Type containing symbol $(D member).
 *     member = Name of symbol to test that resides in $(D T).
 *
 * Returns:
 *     $(D true) iff $(D member) exists and is static.
 */
template hasStaticMember(T, string member)
{
    static if (__traits(hasMember, T, member))
    {
        import std.meta : Alias;
        alias sym = Alias!(__traits(getMember, T, member));

        static if (__traits(getOverloads, T, member).length == 0)
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

        shared void g();

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

        shared void g() { }

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
    static assert(!hasStaticMember!(S, "dm"));
    static assert( hasStaticMember!(S, "sd"));
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
}

/**
Retrieves the members of an enumerated type $(D enum E).

Params:
 E = An enumerated type. $(D E) may have duplicated values.

Returns:
 Static tuple composed of the members of the enumerated type $(D E).
 The members are arranged in the same order as declared in $(D E).

Note:
 An enum can have multiple members which have the same value. If you want
 to use EnumMembers to e.g. generate switch cases at compile-time,
 you should use the $(REF NoDuplicates, std,meta) template to avoid
 generating duplicate switch cases.

Note:
 Returned values are strictly typed with $(D E). Thus, the following code
 does not work without the explicit cast:
--------------------
enum E : int { a, b, c }
int[] abc = cast(int[]) [ EnumMembers!E ];
--------------------
 Cast is not necessary if the type of the variable is inferred. See the
 example below.

Example:
 Creating an array of enumerated values:
--------------------
enum Sqrts : real
{
    one   = 1,
    two   = 1.41421,
    three = 1.73205,
}
auto sqrts = [ EnumMembers!Sqrts ];
assert(sqrts == [ Sqrts.one, Sqrts.two, Sqrts.three ]);
--------------------

 A generic function $(D rank(v)) in the following example uses this
 template for finding a member $(D e) in an enumerated type $(D E).
--------------------
// Returns i if e is the i-th enumerator of E.
size_t rank(E)(E e)
    if (is(E == enum))
{
    foreach (i, member; EnumMembers!E)
    {
        if (e == member)
            return i;
    }
    assert(0, "Not an enum member");
}

enum Mode
{
    read  = 1,
    write = 2,
    map   = 4,
}
assert(rank(Mode.read ) == 0);
assert(rank(Mode.write) == 1);
assert(rank(Mode.map  ) == 2);
--------------------
 */
template EnumMembers(E)
    if (is(E == enum))
{
    import std.meta : AliasSeq;
    // Supply the specified identifier to an constant value.
    template WithIdentifier(string ident)
    {
        static if (ident == "Symbolize")
        {
            template Symbolize(alias value)
            {
                enum Symbolize = value;
            }
        }
        else
        {
            mixin("template Symbolize(alias "~ ident ~")"
                 ~"{"
                     ~"alias Symbolize = "~ ident ~";"
                 ~"}");
        }
    }

    template EnumSpecificMembers(names...)
    {
        static if (names.length == 1)
        {
            alias EnumSpecificMembers = AliasSeq!(WithIdentifier!(names[0])
                        .Symbolize!(__traits(getMember, E, names[0])));
        }
        else static if (names.length > 0)
        {
            alias EnumSpecificMembers =
                AliasSeq!(
                    WithIdentifier!(names[0])
                        .Symbolize!(__traits(getMember, E, names[0])),
                    EnumSpecificMembers!(names[1 .. $/2]),
                    EnumSpecificMembers!(names[$/2..$])
                );
        }
        else
        {
            alias EnumSpecificMembers = AliasSeq!();
        }
    }

    alias EnumMembers = EnumSpecificMembers!(__traits(allMembers, E));
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

@safe unittest // Bugzilla 14561: huge enums
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
    struct S { }
    static assert(!__traits(compiles, BaseClassesTuple!S));
    interface I { }
    static assert(!__traits(compiles, BaseClassesTuple!I));
    class C4 : I { }
    class C5 : C4, I { }
    static assert(is(BaseClassesTuple!C5 == AliasSeq!(C4, Object)));
}

/**
 * Get a $(D_PARAM AliasSeq) of $(I all) interfaces directly or
 * indirectly inherited by this class or interface. Interfaces do not
 * repeat if multiply implemented. $(D_PARAM InterfacesTuple!Object)
 * yields the empty type tuple.
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

@safe unittest
{
    // doc example
    interface I1 {}
    interface I2 {}
    class A : I1, I2 { }
    class B : A, I1 { }
    class C : B { }
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
template TransitiveBaseTypeTuple(T)
{
    static if (is(T == Object))
        alias TransitiveBaseTypeTuple = AliasSeq!();
    else
        alias TransitiveBaseTypeTuple =
            AliasSeq!(BaseClassesTuple!T, InterfacesTuple!T);
}

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
Returns a tuple of non-static functions with the name $(D name) declared in the
class or interface $(D C).  Covariant duplicates are shrunk into the most
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
                alias inSight = AliasSeq!(__traits(getVirtualFunctions, Node, name));

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
                    static if (is(AliasSeq!(__traits(parent, target))[0] : AliasSeq!(__traits(parent, rest[0]))[0]))
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

@safe unittest // Issue 15920
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
    alias bfs = AliasSeq!(__traits(getOverloads, B, "f"));
    assert(__traits(isSame, fs[0], bfs[0]) || __traits(isSame, fs[0], bfs[1]));
    assert(__traits(isSame, fs[1], bfs[0]) || __traits(isSame, fs[1], bfs[1]));
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
Returns an alias to the template that $(D T) is an instance of.
 */
template TemplateOf(alias T : Base!Args, alias Base, Args...)
{
    alias TemplateOf = Base;
}

/// ditto
template TemplateOf(T : Base!Args, alias Base, Args...)
{
    alias TemplateOf = Base;
}

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


/**
Returns a $(D AliasSeq) of the template arguments used to instantiate $(D T).
 */
template TemplateArgsOf(alias T : Base!Args, alias Base, Args...)
{
    alias TemplateArgsOf = Args;
}

/// ditto
template TemplateArgsOf(T : Base!Args, alias Base, Args...)
{
    alias TemplateArgsOf = Args;
}

///
@safe unittest
{
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


private template maxAlignment(U...) if (isTypeTuple!U)
{
    import std.meta : staticMap;
    static if (U.length == 0)
        static assert(0);
    else static if (U.length == 1)
        enum maxAlignment = U[0].alignof;
    else
    {
        import std.algorithm.comparison : max;
        enum maxAlignment = max(staticMap!(.maxAlignment, U));
    }
}


/**
Returns class instance alignment.
 */
template classInstanceAlignment(T) if (is(T == class))
{
    alias classInstanceAlignment = maxAlignment!(void*, typeof(T.tupleof));
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
    static if (!T.length)
    {
        alias CommonType = void;
    }
    else static if (T.length == 1)
    {
        static if (is(typeof(T[0])))
        {
            alias CommonType = typeof(T[0]);
        }
        else
        {
            alias CommonType = T[0];
        }
    }
    else static if (is(typeof(true ? T[0].init : T[1].init) U))
    {
        alias CommonType = CommonType!(U, T[2 .. $]);
    }
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
@safe unittest
{
    static assert(is(CommonType!(3) == int));
    static assert(is(CommonType!(double, 4, float) == double));
    static assert(is(CommonType!(string, char[]) == const(char)[]));
    static assert(is(CommonType!(3, 3U) == uint));
}


/**
 * Returns a tuple with all possible target types of an implicit
 * conversion of a value of type $(D_PARAM T).
 *
 * Important note:
 *
 * The possible targets are computed more conservatively than the D
 * 2.005 compiler does, eliminating all dangerous conversions. For
 * example, $(D_PARAM ImplicitConversionTargets!double) does not
 * include $(D_PARAM float).
 */
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
    else static if (is(T : Object))
        alias ImplicitConversionTargets = TransitiveBaseTypeTuple!(T);
    else static if (isDynamicArray!T && !is(typeof(T.init[0]) == const))
        alias ImplicitConversionTargets =
            AliasSeq!(const(Unqual!(typeof(T.init[0])))[]);
    else static if (is(T : void*))
        alias ImplicitConversionTargets = AliasSeq!(void*);
    else
        alias ImplicitConversionTargets = AliasSeq!();
}

@safe unittest
{
    static assert(is(ImplicitConversionTargets!(double)[0] == real));
    static assert(is(ImplicitConversionTargets!(string)[0] == const(char)[]));
}

/**
Is $(D From) implicitly convertible to $(D To)?
 */
template isImplicitlyConvertible(From, To)
{
    enum bool isImplicitlyConvertible = is(typeof({
        void fun(ref From v)
        {
            void gun(To) {}
            gun(v);
        }
    }));
}

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
Returns $(D true) iff a value of type $(D Rhs) can be assigned to a variable of
type $(D Lhs).

$(D isAssignable) returns whether both an lvalue and rvalue can be assigned.

If you omit $(D Rhs), $(D isAssignable) will check identity assignable of $(D Lhs).
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

// ditto
private enum isRvalueAssignable(Lhs, Rhs = Lhs) = __traits(compiles, lvalueOf!Lhs = rvalueOf!Rhs);

// ditto
private enum isLvalueAssignable(Lhs, Rhs = Lhs) = __traits(compiles, lvalueOf!Lhs = lvalueOf!Rhs);

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
    struct S6 { void opAssign(in ref S5); }
    static assert(!isAssignable!(S6, S5));
    static assert(!isRvalueAssignable!(S6, S5));
    static assert( isLvalueAssignable!(S6, S5));
    static assert( isLvalueAssignable!(S6, immutable S5));
}


// Equivalent with TypeStruct::isAssignable in compiler code.
package template isBlitAssignable(T)
{
    static if (is(OriginalType!T U) && !is(T == U))
    {
        enum isBlitAssignable = isBlitAssignable!U;
    }
    else static if (isStaticArray!T && is(T == E[n], E, size_t n))
    // Workaround for issue 11499 : isStaticArray!T should not be necessary.
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
Works like $(D isImplicitlyConvertible), except this cares only about storage
classes of the arguments.
 */
private template isStorageClassImplicitlyConvertible(From, To)
{
    alias Pointify(T) = void*;

    enum isStorageClassImplicitlyConvertible = isImplicitlyConvertible!(
            ModifyTypePreservingTQ!(Pointify, From),
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
Determines whether the function type $(D F) is covariant with $(D G), i.e.,
functions of the type $(D F) can override ones of the type $(D G).
 */
template isCovariantWith(F, G)
    if (is(F == function) && is(G == function))
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
         *  - require exact match for types (cf. bugzilla 3075)
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
Creates an lvalue or rvalue of type $(D T) for $(D typeof(...)) and
$(D __traits(compiles, ...)) purposes. No actual value is returned.

Note: Trying to use returned value will result in a
"Symbol Undefined" error at link time.

Example:
---
// Note that `f` doesn't have to be implemented
// as is isn't called.
int f(int);
bool f(ref int);
static assert(is(typeof(f(rvalueOf!int)) == int));
static assert(is(typeof(f(lvalueOf!int)) == bool));

int i = rvalueOf!int; // error, no actual value is returned
---
*/
@property T rvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);

/// ditto
@property ref T lvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);

// Note: unittest can't be used as an example here as function overloads
// aren't allowed inside functions.

@system unittest
{
    void needLvalue(T)(ref T);
    static struct S { }
    int i;
    struct Nested { void f() { ++i; } }
    foreach (T; AliasSeq!(int, immutable int, inout int, string, S, Nested, Object))
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

private template AliasThisTypeOf(T) if (isAggregateType!T)
{
    alias members = AliasSeq!(__traits(getAliasThis, T));

    static if (members.length == 1)
    {
        alias AliasThisTypeOf = typeof(__traits(getMember, T.init, members[0]));
    }
    else
        static assert(0, T.stringof~" does not have alias this type");
}

/*
 */
template BooleanTypeOf(T)
{
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = BooleanTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(Unqual!X == bool))
    {
        alias BooleanTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not boolean type");
}

@safe unittest
{
    // unexpected failure, maybe dmd type-merging bug
    foreach (T; AliasSeq!bool)
        foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == BooleanTypeOf!(            Q!T  )));
            static assert( is(Q!T == BooleanTypeOf!( SubTypeOf!(Q!T) )));
        }

    foreach (T; AliasSeq!(void, NumericTypeList, ImaginaryTypeList, ComplexTypeList, CharTypeList))
        foreach (Q; TypeQualifierList)
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
    import std.meta : staticIndexOf;
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = IntegralTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (staticIndexOf!(Unqual!X, IntegralTypeList) >= 0)
    {
        alias IntegralTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not an integral type");
}

@safe unittest
{
    foreach (T; IntegralTypeList)
        foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == IntegralTypeOf!(            Q!T  )));
            static assert( is(Q!T == IntegralTypeOf!( SubTypeOf!(Q!T) )));
        }

    foreach (T; AliasSeq!(void, bool, FloatingPointTypeList, ImaginaryTypeList, ComplexTypeList, CharTypeList))
        foreach (Q; TypeQualifierList)
        {
            static assert(!is(IntegralTypeOf!(            Q!T  )));
            static assert(!is(IntegralTypeOf!( SubTypeOf!(Q!T) )));
        }
}

/*
 */
template FloatingPointTypeOf(T)
{
    import std.meta : staticIndexOf;
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = FloatingPointTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (staticIndexOf!(Unqual!X, FloatingPointTypeList) >= 0)
    {
        alias FloatingPointTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not a floating point type");
}

@safe unittest
{
    foreach (T; FloatingPointTypeList)
        foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == FloatingPointTypeOf!(            Q!T  )));
            static assert( is(Q!T == FloatingPointTypeOf!( SubTypeOf!(Q!T) )));
        }

    foreach (T; AliasSeq!(void, bool, IntegralTypeList, ImaginaryTypeList, ComplexTypeList, CharTypeList))
        foreach (Q; TypeQualifierList)
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
    foreach (T; NumericTypeList)
        foreach (Q; TypeQualifierList)
        {
            static assert( is(Q!T == NumericTypeOf!(            Q!T  )));
            static assert( is(Q!T == NumericTypeOf!( SubTypeOf!(Q!T) )));
        }

    foreach (T; AliasSeq!(void, bool, CharTypeList, ImaginaryTypeList, ComplexTypeList))
        foreach (Q; TypeQualifierList)
        {
            static assert(!is(NumericTypeOf!(            Q!T  )));
            static assert(!is(NumericTypeOf!( SubTypeOf!(Q!T) )));
        }
}

/*
 */
template UnsignedTypeOf(T)
{
    import std.meta : staticIndexOf;
    static if (is(IntegralTypeOf!T X) &&
               staticIndexOf!(Unqual!X, UnsignedIntTypeList) >= 0)
        alias UnsignedTypeOf = X;
    else
        static assert(0, T.stringof~" is not an unsigned type.");
}

/*
 */
template SignedTypeOf(T)
{
    import std.meta : staticIndexOf;
    static if (is(IntegralTypeOf!T X) &&
               staticIndexOf!(Unqual!X, SignedIntTypeList) >= 0)
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
    import std.meta : staticIndexOf;
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = CharTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (staticIndexOf!(Unqual!X, CharTypeList) >= 0)
    {
        alias CharTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not a character type");
}

@safe unittest
{
    foreach (T; CharTypeList)
        foreach (Q; TypeQualifierList)
        {
            static assert( is(CharTypeOf!(            Q!T  )));
            static assert( is(CharTypeOf!( SubTypeOf!(Q!T) )));
        }

    foreach (T; AliasSeq!(void, bool, NumericTypeList, ImaginaryTypeList, ComplexTypeList))
        foreach (Q; TypeQualifierList)
        {
            static assert(!is(CharTypeOf!(            Q!T  )));
            static assert(!is(CharTypeOf!( SubTypeOf!(Q!T) )));
        }

    foreach (T; AliasSeq!(string, wstring, dstring, char[4]))
        foreach (Q; TypeQualifierList)
        {
            static assert(!is(CharTypeOf!(            Q!T  )));
            static assert(!is(CharTypeOf!( SubTypeOf!(Q!T) )));
        }
}

/*
 */
template StaticArrayTypeOf(T)
{
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = StaticArrayTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(X : E[n], E, size_t n))
        alias StaticArrayTypeOf = X;
    else
        static assert(0, T.stringof~" is not a static array type");
}

@safe unittest
{
    foreach (T; AliasSeq!(bool, NumericTypeList, ImaginaryTypeList, ComplexTypeList))
        foreach (Q; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
        {
            static assert(is( Q!(   T[1] ) == StaticArrayTypeOf!( Q!(              T[1]  ) ) ));

            foreach (P; TypeQualifierList)
            { // SubTypeOf cannot have inout type
                static assert(is( Q!(P!(T[1])) == StaticArrayTypeOf!( Q!(SubTypeOf!(P!(T[1]))) ) ));
            }
        }

    foreach (T; AliasSeq!void)
        foreach (Q; AliasSeq!TypeQualifierList)
        {
            static assert(is( StaticArrayTypeOf!( Q!(void[1]) ) == Q!(void[1]) ));
        }
}

/*
 */
template DynamicArrayTypeOf(T)
{
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = DynamicArrayTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(Unqual!X : E[], E) && !is(typeof({ enum n = X.length; })))
    {
        alias DynamicArrayTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not a dynamic array");
}

@safe unittest
{
    foreach (T; AliasSeq!(/*void, */bool, NumericTypeList, ImaginaryTypeList, ComplexTypeList))
        foreach (Q; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
        {
            static assert(is( Q!T[]  == DynamicArrayTypeOf!( Q!T[] ) ));
            static assert(is( Q!(T[])  == DynamicArrayTypeOf!( Q!(T[]) ) ));

            foreach (P; AliasSeq!(MutableOf, ConstOf, ImmutableOf))
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
Always returns the Dynamic Array version.
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
    foreach (T; CharTypeList)
        foreach (Q; AliasSeq!(MutableOf, ConstOf, ImmutableOf, InoutOf))
        {
            static assert(is(Q!T[] == StringTypeOf!( Q!T[] )));

            static if (!__traits(isSame, Q, InoutOf))
            {
                static assert(is(Q!T[] == StringTypeOf!( SubTypeOf!(Q!T[]) )));

                alias Str = Q!T[];
                class C(S) { S val;  alias val this; }
                static assert(is(StringTypeOf!(C!Str) == Str));
            }
        }

    foreach (T; CharTypeList)
        foreach (Q; AliasSeq!(SharedOf, SharedConstOf, SharedInoutOf))
        {
            static assert(!is(StringTypeOf!( Q!T[] )));
        }
}

@safe unittest
{
    static assert(is(StringTypeOf!(char[4]) == char[]));
}

/*
 */
template AssocArrayTypeOf(T)
{
    static if (is(AliasThisTypeOf!T AT) && !is(AT[] == AT))
        alias X = AssocArrayTypeOf!AT;
    else
        alias X = OriginalType!T;

    static if (is(Unqual!X : V[K], K, V))
    {
        alias AssocArrayTypeOf = X;
    }
    else
        static assert(0, T.stringof~" is not an associative array type");
}

@safe unittest
{
    foreach (T; AliasSeq!(int/*bool, CharTypeList, NumericTypeList, ImaginaryTypeList, ComplexTypeList*/))
        foreach (P; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
            foreach (Q; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
                foreach (R; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
                {
                    static assert(is( P!(Q!T[R!T]) == AssocArrayTypeOf!(            P!(Q!T[R!T])  ) ));
                }

    foreach (T; AliasSeq!(int/*bool, CharTypeList, NumericTypeList, ImaginaryTypeList, ComplexTypeList*/))
        foreach (O; AliasSeq!(TypeQualifierList, InoutOf, SharedInoutOf))
            foreach (P; AliasSeq!TypeQualifierList)
                foreach (Q; AliasSeq!TypeQualifierList)
                    foreach (R; AliasSeq!TypeQualifierList)
                    {
                        static assert(is( O!(P!(Q!T[R!T])) == AssocArrayTypeOf!( O!(SubTypeOf!(P!(Q!T[R!T]))) ) ));
                    }
}

/*
 */
template BuiltinTypeOf(T)
{
         static if (is(T : void))               alias BuiltinTypeOf = void;
    else static if (is(BooleanTypeOf!T X))      alias BuiltinTypeOf = X;
    else static if (is(IntegralTypeOf!T X))     alias BuiltinTypeOf = X;
    else static if (is(FloatingPointTypeOf!T X))alias BuiltinTypeOf = X;
    else static if (is(T : const(ireal)))       alias BuiltinTypeOf = ireal;  //TODO
    else static if (is(T : const(creal)))       alias BuiltinTypeOf = creal;  //TODO
    else static if (is(CharTypeOf!T X))         alias BuiltinTypeOf = X;
    else static if (is(ArrayTypeOf!T X))        alias BuiltinTypeOf = X;
    else static if (is(AssocArrayTypeOf!T X))   alias BuiltinTypeOf = X;
    else                                        static assert(0);
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// isSomething
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
 * Detect whether $(D T) is a built-in boolean type.
 */
enum bool isBoolean(T) = is(BooleanTypeOf!T) && !isAggregateType!T;

///
@safe unittest
{
    static assert( isBoolean!bool);
    enum EB : bool { a = true }
    static assert( isBoolean!EB);
    static assert(!isBoolean!(SubTypeOf!bool));
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
 * Detect whether $(D T) is a built-in integral type. Types $(D bool),
 * $(D char), $(D wchar), and $(D dchar) are not considered integral.
 */
enum bool isIntegral(T) = is(IntegralTypeOf!T) && !isAggregateType!T;

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
    foreach (T; IntegralTypeList)
    {
        foreach (Q; TypeQualifierList)
        {
            static assert( isIntegral!(Q!T));
            static assert(!isIntegral!(SubTypeOf!(Q!T)));
        }
    }

    static assert(!isIntegral!float);

    enum EU : uint { a = 0, b = 1, c = 2 }  // base type is unsigned
    enum EI : int { a = -1, b = 0, c = 1 }  // base type is signed (bug 7909)
    static assert(isIntegral!EU &&  isUnsigned!EU && !isSigned!EU);
    static assert(isIntegral!EI && !isUnsigned!EI &&  isSigned!EI);
}

/**
 * Detect whether $(D T) is a built-in floating point type.
 */
enum bool isFloatingPoint(T) = __traits(isFloating, T) && !(is(Unqual!T == cfloat) ||
                                                            is(Unqual!T == cdouble) ||
                                                            is(Unqual!T == creal) ||
                                                            is(Unqual!T == ifloat) ||
                                                            is(Unqual!T == idouble) ||
                                                            is(Unqual!T == ireal));

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

    // complex and imaginary numbers do not pass
    static assert(
        !isFloatingPoint!cfloat &&
        !isFloatingPoint!ifloat
    );

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

    foreach (T; AliasSeq!(FloatingPointTypeList, EF))
    {
        foreach (Q; TypeQualifierList)
        {
            static assert( isFloatingPoint!(Q!T));
            static assert(!isFloatingPoint!(SubTypeOf!(Q!T)));
        }
    }
    foreach (T; IntegralTypeList)
    {
        foreach (Q; TypeQualifierList)
        {
            static assert(!isFloatingPoint!(Q!T));
        }
    }
}

// https://issues.dlang.org/show_bug.cgi?id=17195
@safe unittest
{
    static assert(!isFloatingPoint!cfloat);
    static assert(!isFloatingPoint!cdouble);
    static assert(!isFloatingPoint!creal);

    static assert(!isFloatingPoint!ifloat);
    static assert(!isFloatingPoint!idouble);
    static assert(!isFloatingPoint!ireal);
}

/**
 * Detect whether $(D T) is a built-in numeric type (integral or floating
 * point).
 */
enum bool isNumeric(T) = __traits(isArithmetic, T) && !(is(Unqual!T == bool) ||
                                                        is(Unqual!T == char) ||
                                                        is(Unqual!T == wchar) ||
                                                        is(Unqual!T == dchar));

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

    static assert(!isIntegral!S);
}

@safe unittest
{
    foreach (T; AliasSeq!(NumericTypeList))
    {
        foreach (Q; TypeQualifierList)
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
}

/**
 * Detect whether $(D T) is a scalar type (a built-in numeric, character or
 * boolean type).
 */
enum bool isScalarType(T) = is(T : real) && !isAggregateType!T;

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
 * Detect whether $(D T) is a basic type (scalar type or void).
 */
enum bool isBasicType(T) = isScalarType!T || is(Unqual!T == void);

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
 * Detect whether $(D T) is a built-in unsigned numeric type.
 */
enum bool isUnsigned(T) = __traits(isUnsigned, T) && !(is(Unqual!T == char) ||
                                                       is(Unqual!T == wchar) ||
                                                       is(Unqual!T == dchar) ||
                                                       is(Unqual!T == bool));

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
    foreach (T; AliasSeq!(UnsignedIntTypeList))
    {
        foreach (Q; TypeQualifierList)
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
}

/**
 * Detect whether $(D T) is a built-in signed numeric type.
 */
enum bool isSigned(T) = __traits(isArithmetic, T) && !__traits(isUnsigned, T);

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

    foreach (T; AliasSeq!(SignedIntTypeList))
    {
        foreach (Q; TypeQualifierList)
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
}

// https://issues.dlang.org/show_bug.cgi?id=17196
@safe unittest
{
    static assert(isUnsigned!bool == false);
    static assert(isSigned!bool == false);
}

/**
 * Detect whether $(D T) is one of the built-in character types.
 *
 * The built-in char types are any of $(D char), $(D wchar) or $(D dchar), with
 * or without qualifiers.
 */
enum bool isSomeChar(T) = is(CharTypeOf!T) && !isAggregateType!T;

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

    foreach (T; AliasSeq!(CharTypeList, EC))
    {
        foreach (Q; TypeQualifierList)
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
Detect whether $(D T) is one of the built-in string types.

The built-in string types are $(D Char[]), where $(D Char) is any of $(D char),
$(D wchar) or $(D dchar), with or without qualifiers.

Static arrays of characters (like $(D char[80])) are not considered
built-in string types.
 */
enum bool isSomeString(T) = is(StringTypeOf!T) && !isAggregateType!T && !isStaticArray!T;

///
@safe unittest
{
    //String types
    static assert( isSomeString!string);
    static assert( isSomeString!(wchar[]));
    static assert( isSomeString!(dchar[]));
    static assert( isSomeString!(typeof("aaa")));
    static assert( isSomeString!(const(char)[]));

    enum ES : string { a = "aaa", b = "bbb" }
    static assert( isSomeString!ES);

    //Non string types
    static assert(!isSomeString!int);
    static assert(!isSomeString!(int[]));
    static assert(!isSomeString!(byte[]));
    static assert(!isSomeString!(typeof(null)));
    static assert(!isSomeString!(char[4]));
}

@safe unittest
{
    foreach (T; AliasSeq!(char[], dchar[], string, wstring, dstring))
    {
        static assert( isSomeString!(           T ));
        static assert(!isSomeString!(SubTypeOf!(T)));
    }
}

/**
 * Detect whether type $(D T) is a narrow string.
 *
 * All arrays that use char, wchar, and their qualified versions are narrow
 * strings. (Those include string and wstring).
 */
enum bool isNarrowString(T) = (is(T : const char[]) || is(T : const wchar[])) && !isAggregateType!T && !isStaticArray!T;

///
@safe unittest
{
    static assert(isNarrowString!string);
    static assert(isNarrowString!wstring);
    static assert(isNarrowString!(char[]));
    static assert(isNarrowString!(wchar[]));

    static assert(!isNarrowString!dstring);
    static assert(!isNarrowString!(dchar[]));
}

@safe unittest
{
    foreach (T; AliasSeq!(char[], string, wstring))
    {
        foreach (Q; AliasSeq!(MutableOf, ConstOf, ImmutableOf)/*TypeQualifierList*/)
        {
            static assert( isNarrowString!(            Q!T  ));
            static assert(!isNarrowString!( SubTypeOf!(Q!T) ));
        }
    }

    foreach (T; AliasSeq!(int, int[], byte[], dchar[], dstring, char[4]))
    {
        foreach (Q; TypeQualifierList)
        {
            static assert(!isNarrowString!(            Q!T  ));
            static assert(!isNarrowString!( SubTypeOf!(Q!T) ));
        }
    }
}

/**
 * Detects whether `T` is a comparable type. Basic types and structs and
 * classes that implement opCmp are ordering comparable.
 */
enum bool isOrderingComparable(T) = ifTestable!(T, unaryFun!"a < a");

///
@safe unittest
{
    static assert(isOrderingComparable!int);
    static assert(isOrderingComparable!string);
    static assert(!isOrderingComparable!creal);

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
enum bool isEqualityComparable(T) = ifTestable!(T, unaryFun!"a == a");

@safe unittest
{
    static assert(isEqualityComparable!int);
    static assert(isEqualityComparable!string);
    static assert(isEqualityComparable!creal);
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
 * Detect whether $(D T) is a struct, static array, or enum that is implicitly
 * convertible to a string.
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

@safe unittest // Bugzilla 16573
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
 * Detect whether type $(D T) is a string that will be autodecoded.
 *
 * All arrays that use char, wchar, and their qualified versions are narrow
 * strings. (Those include string and wstring).
 * Aggregates that implicitly cast to narrow strings are included.
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
enum bool isAutodecodableString(T) = (is(T : const char[]) || is(T : const wchar[])) && !isStaticArray!T;

///
@safe unittest
{
    static struct Stringish
    {
        string s;
        alias s this;
    }
    assert(isAutodecodableString!wstring);
    assert(isAutodecodableString!Stringish);
    assert(!isAutodecodableString!dstring);
}

/**
 * Detect whether type $(D T) is a static array.
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
    foreach (T; AliasSeq!(int[51], int[][2],
                           char[][int][11], immutable char[13u],
                           const(real)[1], const(real)[1][1], void[0]))
    {
        foreach (Q; TypeQualifierList)
        {
            static assert( isStaticArray!(            Q!T  ));
            static assert(!isStaticArray!( SubTypeOf!(Q!T) ));
        }
    }

    //enum ESA : int[1] { a = [1], b = [2] }
    //static assert( isStaticArray!ESA);
}

/**
 * Detect whether type $(D T) is a dynamic array.
 */
enum bool isDynamicArray(T) = is(DynamicArrayTypeOf!T) && !isAggregateType!T;

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
    foreach (T; AliasSeq!(int[], char[], string, long[3][], double[string][]))
    {
        foreach (Q; TypeQualifierList)
        {
            static assert( isDynamicArray!(            Q!T  ));
            static assert(!isDynamicArray!( SubTypeOf!(Q!T) ));
        }
    }
}

/**
 * Detect whether type $(D T) is an array (static or dynamic; for associative
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
    foreach (T; AliasSeq!(int[], int[5], void[]))
    {
        foreach (Q; TypeQualifierList)
        {
            static assert( isArray!(Q!T));
            static assert(!isArray!(SubTypeOf!(Q!T)));
        }
    }
}

/**
 * Detect whether $(D T) is an associative array type
 */
enum bool isAssociativeArray(T) = __traits(isAssociativeArray, T);

@safe unittest
{
    struct Foo
    {
        @property uint[] keys()   { return null; }
        @property uint[] values() { return null; }
    }

    foreach (T; AliasSeq!(int[int], int[string], immutable(char[5])[int]))
    {
        foreach (Q; TypeQualifierList)
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
 * Detect whether type $(D T) is a builtin type.
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
    static assert(!isBuiltinType!C);
    static assert(!isBuiltinType!U);
    static assert(!isBuiltinType!S);
    static assert(!isBuiltinType!I);
    static assert(!isBuiltinType!(void delegate(int)));
}

/**
 * Detect whether type $(D T) is a SIMD vector type.
 */
enum bool isSIMDVector(T) = is(T : __vector(V[N]), V, size_t N);

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
 * Detect whether type $(D T) is a pointer.
 */
enum bool isPointer(T) = is(T == U*, U) && !isAggregateType!T;

@safe unittest
{
    foreach (T; AliasSeq!(int*, void*, char[]*))
    {
        foreach (Q; TypeQualifierList)
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
 * Detect whether type $(D T) is an aggregate type.
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
 * Returns $(D true) if T can be iterated over using a $(D foreach) loop with
 * a single loop variable of automatically inferred type, regardless of how
 * the $(D foreach) loop is implemented.  This includes ranges, structs/classes
 * that define $(D opApply) with a single loop variable, and builtin dynamic,
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
template isExpressions(T ...)
{
    static if (T.length >= 2)
        enum bool isExpressions =
            isExpressions!(T[0 .. $/2]) &&
            isExpressions!(T[$/2 .. $]);
    else static if (T.length == 1)
        enum bool isExpressions =
            !is(T[0]) && __traits(compiles, { auto ex = T[0]; });
    else
        enum bool isExpressions = true; // default
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
 * Check whether the tuple $(D T) is a type tuple.
 * A type tuple only contains types.
 *
 * See_Also: $(LREF isExpressions).
 */
template isTypeTuple(T...)
{
    static if (T.length >= 2)
        enum bool isTypeTuple = isTypeTuple!(T[0 .. $/2]) && isTypeTuple!(T[$/2 .. $]);
    else static if (T.length == 1)
        enum bool isTypeTuple = is(T[0]);
    else
        enum bool isTypeTuple = true; // default
}

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
Detect whether symbol or type $(D T) is a function pointer.
 */
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
Detect whether symbol or type $(D T) is a delegate.
*/
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
Detect whether symbol or type $(D T) is a function, a function pointer or a delegate.
 */
template isSomeFunction(T...)
    if (T.length == 1)
{
    static if (is(typeof(& T[0]) U : U*) && is(U == function) || is(typeof(& T[0]) U == delegate))
    {
        // T is a (nested) function symbol.
        enum bool isSomeFunction = true;
    }
    else static if (is(T[0] W) || is(typeof(T[0]) W))
    {
        // T is an expression or a type.  Take the type of it and examine.
        static if (is(W F : F*) && is(F == function))
            enum bool isSomeFunction = true; // function pointer
        else
            enum bool isSomeFunction = is(W == function) || is(W == delegate);
    }
    else
        enum bool isSomeFunction = false;
}

@safe unittest
{
    static real func(ref int) { return 0; }
    static void prop() @property { }
    void nestedFunc() { }
    void nestedProp() @property { }
    class C
    {
        real method(ref int) { return 0; }
        real prop() @property { return 0; }
    }
    auto c = new C;
    auto fp = &func;
    auto dg = &c.method;
    real val;

    static assert( isSomeFunction!func);
    static assert( isSomeFunction!prop);
    static assert( isSomeFunction!nestedFunc);
    static assert( isSomeFunction!nestedProp);
    static assert( isSomeFunction!(C.method));
    static assert( isSomeFunction!(C.prop));
    static assert( isSomeFunction!(c.prop));
    static assert( isSomeFunction!(c.prop));
    static assert( isSomeFunction!fp);
    static assert( isSomeFunction!dg);
    static assert( isSomeFunction!(typeof(func)));
    static assert( isSomeFunction!(real function(ref int)));
    static assert( isSomeFunction!(real delegate(ref int)));
    static assert( isSomeFunction!((int a) { return a; }));

    static assert(!isSomeFunction!int);
    static assert(!isSomeFunction!val);
    static assert(!isSomeFunction!isSomeFunction);
}


/**
Detect whether $(D T) is a callable object, which can be called with the
function call operator $(D $(LPAREN)...$(RPAREN)).
 */
template isCallable(T...)
    if (T.length == 1)
{
    static if (is(typeof(& T[0].opCall) == delegate))
        // T is a object which has a member function opCall().
        enum bool isCallable = true;
    else static if (is(typeof(& T[0].opCall) V : V*) && is(V == function))
        // T is a type which has a static member function opCall().
        enum bool isCallable = true;
    else
        enum bool isCallable = isSomeFunction!T;
}

///
@safe unittest
{
    interface I { real value() @property; }
    struct S { static int opCall(int) { return 0; } }
    class C { int opCall(int) { return 0; } }
    auto c = new C;

    static assert( isCallable!c);
    static assert( isCallable!S);
    static assert( isCallable!(c.opCall));
    static assert( isCallable!(I.value));
    static assert( isCallable!((int a) { return a; }));

    static assert(!isCallable!I);
}


/**
 * Detect whether $(D T) is an abstract function.
 */
template isAbstractFunction(T...)
    if (T.length == 1)
{
    enum bool isAbstractFunction = __traits(isAbstractFunction, T[0]);
}

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
 * Detect whether $(D T) is a final function.
 */
template isFinalFunction(T...)
    if (T.length == 1)
{
    enum bool isFinalFunction = __traits(isFinalFunction, T[0]);
}

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
Determines whether function $(D f) requires a context pointer.
*/
template isNestedFunction(alias f)
{
    enum isNestedFunction = __traits(isNested, f);
}

@safe unittest
{
    static void f() { }
    void g() { }
    static assert(!isNestedFunction!f);
    static assert( isNestedFunction!g);
}

/**
 * Detect whether $(D T) is an abstract class.
 */
template isAbstractClass(T...)
    if (T.length == 1)
{
    enum bool isAbstractClass = __traits(isAbstractClass, T[0]);
}

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
 * Detect whether $(D T) is a final class.
 */
template isFinalClass(T...)
    if (T.length == 1)
{
    enum bool isFinalClass = __traits(isFinalClass, T[0]);
}

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

/**
Removes all qualifiers, if any, from type $(D T).
 */
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

/**
 * Copies type qualifiers from $(D FromType) to $(D ToType).
 *
 * Supported type qualifiers:
 * $(UL
 *     $(LI $(D const))
 *     $(LI $(D inout))
 *     $(LI $(D immutable))
 *     $(LI $(D shared))
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
Returns the type of `Target` with the "constness" of `Source`. A type's $(B constness)
refers to whether it is `const`, `immutable`, or `inout`. If `source` has no constness, the
returned type will be the same as `Target`.
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
is iterated over using a $(D foreach) loop with a single loop variable and
automatically inferred return type.  Note that this may not be the same as
$(D std.range.ElementType!Range) in the case of narrow strings, or if T
has both opApply and a range interface.
*/
template ForeachType(T)
{
    alias ForeachType = ReturnType!(typeof(
    (inout int x = 0)
    {
        foreach (elem; T.init)
        {
            return elem;
        }
        assert(0);
    }));
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
 * Strips off all $(D enum)s from type $(D T).
 */
template OriginalType(T)
{
    template Impl(T)
    {
        static if (is(T U == enum)) alias Impl = OriginalType!U;
        else                        alias Impl =              T;
    }

    alias OriginalType = ModifyTypePreservingTQ!(Impl, T);
}

///
@safe unittest
{
    enum E : real { a }
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
    import std.traits;
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
    import std.traits;
    alias Hash = int[string];
    static assert(is(KeyType!Hash == string));
    static assert(is(ValueType!Hash == int));
    KeyType!Hash str = "a"; // str is declared as string
    ValueType!Hash num = 1; // num is declared as int
}

/**
 * Returns the corresponding unsigned type for T. T must be a numeric
 * integral type, otherwise a compile-time error occurs.
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
template Largest(T...) if (T.length >= 1)
{
    static if (T.length == 1)
    {
        alias Largest = T[0];
    }
    else static if (T.length == 2)
    {
        static if (T[0].sizeof >= T[1].sizeof)
        {
            alias Largest = T[0];
        }
        else
        {
            alias Largest = T[1];
        }
    }
    else
    {
        alias Largest = Largest!(Largest!(T[0 .. $/2]), Largest!(T[$/2 .. $]));
    }
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
    foreach (T; AliasSeq!(bool, byte, short, int, long))
        static assert(mostNegative!T == T.min);

    foreach (T; AliasSeq!(ubyte, ushort, uint, ulong, char, wchar, dchar))
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
    foreach (T; AliasSeq!(bool, byte, ubyte, short, ushort, char, wchar))
    {
        static assert(is(Promoted!T == int));
        static assert(is(Promoted!(shared(const T)) == shared(const int)));
    }

    // already promoted:
    foreach (T; AliasSeq!(int, uint, long, ulong, float, double, real))
    {
        static assert(is(Promoted!T == T));
        static assert(is(Promoted!(immutable(T)) == immutable(T)));
    }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
// Misc.
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

/**
Returns the mangled name of symbol or type $(D sth).

$(D mangledName) is the same as builtin $(D .mangleof) property, but
might be more convenient in generic code, e.g. as a template argument
when invoking staticMap.
 */
template mangledName(sth...)
    if (sth.length == 1)
{
    enum string mangledName = sth[0].mangleof;
}

///
@safe unittest
{
    alias TL = staticMap!(mangledName, int, const int, immutable int);
    static assert(TL == AliasSeq!("i", "xi", "yi"));
}

version (unittest) void freeFunc(string);

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
  static if (is(typeof({ return x; }) : int delegate() pure))   // issue 9148
    static assert(mangledName!((int a) { return a+x; }) == "DFNaNbNiNfiZi");  // pure nothrow @safe @nogc
  else
    static assert(mangledName!((int a) { return a+x; }) == "DFNbNiNfiZi");  // nothrow @safe @nnogc
}

@system unittest
{
    // @system due to demangle
    // Test for bug 5718
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
Aliases itself to $(D T[0]) if the boolean $(D condition) is $(D true)
and to $(D T[1]) otherwise.
 */
template Select(bool condition, T...) if (T.length == 2)
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
If $(D cond) is $(D true), returns $(D a) without evaluating $(D
b). Otherwise, returns $(D b) without evaluating $(D a).
 */
A select(bool cond : true, A, B)(A a, lazy B b) { return a; }
/// Ditto
B select(bool cond : false, A, B)(lazy A a, B b) { return b; }

@safe unittest
{
    real pleasecallme() { return 0; }
    int dontcallme() { assert(0); }
    auto a = select!true(pleasecallme(), dontcallme());
    auto b = select!false(dontcallme(), pleasecallme());
    static assert(is(typeof(a) == real));
    static assert(is(typeof(b) == real));
}

/++
    Determine if a symbol has a given
    $(DDSUBLINK spec/attribute, uda, user-defined attribute).

    See_Also:
        $(LREF getUDAs)
  +/
template hasUDA(alias symbol, alias attribute)
{
    enum hasUDA = getUDAs!(symbol, attribute).length != 0;
}

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
    alias getUDAs = Filter!(isDesiredUDA, __traits(getAttributes, symbol));
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

/**
 * Gets all symbols within `symbol` that have the given user-defined attribute.
 * This is not recursive; it will not search for symbols within symbols such as
 * nested structs or unions.
 */
template getSymbolsByUDA(alias symbol, alias attribute)
{
    import std.format : format;
    import std.meta : AliasSeq, Filter;

    // translate a list of strings into symbols. mixing in the entire alias
    // avoids trying to access the symbol, which could cause a privacy violation
    template toSymbols(names...)
    {
        static if (names.length == 0)
            alias toSymbols = AliasSeq!();
        else
            mixin("alias toSymbols = AliasSeq!(symbol.%s, toSymbols!(names[1..$]));"
                  .format(names[0]));
    }

    // filtering inaccessible members
    enum isAccessibleMember(string name) = __traits(compiles, __traits(getMember, symbol, name));
    alias accessibleMembers = Filter!(isAccessibleMember, __traits(allMembers, symbol));

    // filtering not compiled members such as alias of basic types
    enum hasSpecificUDA(string name) = mixin("hasUDA!(symbol." ~ name ~ ", attribute)");
    enum isCorrectMember(string name) = __traits(compiles, hasSpecificUDA!(name));

    alias correctMembers = Filter!(isCorrectMember, accessibleMembers);
    alias membersWithUDA = toSymbols!(Filter!(hasSpecificUDA, correctMembers));

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

    @UDA("A")
    static struct C
    {
        @UDA("B")
        int d;
    }

    // Also checks the symbol itself
    static assert(getSymbolsByUDA!(C, UDA).length == 2);
    static assert(getSymbolsByUDA!(C, UDA)[0].stringof == "C");
    static assert(getSymbolsByUDA!(C, UDA)[1].stringof == "d");

    static struct D
    {
        int x;
    }

    //Finds nothing if there is no member with specific UDA
    static assert(getSymbolsByUDA!(D,UDA).length == 0);
}

// #15335: getSymbolsByUDA fails if type has private members
@safe unittest
{
    // HasPrivateMembers has, well, private members, one of which has a UDA.
    import std.internal.test.uda : Attr, HasPrivateMembers;
    // Trying access to private member from another file therefore we do not have access
    // for this otherwise we get deprecation warning - not visible from module
    static assert(getSymbolsByUDA!(HasPrivateMembers, Attr).length == 1);
    static assert(hasUDA!(getSymbolsByUDA!(HasPrivateMembers, Attr)[0], Attr));
}

///
@safe unittest
{
    enum Attr;
    struct A
    {
        alias int INT;
        alias void function(INT) SomeFunction;
        @Attr int a;
        int b;
        @Attr private int c;
        private int d;
    }

    // Here everything is fine, we have access to private member c
    static assert(getSymbolsByUDA!(A, Attr).length == 2);
    static assert(hasUDA!(getSymbolsByUDA!(A, Attr)[0], Attr));
    static assert(hasUDA!(getSymbolsByUDA!(A, Attr)[1], Attr));
}

// #16387: getSymbolsByUDA works with structs but fails with classes
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

/**
   Returns: $(D true) iff all types $(D T) are the same.
*/
template allSameType(T...)
{
    static if (T.length <= 1)
    {
        enum bool allSameType = true;
    }
    else
    {
        enum bool allSameType = is(T[0] == T[1]) && allSameType!(T[1..$]);
    }
}

///
@safe unittest
{
    static assert(allSameType!(int, int));
    static assert(allSameType!(int, int, int));
    static assert(allSameType!(float, float, float));
    static assert(!allSameType!(int, double));
    static assert(!allSameType!(int, float, double));
    static assert(!allSameType!(int, float, double, real));
    static assert(!allSameType!(short, int, float, double, real));
}

/**
   Returns: $(D true) iff the type $(D T) can be tested in an $(D
   if)-expression, that is if $(D if (pred(T.init)) {}) is compilable.
*/
enum ifTestable(T, alias pred = a => a) = __traits(compiles, { if (pred(T.init)) {} });

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
template isType(X...) if (X.length == 1)
{
    enum isType = is(X[0]);
}

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
template isFunction(X...) if (X.length == 1)
{
    static if (is(typeof(&X[0]) U : U*) && is(U == function) ||
               is(typeof(&X[0]) U == delegate))
    {
        // x is a (nested) function symbol.
        enum isFunction = true;
    }
    else static if (is(X[0] T))
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
template isFinal(X...) if (X.length == 1)
{
    static if (is(X[0] == class))
        enum isFinal = __traits(isFinalClass, X[0]);
    else static if (isFunction!X)
        enum isFinal = __traits(isFinalFunction, X[0]);
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
 + Params:
 +  S = The type to check.
 +
 + Returns:
 +  `true` if `S` can be copied. `false` otherwise.
 + ++/
enum isCopyable(S) = is(typeof(
    { S foo = S.init; S copy = foo; }
));

///
@safe unittest
{
    struct S1 {}                        // Fine. Can be copied
    struct S2 {         this(this) {}}  // Fine. Can be copied
    struct S3 {@disable this(this) {}}  // Not fine. Copying is disabled.
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

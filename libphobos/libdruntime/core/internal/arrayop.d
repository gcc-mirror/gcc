module core.internal.arrayop;
import core.internal.traits : Filter, Unqual;

version (GNU) version = GNU_OR_LDC;
version (LDC) version = GNU_OR_LDC;

/**
 * Perform array (vector) operations and store the result in `res`.  Operand
 * types and operations are passed as template arguments in Reverse Polish
 * Notation (RPN).

 * Operands can be slices or scalar types. The unqualified element types of all
 * slices must be `T`, scalar types must be implicitly convertible to `T`.
 *
 * Operations are encoded as strings, e.g. `"+"`, `"%"`, `"*="`. Unary
 * operations are prefixed with "u", e.g. `"u-"`, `"u~"`. Only the last
 * operation can and must be an assignment (`"="`) or op-assignment (`"op="`).
 *
 * All slice operands must have the same length as the result slice.
 *
 * Params: T[] = type of result slice
 *        Args = operand types and operations in RPN
 *         res = the slice in which to store the results
 *        args = operand values
 *
 * Returns: the slice containing the result
 */
T[] arrayOp(T : T[], Args...)(T[] res, Filter!(isType, Args) args) @trusted @nogc pure nothrow
{
    enum check = opsSupported!(true, T, Filter!(not!isType, Args)); // must support all scalar ops

    size_t pos;
    static if (vectorizeable!(T[], Args))
    {
        alias vec = .vec!T;
        alias load = .load!(T, vec.length);
        alias store = .store!(T, vec.length);

        // Given that there are at most as many scalars broadcast as there are
        // operations in any `ary[] = ary[] op const op const`, it should always be
        // worthwhile to choose vector operations.
        if (res.length >= vec.length)
        {
            mixin(initScalarVecs!Args);

            auto n = res.length / vec.length;
            do
            {
                mixin(vectorExp!Args ~ ";");
                pos += vec.length;
            }
            while (--n);
        }
    }
    for (; pos < res.length; ++pos)
        mixin(scalarExp!Args ~ ";");

    return res;
}

private:

// SIMD helpers

version (DigitalMars)
{
    import core.simd;

    template vec(T)
    {
        enum regsz = 16; // SSE2
        enum N = regsz / T.sizeof;
        alias vec = __vector(T[N]);
    }

    void store(T, size_t N)(T* p, in __vector(T[N]) val)
    {
        pragma(inline, true);
        alias vec = __vector(T[N]);

        static if (is(T == float))
            cast(void) __simd_sto(XMM.STOUPS, *cast(vec*) p, val);
        else static if (is(T == double))
            cast(void) __simd_sto(XMM.STOUPD, *cast(vec*) p, val);
        else
            cast(void) __simd_sto(XMM.STODQU, *cast(vec*) p, val);
    }

    const(__vector(T[N])) load(T, size_t N)(in T* p)
    {
        import core.simd;

        pragma(inline, true);
        alias vec = __vector(T[N]);

        static if (is(T == float))
            return __simd(XMM.LODUPS, *cast(const vec*) p);
        else static if (is(T == double))
            return __simd(XMM.LODUPD, *cast(const vec*) p);
        else
            return __simd(XMM.LODDQU, *cast(const vec*) p);
    }

    __vector(T[N]) binop(string op, T, size_t N)(in __vector(T[N]) a, in __vector(T[N]) b)
    {
        pragma(inline, true);
        return mixin("a " ~ op ~ " b");
    }

    __vector(T[N]) unaop(string op, T, size_t N)(in __vector(T[N]) a)
            if (op[0] == 'u')
    {
        pragma(inline, true);
        return mixin(op[1 .. $] ~ "a");
    }
}

// mixin gen

// Check whether operations `ops` are supported for type `T`. Fails with a human-friendly static assert message, if `fail` is true.
template opsSupported(bool fail, T, ops...) if (ops.length > 1)
{
    enum opsSupported = opsSupported!(fail, T, ops[0 .. $ / 2])
            && opsSupported!(fail, T, ops[$ / 2 .. $]);
}

template opsSupported(bool fail, T, string op)
{
    static if (isUnaryOp(op))
    {
        enum opsSupported = is(typeof((T a) => mixin(op[1 .. $] ~ " a")));
        static assert(!fail || opsSupported,
                "Unary op `" ~ op[1 .. $] ~ "` not supported for element type " ~ T.stringof ~ ".");
    }
    else
    {
        enum opsSupported = is(typeof((T a, T b) => mixin("a " ~ op ~ " b")));
        static assert(!fail || opsSupported,
                "Binary op `" ~ op ~ "` not supported for element type " ~ T.stringof ~ ".");
    }
}

// check whether slices have the unqualified element type `E` and scalars are implicitly convertible to `E`
// i.e. filter out things like float[] = float[] / size_t[]
enum compatibleVecTypes(E, T : T[]) = is(Unqual!T == Unqual!E); // array elem types must be same (maybe add cvtpi2ps)
enum compatibleVecTypes(E, T) = is(T : E); // scalar must be convertible to target elem type
enum compatibleVecTypes(E, Types...) = compatibleVecTypes!(E, Types[0 .. $ / 2])
        && compatibleVecTypes!(E, Types[$ / 2 .. $]);

version (GNU_OR_LDC)
{
    // leave it to the auto-vectorizer
    enum vectorizeable(E : E[], Args...) = false;
}
else
{
    // check whether arrayOp is vectorizable
    template vectorizeable(E : E[], Args...)
    {
        static if (is(vec!E))
            enum vectorizeable = opsSupported!(false, vec!E, Filter!(not!isType, Args))
                    && compatibleVecTypes!(E, Filter!(isType, Args));
        else
            enum vectorizeable = false;
    }

    version (X86_64) unittest
    {
        static assert(vectorizeable!(double[], const(double)[], double[], "+", "="));
        static assert(!vectorizeable!(double[], const(ulong)[], double[], "+", "="));
    }
}

bool isUnaryOp(string op)
{
    return op[0] == 'u';
}

bool isBinaryOp(string op)
{
    if (op == "^^")
        return true;
    if (op.length != 1)
        return false;
    switch (op[0])
    {
    case '+', '-', '*', '/', '%', '|', '&', '^':
        return true;
    default:
        return false;
    }
}

bool isBinaryAssignOp(string op)
{
    return op.length >= 2 && op[$ - 1] == '=' && isBinaryOp(op[0 .. $ - 1]);
}

// Generate mixin expression to perform scalar arrayOp loop expression, assumes
// `pos` to be the current slice index, `args` to contain operand values, and
// `res` the target slice.
string scalarExp(Args...)()
{
    string[] stack;
    size_t argsIdx;
    foreach (i, arg; Args)
    {
        static if (is(arg == T[], T))
            stack ~= "args[" ~ argsIdx++.toString ~ "][pos]";
        else static if (is(arg))
            stack ~= "args[" ~ argsIdx++.toString ~ "]";
        else static if (isUnaryOp(arg))
        {
            auto op = arg[0] == 'u' ? arg[1 .. $] : arg;
            stack[$ - 1] = op ~ stack[$ - 1];
        }
        else static if (arg == "=")
        {
            stack[$ - 1] = "res[pos] = cast(T)(" ~ stack[$ - 1] ~ ")";
        }
        else static if (isBinaryAssignOp(arg))
        {
            stack[$ - 1] = "res[pos] " ~ arg ~ " cast(T)(" ~ stack[$ - 1] ~ ")";
        }
        else static if (isBinaryOp(arg))
        {
            stack[$ - 2] = "(cast(T)(" ~ stack[$ - 2] ~ " " ~ arg ~ " " ~ stack[$ - 1] ~ "))";
            stack.length -= 1;
        }
        else
            assert(0, "Unexpected op " ~ arg);
    }
    assert(stack.length == 1);
    return stack[0];
}

// Generate mixin statement to perform vector loop initialization, assumes
// `args` to contain operand values.
string initScalarVecs(Args...)()
{
    size_t scalarsIdx;
    string res;
    foreach (aidx, arg; Args)
    {
        static if (is(arg == T[], T))
        {
        }
        else static if (is(arg))
            res ~= "immutable vec scalar" ~ scalarsIdx++.toString ~ " = args["
                ~ aidx.toString ~ "];\n";
    }
    return res;
}

// Generate mixin expression to perform vector arrayOp loop expression, assumes
// `pos` to be the current slice index, `args` to contain operand values, and
// `res` the target slice.
string vectorExp(Args...)()
{
    size_t scalarsIdx, argsIdx;
    string[] stack;
    foreach (i, arg; Args)
    {
        static if (is(arg == T[], T))
            stack ~= "load(&args[" ~ argsIdx++.toString ~ "][pos])";
        else static if (is(arg))
        {
            ++argsIdx;
            stack ~= "scalar" ~ scalarsIdx++.toString;
        }
        else static if (isUnaryOp(arg))
        {
            auto op = arg[0] == 'u' ? arg[1 .. $] : arg;
            stack[$ - 1] = "unaop!\"" ~ arg ~ "\"(" ~ stack[$ - 1] ~ ")";
        }
        else static if (arg == "=")
        {
            stack[$ - 1] = "store(&res[pos], " ~ stack[$ - 1] ~ ")";
        }
        else static if (isBinaryAssignOp(arg))
        {
            stack[$ - 1] = "store(&res[pos], binop!\"" ~ arg[0 .. $ - 1]
                ~ "\"(load(&res[pos]), " ~ stack[$ - 1] ~ "))";
        }
        else static if (isBinaryOp(arg))
        {
            stack[$ - 2] = "binop!\"" ~ arg ~ "\"(" ~ stack[$ - 2] ~ ", " ~ stack[$ - 1] ~ ")";
            stack.length -= 1;
        }
        else
            assert(0, "Unexpected op " ~ arg);
    }
    assert(stack.length == 1);
    return stack[0];
}

// other helpers

enum isType(T) = true;
enum isType(alias a) = false;
template not(alias tmlp)
{
    enum not(Args...) = !tmlp!Args;
}

string toString(size_t num)
{
    import core.internal.string : unsignedToTempString;

    char[20] buf = void;
    return unsignedToTempString(num, buf).idup;
}

bool contains(T)(in T[] ary, in T[] vals...)
{
    foreach (v1; ary)
        foreach (v2; vals)
            if (v1 == v2)
                return true;
    return false;
}

// tests

version (unittest) template TT(T...)
{
    alias TT = T;
}

version (unittest) template _arrayOp(Args...)
{
    alias _arrayOp = arrayOp!Args;
}

unittest
{
    static void check(string op, TA, TB, T, size_t N)(TA a, TB b, in ref T[N] exp)
    {
        T[N] res;
        _arrayOp!(T[], TA, TB, op, "=")(res[], a, b);
        foreach (i; 0 .. N)
            assert(res[i] == exp[i]);
    }

    static void check2(string unaOp, string binOp, TA, TB, T, size_t N)(TA a, TB b, in ref T[N] exp)
    {
        T[N] res;
        _arrayOp!(T[], TA, TB, unaOp, binOp, "=")(res[], a, b);
        foreach (i; 0 .. N)
            assert(res[i] == exp[i]);
    }

    static void test(T, string op, size_t N = 16)(T a, T b, T exp)
    {
        T[N] va = a, vb = b, vexp = exp;

        check!op(va[], vb[], vexp);
        check!op(va[], b, vexp);
        check!op(a, vb[], vexp);
    }

    static void test2(T, string unaOp, string binOp, size_t N = 16)(T a, T b, T exp)
    {
        T[N] va = a, vb = b, vexp = exp;

        check2!(unaOp, binOp)(va[], vb[], vexp);
        check2!(unaOp, binOp)(va[], b, vexp);
        check2!(unaOp, binOp)(a, vb[], vexp);
    }

    alias UINTS = TT!(ubyte, ushort, uint, ulong);
    alias INTS = TT!(byte, short, int, long);
    alias FLOATS = TT!(float, double);

    foreach (T; TT!(UINTS, INTS, FLOATS))
    {
        test!(T, "+")(1, 2, 3);
        test!(T, "-")(3, 2, 1);
        static if (__traits(compiles, { import std.math; }))
            test!(T, "^^")(2, 3, 8);

        test2!(T, "u-", "+")(3, 2, 1);
    }

    foreach (T; TT!(UINTS, INTS))
    {
        test!(T, "|")(1, 2, 3);
        test!(T, "&")(3, 1, 1);
        test!(T, "^")(3, 1, 2);

        test2!(T, "u~", "+")(3, cast(T)~2, 5);
    }

    foreach (T; TT!(INTS, FLOATS))
    {
        test!(T, "-")(1, 2, -1);
        test2!(T, "u-", "+")(-3, -2, -1);
        test2!(T, "u-", "*")(-3, -2, -6);
    }

    foreach (T; TT!(UINTS, INTS, FLOATS))
    {
        test!(T, "*")(2, 3, 6);
        test!(T, "/")(8, 4, 2);
        test!(T, "%")(8, 6, 2);
    }
}

// test handling of v op= exp
unittest
{
    uint[32] c;
    arrayOp!(uint[], uint, "+=")(c[], 2);
    foreach (v; c)
        assert(v == 2);
    static if (__traits(compiles, { import std.math; }))
    {
        arrayOp!(uint[], uint, "^^=")(c[], 3);
        foreach (v; c)
            assert(v == 8);
    }
}

// proper error message for UDT lacking certain ops
unittest
{
    static assert(!is(typeof(&arrayOp!(int[4][], int[4], "+="))));
    static assert(!is(typeof(&arrayOp!(int[4][], int[4], "u-", "="))));

    static struct S
    {
    }

    static assert(!is(typeof(&arrayOp!(S[], S, "+="))));
    static assert(!is(typeof(&arrayOp!(S[], S[], "*", S, "+="))));
    static struct S2
    {
        S2 opBinary(string op)(in S2) @nogc pure nothrow
        {
            return this;
        }

        ref S2 opOpAssign(string op)(in S2) @nogc pure nothrow
        {
            return this;
        }
    }

    static assert(is(typeof(&arrayOp!(S2[], S2[], S2[], S2, "*", "+", "="))));
    static assert(is(typeof(&arrayOp!(S2[], S2[], S2, "*", "+="))));
}

// Written in the D programming language.

/** This module contains the $(LREF Complex) type, which is used to represent
    complex numbers, along with related mathematical operations and functions.

    $(LREF Complex) will eventually
    $(DDLINK deprecate, Deprecated Features, replace)
    the built-in types `cfloat`, `cdouble`, `creal`, `ifloat`,
    `idouble`, and `ireal`.

    Macros:
        TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
                <caption>Special Values</caption>
                $0</table>
        PLUSMN = &plusmn;
        NAN = $(RED NAN)
        INFIN = &infin;
        PI = &pi;

    Authors:    Lars Tandle Kyllingstad, Don Clugston
    Copyright:  Copyright (c) 2010, Lars T. Kyllingstad.
    License:    $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Source:     $(PHOBOSSRC std/complex.d)
*/
module std.complex;

import std.traits;

/** Helper function that returns a complex number with the specified
    real and imaginary parts.

    Params:
        R = (template parameter) type of real part of complex number
        I = (template parameter) type of imaginary part of complex number

        re = real part of complex number to be constructed
        im = (optional) imaginary part of complex number, 0 if omitted.

    Returns:
        `Complex` instance with real and imaginary parts set
        to the values provided as input.  If neither `re` nor
        `im` are floating-point numbers, the return type will
        be `Complex!double`.  Otherwise, the return type is
        deduced using $(D std.traits.CommonType!(R, I)).
*/
auto complex(R)(const R re)  @safe pure nothrow @nogc
if (is(R : double))
{
    static if (isFloatingPoint!R)
        return Complex!R(re, 0);
    else
        return Complex!double(re, 0);
}

/// ditto
auto complex(R, I)(const R re, const I im)  @safe pure nothrow @nogc
if (is(R : double) && is(I : double))
{
    static if (isFloatingPoint!R || isFloatingPoint!I)
        return Complex!(CommonType!(R, I))(re, im);
    else
        return Complex!double(re, im);
}

///
@safe pure nothrow unittest
{
    auto a = complex(1.0);
    static assert(is(typeof(a) == Complex!double));
    assert(a.re == 1.0);
    assert(a.im == 0.0);

    auto b = complex(2.0L);
    static assert(is(typeof(b) == Complex!real));
    assert(b.re == 2.0L);
    assert(b.im == 0.0L);

    auto c = complex(1.0, 2.0);
    static assert(is(typeof(c) == Complex!double));
    assert(c.re == 1.0);
    assert(c.im == 2.0);

    auto d = complex(3.0, 4.0L);
    static assert(is(typeof(d) == Complex!real));
    assert(d.re == 3.0);
    assert(d.im == 4.0L);

    auto e = complex(1);
    static assert(is(typeof(e) == Complex!double));
    assert(e.re == 1);
    assert(e.im == 0);

    auto f = complex(1L, 2);
    static assert(is(typeof(f) == Complex!double));
    assert(f.re == 1L);
    assert(f.im == 2);

    auto g = complex(3, 4.0L);
    static assert(is(typeof(g) == Complex!real));
    assert(g.re == 3);
    assert(g.im == 4.0L);
}


/** A complex number parametrised by a type `T`, which must be either
    `float`, `double` or `real`.
*/
struct Complex(T)
if (isFloatingPoint!T)
{
    import std.format.spec : FormatSpec;
    import std.range.primitives : isOutputRange;

    /** The real part of the number. */
    T re;

    /** The imaginary part of the number. */
    T im;

    /** Converts the complex number to a string representation.

    The second form of this function is usually not called directly;
    instead, it is used via $(REF format, std,string), as shown in the examples
    below.  Supported format characters are 'e', 'f', 'g', 'a', and 's'.

    See the $(MREF std, format) and $(REF format, std,string)
    documentation for more information.
    */
    string toString() const @safe /* TODO: pure nothrow */
    {
        import std.exception : assumeUnique;
        char[] buf;
        buf.reserve(100);
        auto fmt = FormatSpec!char("%s");
        toString((const(char)[] s) { buf ~= s; }, fmt);
        static trustedAssumeUnique(T)(T t) @trusted { return assumeUnique(t); }
        return trustedAssumeUnique(buf);
    }

    static if (is(T == double))
    ///
    @safe unittest
    {
        auto c = complex(1.2, 3.4);

        // Vanilla toString formatting:
        assert(c.toString() == "1.2+3.4i");

        // Formatting with std.string.format specs: the precision and width
        // specifiers apply to both the real and imaginary parts of the
        // complex number.
        import std.format : format;
        assert(format("%.2f", c)  == "1.20+3.40i");
        assert(format("%4.1f", c) == " 1.2+ 3.4i");
    }

    /// ditto
    void toString(Writer, Char)(scope Writer w, scope const ref FormatSpec!Char formatSpec) const
        if (isOutputRange!(Writer, const(Char)[]))
    {
        import std.format.write : formatValue;
        import std.math.traits : signbit;
        import std.range.primitives : put;
        formatValue(w, re, formatSpec);
        if (signbit(im) == 0)
           put(w, "+");
        formatValue(w, im, formatSpec);
        put(w, "i");
    }

@safe pure nothrow @nogc:

    /** Construct a complex number with the specified real and
    imaginary parts. In the case where a single argument is passed
    that is not complex, the imaginary part of the result will be
    zero.
    */
    this(R : T)(Complex!R z)
    {
        re = z.re;
        im = z.im;
    }

    /// ditto
    this(Rx : T, Ry : T)(const Rx x, const Ry y)
    {
        re = x;
        im = y;
    }

    /// ditto
    this(R : T)(const R r)
    {
        re = r;
        im = 0;
    }

    // ASSIGNMENT OPERATORS

    // this = complex
    ref Complex opAssign(R : T)(Complex!R z)
    {
        re = z.re;
        im = z.im;
        return this;
    }

    // this = numeric
    ref Complex opAssign(R : T)(const R r)
    {
        re = r;
        im = 0;
        return this;
    }

    // COMPARISON OPERATORS

    // this == complex
    bool opEquals(R : T)(Complex!R z) const
    {
        return re == z.re && im == z.im;
    }

    // this == numeric
    bool opEquals(R : T)(const R r) const
    {
        return re == r && im == 0;
    }

    // UNARY OPERATORS

    // +complex
    Complex opUnary(string op)() const
        if (op == "+")
    {
        return this;
    }

    // -complex
    Complex opUnary(string op)() const
        if (op == "-")
    {
        return Complex(-re, -im);
    }

    // BINARY OPERATORS

    // complex op complex
    Complex!(CommonType!(T,R)) opBinary(string op, R)(Complex!R z) const
    {
        alias C = typeof(return);
        auto w = C(this.re, this.im);
        return w.opOpAssign!(op)(z);
    }

    // complex op numeric
    Complex!(CommonType!(T,R)) opBinary(string op, R)(const R r) const
        if (isNumeric!R)
    {
        alias C = typeof(return);
        auto w = C(this.re, this.im);
        return w.opOpAssign!(op)(r);
    }

    // numeric + complex,  numeric * complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
        if ((op == "+" || op == "*") && (isNumeric!R))
    {
        return opBinary!(op)(r);
    }

    // numeric - complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
        if (op == "-" && isNumeric!R)
    {
        return Complex(r - re, -im);
    }

    // numeric / complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
        if (op == "/" && isNumeric!R)
    {
        version (FastMath)
        {
            // Compute norm(this)
            immutable norm = re * re + im * im;
            // Compute r * conj(this)
            immutable prod_re = r * re;
            immutable prod_im = r * -im;
            // Divide the product by the norm
            typeof(return) w = void;
            w.re = prod_re / norm;
            w.im = prod_im / norm;
            return w;
        }
        else
        {
            import core.math : fabs;
            typeof(return) w = void;
            if (fabs(re) < fabs(im))
            {
                immutable ratio = re/im;
                immutable rdivd = r/(re*ratio + im);

                w.re = rdivd*ratio;
                w.im = -rdivd;
            }
            else
            {
                immutable ratio = im/re;
                immutable rdivd = r/(re + im*ratio);

                w.re = rdivd;
                w.im = -rdivd*ratio;
            }

            return w;
        }
    }

    // numeric ^^ complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(const R lhs) const
        if (op == "^^" && isNumeric!R)
    {
        import core.math : cos, sin;
        import std.math.exponential : exp, log;
        import std.math.constants : PI;
        Unqual!(CommonType!(T, R)) ab = void, ar = void;

        if (lhs >= 0)
        {
            // r = lhs
            // theta = 0
            ab = lhs ^^ this.re;
            ar = log(lhs) * this.im;
        }
        else
        {
            // r = -lhs
            // theta = PI
            ab = (-lhs) ^^ this.re * exp(-PI * this.im);
            ar = PI * this.re + log(-lhs) * this.im;
        }

        return typeof(return)(ab * cos(ar), ab * sin(ar));
    }

    // OP-ASSIGN OPERATORS

    // complex += complex,  complex -= complex
    ref Complex opOpAssign(string op, C)(const C z)
        if ((op == "+" || op == "-") && is(C R == Complex!R))
    {
        mixin ("re "~op~"= z.re;");
        mixin ("im "~op~"= z.im;");
        return this;
    }

    // complex *= complex
    ref Complex opOpAssign(string op, C)(const C z)
        if (op == "*" && is(C R == Complex!R))
    {
        auto temp = re*z.re - im*z.im;
        im = im*z.re + re*z.im;
        re = temp;
        return this;
    }

    // complex /= complex
    ref Complex opOpAssign(string op, C)(const C z)
        if (op == "/" && is(C R == Complex!R))
    {
        version (FastMath)
        {
            // Compute norm(z)
            immutable norm = z.re * z.re + z.im * z.im;
            // Compute this * conj(z)
            immutable prod_re = re * z.re - im * -z.im;
            immutable prod_im = im * z.re + re * -z.im;
            // Divide the product by the norm
            re = prod_re / norm;
            im = prod_im / norm;
            return this;
        }
        else
        {
            import core.math : fabs;
            if (fabs(z.re) < fabs(z.im))
            {
                immutable ratio = z.re/z.im;
                immutable denom = z.re*ratio + z.im;

                immutable temp = (re*ratio + im)/denom;
                im = (im*ratio - re)/denom;
                re = temp;
            }
            else
            {
                immutable ratio = z.im/z.re;
                immutable denom = z.re + z.im*ratio;

                immutable temp = (re + im*ratio)/denom;
                im = (im - re*ratio)/denom;
                re = temp;
            }
            return this;
        }
    }

    // complex ^^= complex
    ref Complex opOpAssign(string op, C)(const C z)
        if (op == "^^" && is(C R == Complex!R))
    {
        import core.math : cos, sin;
        import std.math.exponential : exp, log;
        immutable r = abs(this);
        immutable t = arg(this);
        immutable ab = r^^z.re * exp(-t*z.im);
        immutable ar = t*z.re + log(r)*z.im;

        re = ab*cos(ar);
        im = ab*sin(ar);
        return this;
    }

    // complex += numeric,  complex -= numeric
    ref Complex opOpAssign(string op, U : T)(const U a)
        if (op == "+" || op == "-")
    {
        mixin ("re "~op~"= a;");
        return this;
    }

    // complex *= numeric,  complex /= numeric
    ref Complex opOpAssign(string op, U : T)(const U a)
        if (op == "*" || op == "/")
    {
        mixin ("re "~op~"= a;");
        mixin ("im "~op~"= a;");
        return this;
    }

    // complex ^^= real
    ref Complex opOpAssign(string op, R)(const R r)
        if (op == "^^" && isFloatingPoint!R)
    {
        import core.math : cos, sin;
        immutable ab = abs(this)^^r;
        immutable ar = arg(this)*r;
        re = ab*cos(ar);
        im = ab*sin(ar);
        return this;
    }

    // complex ^^= int
    ref Complex opOpAssign(string op, U)(const U i)
        if (op == "^^" && isIntegral!U)
    {
        switch (i)
        {
        case 0:
            re = 1.0;
            im = 0.0;
            break;
        case 1:
            // identity; do nothing
            break;
        case 2:
            this *= this;
            break;
        case 3:
            auto z = this;
            this *= z;
            this *= z;
            break;
        default:
            this ^^= cast(real) i;
        }
        return this;
    }

    /** Returns a complex number instance that correponds in size and in ABI
        to the associated C compiler's `_Complex` type.
     */
    auto toNative()
    {
        import core.stdc.config : c_complex_float, c_complex_double, c_complex_real;
        static if (is(T == float))
            return c_complex_float(re, im);
        else static if (is(T == double))
            return c_complex_double(re, im);
        else
            return c_complex_real(re, im);
    }
}

@safe pure nothrow unittest
{
    import std.complex;
    static import core.math;
    import std.math;

    enum EPS = double.epsilon;
    auto c1 = complex(1.0, 1.0);

    // Check unary operations.
    auto c2 = Complex!double(0.5, 2.0);

    assert(c2 == +c2);

    assert((-c2).re == -(c2.re));
    assert((-c2).im == -(c2.im));
    assert(c2 == -(-c2));

    // Check complex-complex operations.
    auto cpc = c1 + c2;
    assert(cpc.re == c1.re + c2.re);
    assert(cpc.im == c1.im + c2.im);

    auto cmc = c1 - c2;
    assert(cmc.re == c1.re - c2.re);
    assert(cmc.im == c1.im - c2.im);

    auto ctc = c1 * c2;
    assert(isClose(abs(ctc), abs(c1)*abs(c2), EPS));
    assert(isClose(arg(ctc), arg(c1)+arg(c2), EPS));

    auto cdc = c1 / c2;
    assert(isClose(abs(cdc), abs(c1)/abs(c2), EPS));
    assert(isClose(arg(cdc), arg(c1)-arg(c2), EPS));

    auto cec = c1^^c2;
    assert(isClose(cec.re, 0.1152413197994, 1e-12));
    assert(isClose(cec.im, 0.2187079045274, 1e-12));

    // Check complex-real operations.
    double a = 123.456;

    auto cpr = c1 + a;
    assert(cpr.re == c1.re + a);
    assert(cpr.im == c1.im);

    auto cmr = c1 - a;
    assert(cmr.re == c1.re - a);
    assert(cmr.im == c1.im);

    auto ctr = c1 * a;
    assert(ctr.re == c1.re*a);
    assert(ctr.im == c1.im*a);

    auto cdr = c1 / a;
    assert(isClose(abs(cdr), abs(c1)/a, EPS));
    assert(isClose(arg(cdr), arg(c1), EPS));

    auto cer = c1^^3.0;
    assert(isClose(abs(cer), abs(c1)^^3, EPS));
    assert(isClose(arg(cer), arg(c1)*3, EPS));

    auto rpc = a + c1;
    assert(rpc == cpr);

    auto rmc = a - c1;
    assert(rmc.re == a-c1.re);
    assert(rmc.im == -c1.im);

    auto rtc = a * c1;
    assert(rtc == ctr);

    auto rdc = a / c1;
    assert(isClose(abs(rdc), a/abs(c1), EPS));
    assert(isClose(arg(rdc), -arg(c1), EPS));

    rdc = a / c2;
    assert(isClose(abs(rdc), a/abs(c2), EPS));
    assert(isClose(arg(rdc), -arg(c2), EPS));

    auto rec1a = 1.0 ^^ c1;
    assert(rec1a.re == 1.0);
    assert(rec1a.im == 0.0);

    auto rec2a = 1.0 ^^ c2;
    assert(rec2a.re == 1.0);
    assert(rec2a.im == 0.0);

    auto rec1b = (-1.0) ^^ c1;
    assert(isClose(abs(rec1b), std.math.exp(-PI * c1.im), EPS));
    auto arg1b = arg(rec1b);
    /* The argument _should_ be PI, but floating-point rounding error
     * means that in fact the imaginary part is very slightly negative.
     */
    assert(isClose(arg1b, PI, EPS) || isClose(arg1b, -PI, EPS));

    auto rec2b = (-1.0) ^^ c2;
    assert(isClose(abs(rec2b), std.math.exp(-2 * PI), EPS));
    assert(isClose(arg(rec2b), PI_2, EPS));

    auto rec3a = 0.79 ^^ complex(6.8, 5.7);
    auto rec3b = complex(0.79, 0.0) ^^ complex(6.8, 5.7);
    assert(isClose(rec3a.re, rec3b.re, 1e-14));
    assert(isClose(rec3a.im, rec3b.im, 1e-14));

    auto rec4a = (-0.79) ^^ complex(6.8, 5.7);
    auto rec4b = complex(-0.79, 0.0) ^^ complex(6.8, 5.7);
    assert(isClose(rec4a.re, rec4b.re, 1e-14));
    assert(isClose(rec4a.im, rec4b.im, 1e-14));

    auto rer = a ^^ complex(2.0, 0.0);
    auto rcheck = a ^^ 2.0;
    static assert(is(typeof(rcheck) == double));
    assert(feqrel(rer.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer.re, rcheck));
    assert(rer.im == 0.0);

    auto rer2 = (-a) ^^ complex(2.0, 0.0);
    rcheck = (-a) ^^ 2.0;
    assert(feqrel(rer2.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer2.re, rcheck));
    assert(isClose(rer2.im, 0.0, 0.0, 1e-10));

    auto rer3 = (-a) ^^ complex(-2.0, 0.0);
    rcheck = (-a) ^^ (-2.0);
    assert(feqrel(rer3.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer3.re, rcheck));
    assert(isClose(rer3.im, 0.0, 0.0, EPS));

    auto rer4 = a ^^ complex(-2.0, 0.0);
    rcheck = a ^^ (-2.0);
    assert(feqrel(rer4.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer4.re, rcheck));
    assert(rer4.im == 0.0);

    // Check Complex-int operations.
    foreach (i; 0 .. 6)
    {
        auto cei = c1^^i;
        assert(isClose(abs(cei), abs(c1)^^i, 1e-14));
        // Use cos() here to deal with arguments that go outside
        // the (-pi,pi] interval (only an issue for i>3).
        assert(isClose(core.math.cos(arg(cei)), core.math.cos(arg(c1)*i), 1e-14));
    }

    // Check operations between different complex types.
    auto cf = Complex!float(1.0, 1.0);
    auto cr = Complex!real(1.0, 1.0);
    auto c1pcf = c1 + cf;
    auto c1pcr = c1 + cr;
    static assert(is(typeof(c1pcf) == Complex!double));
    static assert(is(typeof(c1pcr) == Complex!real));
    assert(c1pcf.re == c1pcr.re);
    assert(c1pcf.im == c1pcr.im);

    auto c1c = c1;
    auto c2c = c2;

    c1c /= c1;
    assert(isClose(c1c.re, 1.0, EPS));
    assert(isClose(c1c.im, 0.0, 0.0, EPS));

    c1c = c1;
    c1c /= c2;
    assert(isClose(c1c.re, 0.5882352941177, 1e-12));
    assert(isClose(c1c.im, -0.3529411764706, 1e-12));

    c2c /= c1;
    assert(isClose(c2c.re, 1.25, EPS));
    assert(isClose(c2c.im, 0.75, EPS));

    c2c = c2;
    c2c /= c2;
    assert(isClose(c2c.re, 1.0, EPS));
    assert(isClose(c2c.im, 0.0, 0.0, EPS));
}

@safe pure nothrow unittest
{
    // Initialization
    Complex!double a = 1;
    assert(a.re == 1 && a.im == 0);
    Complex!double b = 1.0;
    assert(b.re == 1.0 && b.im == 0);
    Complex!double c = Complex!real(1.0, 2);
    assert(c.re == 1.0 && c.im == 2);
}

@safe pure nothrow unittest
{
    // Assignments and comparisons
    Complex!double z;

    z = 1;
    assert(z == 1);
    assert(z.re == 1.0  &&  z.im == 0.0);

    z = 2.0;
    assert(z == 2.0);
    assert(z.re == 2.0  &&  z.im == 0.0);

    z = 1.0L;
    assert(z == 1.0L);
    assert(z.re == 1.0  &&  z.im == 0.0);

    auto w = Complex!real(1.0, 1.0);
    z = w;
    assert(z == w);
    assert(z.re == 1.0  &&  z.im == 1.0);

    auto c = Complex!float(2.0, 2.0);
    z = c;
    assert(z == c);
    assert(z.re == 2.0  &&  z.im == 2.0);
}


/*  Makes Complex!(Complex!T) fold to Complex!T.

    The rationale for this is that just like the real line is a
    subspace of the complex plane, the complex plane is a subspace
    of itself.  Example of usage:
    ---
    Complex!T addI(T)(T x)
    {
        return x + Complex!T(0.0, 1.0);
    }
    ---
    The above will work if T is both real and complex.
*/
template Complex(T)
if (is(T R == Complex!R))
{
    alias Complex = T;
}

@safe pure nothrow unittest
{
    static assert(is(Complex!(Complex!real) == Complex!real));

    Complex!T addI(T)(T x)
    {
        return x + Complex!T(0.0, 1.0);
    }

    auto z1 = addI(1.0);
    assert(z1.re == 1.0 && z1.im == 1.0);

    enum one = Complex!double(1.0, 0.0);
    auto z2 = addI(one);
    assert(z1 == z2);
}


/**
   Params: z = A complex number.
   Returns: The absolute value (or modulus) of `z`.
*/
T abs(T)(Complex!T z) @safe pure nothrow @nogc
{
    import std.math.algebraic : hypot;
    return hypot(z.re, z.im);
}

///
@safe pure nothrow unittest
{
    static import core.math;
    assert(abs(complex(1.0)) == 1.0);
    assert(abs(complex(0.0, 1.0)) == 1.0);
    assert(abs(complex(1.0L, -2.0L)) == core.math.sqrt(5.0L));
}

@safe pure nothrow @nogc unittest
{
    static import core.math;
    assert(abs(complex(0.0L, -3.2L)) == 3.2L);
    assert(abs(complex(0.0L, 71.6L)) == 71.6L);
    assert(abs(complex(-1.0L, 1.0L)) == core.math.sqrt(2.0L));
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
        static import std.math;
        Complex!T a = complex(T(-12), T(3));
        T b = std.math.hypot(a.re, a.im);
        assert(std.math.isClose(abs(a), b));
        assert(std.math.isClose(abs(-a), b));
    }}
}

/++
   Params:
    z = A complex number.
    x = A real number.
   Returns: The squared modulus of `z`.
   For genericity, if called on a real number, returns its square.
+/
T sqAbs(T)(Complex!T z) @safe pure nothrow @nogc
{
    return z.re*z.re + z.im*z.im;
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    assert(sqAbs(complex(0.0)) == 0.0);
    assert(sqAbs(complex(1.0)) == 1.0);
    assert(sqAbs(complex(0.0, 1.0)) == 1.0);
    assert(isClose(sqAbs(complex(1.0L, -2.0L)), 5.0L));
    assert(isClose(sqAbs(complex(-3.0L, 1.0L)), 10.0L));
    assert(isClose(sqAbs(complex(1.0f,-1.0f)), 2.0f));
}

/// ditto
T sqAbs(T)(const T x) @safe pure nothrow @nogc
if (isFloatingPoint!T)
{
    return x*x;
}

@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    assert(sqAbs(0.0) == 0.0);
    assert(sqAbs(-1.0) == 1.0);
    assert(isClose(sqAbs(-3.0L), 9.0L));
    assert(isClose(sqAbs(-5.0f), 25.0f));
}


/**
 Params: z = A complex number.
 Returns: The argument (or phase) of `z`.
 */
T arg(T)(Complex!T z) @safe pure nothrow @nogc
{
    import std.math.trigonometry : atan2;
    return atan2(z.im, z.re);
}

///
@safe pure nothrow unittest
{
    import std.math.constants : PI_2, PI_4;
    assert(arg(complex(1.0)) == 0.0);
    assert(arg(complex(0.0L, 1.0L)) == PI_2);
    assert(arg(complex(1.0L, 1.0L)) == PI_4);
}


/**
 * Extracts the norm of a complex number.
 * Params:
 *      z = A complex number
 * Returns:
 *      The squared magnitude of `z`.
 */
T norm(T)(Complex!T z) @safe pure nothrow @nogc
{
    return z.re * z.re + z.im * z.im;
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(norm(complex(3.0, 4.0)) == 25.0);
    assert(norm(fromPolar(5.0, 0.0)) == 25.0);
    assert(isClose(norm(fromPolar(5.0L, PI / 6)), 25.0L));
    assert(isClose(norm(fromPolar(5.0L, 13 * PI / 6)), 25.0L));
}


/**
  Params: z = A complex number.
  Returns: The complex conjugate of `z`.
*/
Complex!T conj(T)(Complex!T z) @safe pure nothrow @nogc
{
    return Complex!T(z.re, -z.im);
}

///
@safe pure nothrow unittest
{
    assert(conj(complex(1.0)) == complex(1.0));
    assert(conj(complex(1.0, 2.0)) == complex(1.0, -2.0));
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
         auto c = Complex!T(7, 3L);
         assert(conj(c) == Complex!T(7, -3L));
         auto z = Complex!T(0, -3.2L);
         assert(conj(z) == -z);
    }}
}

/**
 * Returns the projection of `z` onto the Riemann sphere.
 * Params:
 *      z = A complex number
 * Returns:
 *      The projection of `z` onto the Riemann sphere.
 */
Complex!T proj(T)(Complex!T z)
{
    static import std.math;

    if (std.math.isInfinity(z.re) || std.math.isInfinity(z.im))
        return Complex!T(T.infinity, std.math.copysign(0.0, z.im));

    return z;
}

///
@safe pure nothrow unittest
{
    assert(proj(complex(1.0)) == complex(1.0));
    assert(proj(complex(double.infinity, 5.0)) == complex(double.infinity, 0.0));
    assert(proj(complex(5.0, -double.infinity)) == complex(double.infinity, -0.0));
}


/**
  Constructs a complex number given its absolute value and argument.
  Params:
    modulus = The modulus
    argument = The argument
  Returns: The complex number with the given modulus and argument.
*/
Complex!(CommonType!(T, U)) fromPolar(T, U)(const T modulus, const U argument)
    @safe pure nothrow @nogc
{
    import core.math : sin, cos;
    return Complex!(CommonType!(T,U))
        (modulus*cos(argument), modulus*sin(argument));
}

///
@safe pure nothrow unittest
{
    import core.math;
    import std.math.operations : isClose;
    import std.math.algebraic : sqrt;
    import std.math.constants : PI_4;
    auto z = fromPolar(core.math.sqrt(2.0L), PI_4);
    assert(isClose(z.re, 1.0L));
    assert(isClose(z.im, 1.0L));
}

version (StdUnittest)
{
    // Helper function for comparing two Complex numbers.
    int ceqrel(T)(const Complex!T x, const Complex!T y) @safe pure nothrow @nogc
    {
        import std.math.operations : feqrel;
        const r = feqrel(x.re, y.re);
        const i = feqrel(x.im, y.im);
        return r < i ? r : i;
    }
}

/**
    Trigonometric functions on complex numbers.

    Params: z = A complex number.
    Returns: The sine, cosine and tangent of `z`, respectively.
*/
Complex!T sin(T)(Complex!T z)  @safe pure nothrow @nogc
{
    auto cs = expi(z.re);
    auto csh = coshisinh(z.im);
    return typeof(return)(cs.im * csh.re, cs.re * csh.im);
}

///
@safe pure nothrow unittest
{
    static import core.math;
    assert(sin(complex(0.0)) == 0.0);
    assert(sin(complex(2.0, 0)) == core.math.sin(2.0));
}

@safe pure nothrow unittest
{
    static import core.math;
    assert(ceqrel(sin(complex(2.0L, 0)), complex(core.math.sin(2.0L))) >= real.mant_dig - 1);
}

/// ditto
Complex!T cos(T)(Complex!T z)  @safe pure nothrow @nogc
{
    auto cs = expi(z.re);
    auto csh = coshisinh(z.im);
    return typeof(return)(cs.re * csh.re, - cs.im * csh.im);
}

///
@safe pure nothrow unittest
{
    static import core.math;
    static import std.math;
    assert(cos(complex(0.0)) == 1.0);
    assert(cos(complex(1.3, 0.0)) == core.math.cos(1.3));
    assert(cos(complex(0.0, 5.2)) == std.math.cosh(5.2));
}

@safe pure nothrow unittest
{
    static import core.math;
    static import std.math;
    assert(ceqrel(cos(complex(0, 5.2L)), complex(std.math.cosh(5.2L), 0.0L)) >= real.mant_dig - 1);
    assert(ceqrel(cos(complex(1.3L)), complex(core.math.cos(1.3L))) >= real.mant_dig - 1);
}

/// ditto
Complex!T tan(T)(Complex!T z) @safe pure nothrow @nogc
{
    return sin(z) / cos(z);
}

///
@safe pure nothrow @nogc unittest
{
    static import std.math;

    int ceqrel(T)(const Complex!T x, const Complex!T y) @safe pure nothrow @nogc
    {
        import std.math.operations : feqrel;
        const r = feqrel(x.re, y.re);
        const i = feqrel(x.im, y.im);
        return r < i ? r : i;
    }
    assert(ceqrel(tan(complex(1.0, 0.0)), complex(std.math.tan(1.0), 0.0)) >= double.mant_dig - 2);
    assert(ceqrel(tan(complex(0.0, 1.0)), complex(0.0, std.math.tanh(1.0))) >= double.mant_dig - 2);
}

/**
    Inverse trigonometric functions on complex numbers.

    Params: z = A complex number.
    Returns: The arcsine, arccosine and arctangent of `z`, respectively.
*/
Complex!T asin(T)(Complex!T z)  @safe pure nothrow @nogc
{
    auto ash = asinh(Complex!T(-z.im, z.re));
    return Complex!T(ash.im, -ash.re);
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(asin(complex(0.0)) == 0.0);
    assert(isClose(asin(complex(0.5L)), PI / 6));
}

@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    version (DigitalMars) {} else // Disabled because of https://issues.dlang.org/show_bug.cgi?id=21376
    assert(isClose(asin(complex(0.5f)), float(PI) / 6));
}

/// ditto
Complex!T acos(T)(Complex!T z)  @safe pure nothrow @nogc
{
    static import std.math;
    auto as = asin(z);
    return Complex!T(T(std.math.PI_2) - as.re, as.im);
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    import std.math.trigonometry : std_math_acos = acos;
    assert(acos(complex(0.0)) == std_math_acos(0.0));
    assert(isClose(acos(complex(0.5L)), PI / 3));
}

@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    version (DigitalMars) {} else // Disabled because of https://issues.dlang.org/show_bug.cgi?id=21376
    assert(isClose(acos(complex(0.5f)), float(PI) / 3));
}

/// ditto
Complex!T atan(T)(Complex!T z) @safe pure nothrow @nogc
{
    static import std.math;
    const T re2 = z.re * z.re;
    const T x = 1 - re2 - z.im * z.im;

    T num = z.im + 1;
    T den = z.im - 1;

    num = re2 + num * num;
    den = re2 + den * den;

    return Complex!T(T(0.5) * std.math.atan2(2 * z.re, x),
                     T(0.25) * std.math.log(num / den));
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(atan(complex(0.0)) == 0.0);
    assert(isClose(atan(sqrt(complex(3.0L))), PI / 3));
    assert(isClose(atan(sqrt(complex(3.0f))), float(PI) / 3));
}

/**
    Hyperbolic trigonometric functions on complex numbers.

    Params: z = A complex number.
    Returns: The hyperbolic sine, cosine and tangent of `z`, respectively.
*/
Complex!T sinh(T)(Complex!T z)  @safe pure nothrow @nogc
{
    static import core.math, std.math;
    return Complex!T(std.math.sinh(z.re) * core.math.cos(z.im),
                     std.math.cosh(z.re) * core.math.sin(z.im));
}

///
@safe pure nothrow unittest
{
    static import std.math;
    assert(sinh(complex(0.0)) == 0.0);
    assert(sinh(complex(1.0L)) == std.math.sinh(1.0L));
    assert(sinh(complex(1.0f)) == std.math.sinh(1.0f));
}

/// ditto
Complex!T cosh(T)(Complex!T z)  @safe pure nothrow @nogc
{
    static import core.math, std.math;
    return Complex!T(std.math.cosh(z.re) * core.math.cos(z.im),
                     std.math.sinh(z.re) * core.math.sin(z.im));
}

///
@safe pure nothrow unittest
{
    static import std.math;
    assert(cosh(complex(0.0)) == 1.0);
    assert(cosh(complex(1.0L)) == std.math.cosh(1.0L));
    assert(cosh(complex(1.0f)) == std.math.cosh(1.0f));
}

/// ditto
Complex!T tanh(T)(Complex!T z) @safe pure nothrow @nogc
{
    return sinh(z) / cosh(z);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_tanh = tanh;
    assert(tanh(complex(0.0)) == 0.0);
    assert(isClose(tanh(complex(1.0L)), std_math_tanh(1.0L)));
    assert(isClose(tanh(complex(1.0f)), std_math_tanh(1.0f)));
}

/**
    Inverse hyperbolic trigonometric functions on complex numbers.

    Params: z = A complex number.
    Returns: The hyperbolic arcsine, arccosine and arctangent of `z`, respectively.
*/
Complex!T asinh(T)(Complex!T z)  @safe pure nothrow @nogc
{
    auto t = Complex!T((z.re - z.im) * (z.re + z.im) + 1, 2 * z.re * z.im);
    return log(sqrt(t) + z);
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_asinh = asinh;
    assert(asinh(complex(0.0)) == 0.0);
    assert(isClose(asinh(complex(1.0L)), std_math_asinh(1.0L)));
    assert(isClose(asinh(complex(1.0f)), std_math_asinh(1.0f)));
}

/// ditto
Complex!T acosh(T)(Complex!T z)  @safe pure nothrow @nogc
{
    return 2 * log(sqrt(T(0.5) * (z + 1)) + sqrt(T(0.5) * (z - 1)));
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_acosh = acosh;
    assert(acosh(complex(1.0)) == 0.0);
    assert(isClose(acosh(complex(3.0L)), std_math_acosh(3.0L)));
    assert(isClose(acosh(complex(3.0f)), std_math_acosh(3.0f)));
}

/// ditto
Complex!T atanh(T)(Complex!T z) @safe pure nothrow @nogc
{
    static import std.math;
    const T im2 = z.im * z.im;
    const T x = 1 - im2 - z.re * z.re;

    T num = 1 + z.re;
    T den = 1 - z.re;

    num = im2 + num * num;
    den = im2 + den * den;

    return Complex!T(T(0.25) * (std.math.log(num) - std.math.log(den)),
                     T(0.5) * std.math.atan2(2 * z.im, x));
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_atanh = atanh;
    assert(atanh(complex(0.0)) == 0.0);
    assert(isClose(atanh(complex(0.5L)), std_math_atanh(0.5L)));
    assert(isClose(atanh(complex(0.5f)), std_math_atanh(0.5f)));
}

/**
    Params: y = A real number.
    Returns: The value of cos(y) + i sin(y).

    Note:
    `expi` is included here for convenience and for easy migration of code.
*/
Complex!real expi(real y)  @trusted pure nothrow @nogc
{
    import core.math : cos, sin;
    return Complex!real(cos(y), sin(y));
}

///
@safe pure nothrow unittest
{
    import core.math : cos, sin;
    assert(expi(0.0L) == 1.0L);
    assert(expi(1.3e5L) == complex(cos(1.3e5L), sin(1.3e5L)));
}

/**
    Params: y = A real number.
    Returns: The value of cosh(y) + i sinh(y)

    Note:
    `coshisinh` is included here for convenience and for easy migration of code.
*/
Complex!real coshisinh(real y) @safe pure nothrow @nogc
{
    static import core.math;
    static import std.math;
    if (core.math.fabs(y) <= 0.5)
        return Complex!real(std.math.cosh(y), std.math.sinh(y));
    else
    {
        auto z = std.math.exp(y);
        auto zi = 0.5 / z;
        z = 0.5 * z;
        return Complex!real(z + zi, z - zi);
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.trigonometry : cosh, sinh;
    assert(coshisinh(3.0L) == complex(cosh(3.0L), sinh(3.0L)));
}

/**
    Params: z = A complex number.
    Returns: The square root of `z`.
*/
Complex!T sqrt(T)(Complex!T z)  @safe pure nothrow @nogc
{
    static import core.math;
    typeof(return) c;
    real x,y,w,r;

    if (z == 0)
    {
        c = typeof(return)(0, 0);
    }
    else
    {
        real z_re = z.re;
        real z_im = z.im;

        x = core.math.fabs(z_re);
        y = core.math.fabs(z_im);
        if (x >= y)
        {
            r = y / x;
            w = core.math.sqrt(x)
                * core.math.sqrt(0.5 * (1 + core.math.sqrt(1 + r * r)));
        }
        else
        {
            r = x / y;
            w = core.math.sqrt(y)
                * core.math.sqrt(0.5 * (r + core.math.sqrt(1 + r * r)));
        }

        if (z_re >= 0)
        {
            c = typeof(return)(w, z_im / (w + w));
        }
        else
        {
            if (z_im < 0)
                w = -w;
            c = typeof(return)(z_im / (w + w), w);
        }
    }
    return c;
}

///
@safe pure nothrow unittest
{
    static import core.math;
    assert(sqrt(complex(0.0)) == 0.0);
    assert(sqrt(complex(1.0L, 0)) == core.math.sqrt(1.0L));
    assert(sqrt(complex(-1.0L, 0)) == complex(0, 1.0L));
    assert(sqrt(complex(-8.0, -6.0)) == complex(1.0, -3.0));
}

@safe pure nothrow unittest
{
    import std.math.operations : isClose;

    auto c1 = complex(1.0, 1.0);
    auto c2 = Complex!double(0.5, 2.0);

    auto c1s = sqrt(c1);
    assert(isClose(c1s.re, 1.09868411347));
    assert(isClose(c1s.im, 0.455089860562));

    auto c2s = sqrt(c2);
    assert(isClose(c2s.re, 1.13171392428));
    assert(isClose(c2s.im, 0.883615530876));
}

// support %f formatting of complex numbers
// https://issues.dlang.org/show_bug.cgi?id=10881
@safe unittest
{
    import std.format : format;

    auto x = complex(1.2, 3.4);
    assert(format("%.2f", x) == "1.20+3.40i");

    auto y = complex(1.2, -3.4);
    assert(format("%.2f", y) == "1.20-3.40i");
}

@safe unittest
{
    // Test wide string formatting
    import std.format.write : formattedWrite;
    wstring wformat(T)(string format, Complex!T c)
    {
        import std.array : appender;
        auto w = appender!wstring();
        auto n = formattedWrite(w, format, c);
        return w.data;
    }

    auto x = complex(1.2, 3.4);
    assert(wformat("%.2f", x) == "1.20+3.40i"w);
}

@safe unittest
{
    // Test ease of use (vanilla toString() should be supported)
    assert(complex(1.2, 3.4).toString() == "1.2+3.4i");
}

@safe pure nothrow @nogc unittest
{
    auto c = complex(3.0L, 4.0L);
    c = sqrt(c);
    assert(c.re == 2.0L);
    assert(c.im == 1.0L);
}

/**
 * Calculates e$(SUPERSCRIPT x).
 * Params:
 *      x = A complex number
 * Returns:
 *      The complex base e exponential of `x`
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                           $(TH exp(x)))
 *      $(TR $(TD ($(PLUSMN)0, +0))            $(TD (1, +0)))
 *      $(TR $(TD (any, +$(INFIN)))            $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD (any, $(NAN))                $(TD ($(NAN), $(NAN)))))
 *      $(TR $(TD (+$(INFIN), +0))             $(TD (+$(INFIN), +0)))
 *      $(TR $(TD (-$(INFIN), any))            $(TD ($(PLUSMN)0, cis(x.im))))
 *      $(TR $(TD (+$(INFIN), any))            $(TD ($(PLUSMN)$(INFIN), cis(x.im))))
 *      $(TR $(TD (-$(INFIN), +$(INFIN)))      $(TD ($(PLUSMN)0, $(PLUSMN)0)))
 *      $(TR $(TD (+$(INFIN), +$(INFIN)))      $(TD ($(PLUSMN)$(INFIN), $(NAN))))
 *      $(TR $(TD (-$(INFIN), $(NAN)))         $(TD ($(PLUSMN)0, $(PLUSMN)0)))
 *      $(TR $(TD (+$(INFIN), $(NAN)))         $(TD ($(PLUSMN)$(INFIN), $(NAN))))
 *      $(TR $(TD ($(NAN), +0))                $(TD ($(NAN), +0)))
 *      $(TR $(TD ($(NAN), any))               $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD ($(NAN), $(NAN)))            $(TD ($(NAN), $(NAN))))
 *      )
 */
Complex!T exp(T)(Complex!T x) @trusted pure nothrow @nogc // TODO: @safe
{
    static import std.math;

    // Handle special cases explicitly here, as fromPolar will otherwise
    // cause them to return Complex!T(NaN, NaN), or with the wrong sign.
    if (std.math.isInfinity(x.re))
    {
        if (std.math.isNaN(x.im))
        {
            if (std.math.signbit(x.re))
                return Complex!T(0, std.math.copysign(0, x.im));
            else
                return x;
        }
        if (std.math.isInfinity(x.im))
        {
            if (std.math.signbit(x.re))
                return Complex!T(0, std.math.copysign(0, x.im));
            else
                return Complex!T(T.infinity, -T.nan);
        }
        if (x.im == 0.0)
        {
            if (std.math.signbit(x.re))
                return Complex!T(0.0);
            else
                return Complex!T(T.infinity);
        }
    }
    if (std.math.isNaN(x.re))
    {
        if (std.math.isNaN(x.im) || std.math.isInfinity(x.im))
            return Complex!T(T.nan, T.nan);
        if (x.im == 0.0)
            return x;
    }
    if (x.re == 0.0)
    {
        if (std.math.isNaN(x.im) || std.math.isInfinity(x.im))
            return Complex!T(T.nan, T.nan);
        if (x.im == 0.0)
            return Complex!T(1.0, 0.0);
    }

    return fromPolar!(T, T)(std.math.exp(x.re), x.im);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(exp(complex(0.0, 0.0)) == complex(1.0, 0.0));

    auto a = complex(2.0, 1.0);
    assert(exp(conj(a)) == conj(exp(a)));

    auto b = exp(complex(0.0L, 1.0L) * PI);
    assert(isClose(b, -1.0L, 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN, isInfinity;

    auto a = exp(complex(0.0, double.infinity));
    assert(a.re.isNaN && a.im.isNaN);
    auto b = exp(complex(0.0, double.infinity));
    assert(b.re.isNaN && b.im.isNaN);
    auto c = exp(complex(0.0, double.nan));
    assert(c.re.isNaN && c.im.isNaN);

    auto d = exp(complex(+double.infinity, 0.0));
    assert(d == complex(double.infinity, 0.0));
    auto e = exp(complex(-double.infinity, 0.0));
    assert(e == complex(0.0));
    auto f = exp(complex(-double.infinity, 1.0));
    assert(f == complex(0.0));
    auto g = exp(complex(+double.infinity, 1.0));
    assert(g == complex(double.infinity, double.infinity));
    auto h = exp(complex(-double.infinity, +double.infinity));
    assert(h == complex(0.0));
    auto i = exp(complex(+double.infinity, +double.infinity));
    assert(i.re.isInfinity && i.im.isNaN);
    auto j = exp(complex(-double.infinity, double.nan));
    assert(j == complex(0.0));
    auto k = exp(complex(+double.infinity, double.nan));
    assert(k.re.isInfinity && k.im.isNaN);

    auto l = exp(complex(double.nan, 0));
    assert(l.re.isNaN && l.im == 0.0);
    auto m = exp(complex(double.nan, 1));
    assert(m.re.isNaN && m.im.isNaN);
    auto n = exp(complex(double.nan, double.nan));
    assert(n.re.isNaN && n.im.isNaN);
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = exp(complex(0.0, -PI));
    assert(isClose(a, -1.0, 0.0, 1e-15));

    auto b = exp(complex(0.0, -2.0 * PI / 3.0));
    assert(isClose(b, complex(-0.5L, -0.866025403784438646763L)));

    auto c = exp(complex(0.0, PI / 3.0));
    assert(isClose(c, complex(0.5L, 0.866025403784438646763L)));

    auto d = exp(complex(0.0, 2.0 * PI / 3.0));
    assert(isClose(d, complex(-0.5L, 0.866025403784438646763L)));

    auto e = exp(complex(0.0, PI));
    assert(isClose(e, -1.0, 0.0, 1e-15));
}

/**
 * Calculate the natural logarithm of x.
 * The branch cut is along the negative axis.
 * Params:
 *      x = A complex number
 * Returns:
 *      The complex natural logarithm of `x`
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                           $(TH log(x)))
 *      $(TR $(TD (-0, +0))                    $(TD (-$(INFIN), $(PI))))
 *      $(TR $(TD (+0, +0))                    $(TD (-$(INFIN), +0)))
 *      $(TR $(TD (any, +$(INFIN)))            $(TD (+$(INFIN), $(PI)/2)))
 *      $(TR $(TD (any, $(NAN)))               $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD (-$(INFIN), any))            $(TD (+$(INFIN), $(PI))))
 *      $(TR $(TD (+$(INFIN), any))            $(TD (+$(INFIN), +0)))
 *      $(TR $(TD (-$(INFIN), +$(INFIN)))      $(TD (+$(INFIN), 3$(PI)/4)))
 *      $(TR $(TD (+$(INFIN), +$(INFIN)))      $(TD (+$(INFIN), $(PI)/4)))
 *      $(TR $(TD ($(PLUSMN)$(INFIN), $(NAN))) $(TD (+$(INFIN), $(NAN))))
 *      $(TR $(TD ($(NAN), any))               $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD ($(NAN), +$(INFIN)))         $(TD (+$(INFIN), $(NAN))))
 *      $(TR $(TD ($(NAN), $(NAN)))            $(TD ($(NAN), $(NAN))))
 *      )
 */
Complex!T log(T)(Complex!T x) @safe pure nothrow @nogc
{
    static import std.math;

    // Handle special cases explicitly here for better accuracy.
    // The order here is important, so that the correct path is chosen.
    if (std.math.isNaN(x.re))
    {
        if (std.math.isInfinity(x.im))
            return Complex!T(T.infinity, T.nan);
        else
            return Complex!T(T.nan, T.nan);
    }
    if (std.math.isInfinity(x.re))
    {
        if (std.math.isNaN(x.im))
            return Complex!T(T.infinity, T.nan);
        else if (std.math.isInfinity(x.im))
        {
            if (std.math.signbit(x.re))
                return Complex!T(T.infinity, std.math.copysign(3.0 * std.math.PI_4, x.im));
            else
                return Complex!T(T.infinity, std.math.copysign(std.math.PI_4, x.im));
        }
        else
        {
            if (std.math.signbit(x.re))
                return Complex!T(T.infinity, std.math.copysign(std.math.PI, x.im));
            else
                return Complex!T(T.infinity, std.math.copysign(0.0, x.im));
        }
    }
    if (std.math.isNaN(x.im))
        return Complex!T(T.nan, T.nan);
    if (std.math.isInfinity(x.im))
        return Complex!T(T.infinity, std.math.copysign(std.math.PI_2, x.im));
    if (x.re == 0.0 && x.im == 0.0)
    {
        if (std.math.signbit(x.re))
            return Complex!T(-T.infinity, std.math.copysign(std.math.PI, x.im));
        else
            return Complex!T(-T.infinity, std.math.copysign(0.0, x.im));
    }

    return Complex!T(std.math.log(abs(x)), arg(x));
}

///
@safe pure nothrow @nogc unittest
{
    import core.math : sqrt;
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = complex(2.0, 1.0);
    assert(log(conj(a)) == conj(log(a)));

    auto b = 2.0 * log10(complex(0.0, 1.0));
    auto c = 4.0 * log10(complex(sqrt(2.0) / 2, sqrt(2.0) / 2));
    assert(isClose(b, c, 0.0, 1e-15));

    assert(log(complex(-1.0L, 0.0L)) == complex(0.0L, PI));
    assert(log(complex(-1.0L, -0.0L)) == complex(0.0L, -PI));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN, isInfinity;
    import std.math.constants : PI, PI_2, PI_4;

    auto a = log(complex(-0.0L, 0.0L));
    assert(a == complex(-real.infinity, PI));
    auto b = log(complex(0.0L, 0.0L));
    assert(b == complex(-real.infinity, +0.0L));
    auto c = log(complex(1.0L, real.infinity));
    assert(c == complex(real.infinity, PI_2));
    auto d = log(complex(1.0L, real.nan));
    assert(d.re.isNaN && d.im.isNaN);

    auto e = log(complex(-real.infinity, 1.0L));
    assert(e == complex(real.infinity, PI));
    auto f = log(complex(real.infinity, 1.0L));
    assert(f == complex(real.infinity, 0.0L));
    auto g = log(complex(-real.infinity, real.infinity));
    assert(g == complex(real.infinity, 3.0 * PI_4));
    auto h = log(complex(real.infinity, real.infinity));
    assert(h == complex(real.infinity, PI_4));
    auto i = log(complex(real.infinity, real.nan));
    assert(i.re.isInfinity && i.im.isNaN);

    auto j = log(complex(real.nan, 1.0L));
    assert(j.re.isNaN && j.im.isNaN);
    auto k = log(complex(real.nan, real.infinity));
    assert(k.re.isInfinity && k.im.isNaN);
    auto l = log(complex(real.nan, real.nan));
    assert(l.re.isNaN && l.im.isNaN);
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = log(fromPolar(1.0, PI / 6.0));
    assert(isClose(a, complex(0.0L, 0.523598775598298873077L), 0.0, 1e-15));

    auto b = log(fromPolar(1.0, PI / 3.0));
    assert(isClose(b, complex(0.0L, 1.04719755119659774615L), 0.0, 1e-15));

    auto c = log(fromPolar(1.0, PI / 2.0));
    assert(isClose(c, complex(0.0L, 1.57079632679489661923L), 0.0, 1e-15));

    auto d = log(fromPolar(1.0, 2.0 * PI / 3.0));
    assert(isClose(d, complex(0.0L, 2.09439510239319549230L), 0.0, 1e-15));

    auto e = log(fromPolar(1.0, 5.0 * PI / 6.0));
    assert(isClose(e, complex(0.0L, 2.61799387799149436538L), 0.0, 1e-15));

    auto f = log(complex(-1.0L, 0.0L));
    assert(isClose(f, complex(0.0L, PI), 0.0, 1e-15));
}

/**
 * Calculate the base-10 logarithm of x.
 * Params:
 *      x = A complex number
 * Returns:
 *      The complex base 10 logarithm of `x`
 */
Complex!T log10(T)(Complex!T x) @safe pure nothrow @nogc
{
    import std.math.constants : LN10;

    return log(x) / Complex!T(LN10);
}

///
@safe pure nothrow @nogc unittest
{
    import core.math : sqrt;
    import std.math.constants : LN10, PI;
    import std.math.operations : isClose;

    auto a = complex(2.0, 1.0);
    assert(log10(a) == log(a) / log(complex(10.0)));

    auto b = log10(complex(0.0, 1.0)) * 2.0;
    auto c = log10(complex(sqrt(2.0) / 2, sqrt(2.0) / 2)) * 4.0;
    assert(isClose(b, c, 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : LN10, PI;
    import std.math.operations : isClose;

    auto a = log10(fromPolar(1.0, PI / 6.0));
    assert(isClose(a, complex(0.0L, 0.227396058973640224580L), 0.0, 1e-15));

    auto b = log10(fromPolar(1.0, PI / 3.0));
    assert(isClose(b, complex(0.0L, 0.454792117947280449161L), 0.0, 1e-15));

    auto c = log10(fromPolar(1.0, PI / 2.0));
    assert(isClose(c, complex(0.0L, 0.682188176920920673742L), 0.0, 1e-15));

    auto d = log10(fromPolar(1.0, 2.0 * PI / 3.0));
    assert(isClose(d, complex(0.0L, 0.909584235894560898323L), 0.0, 1e-15));

    auto e = log10(fromPolar(1.0, 5.0 * PI / 6.0));
    assert(isClose(e, complex(0.0L, 1.13698029486820112290L), 0.0, 1e-15));

    auto f = log10(complex(-1.0L, 0.0L));
    assert(isClose(f, complex(0.0L, 1.36437635384184134748L), 0.0, 1e-15));

    assert(ceqrel(log10(complex(-100.0L, 0.0L)), complex(2.0L, PI / LN10)) >= real.mant_dig - 1);
    assert(ceqrel(log10(complex(-100.0L, -0.0L)), complex(2.0L, -PI / LN10)) >= real.mant_dig - 1);
}

/**
 * Calculates x$(SUPERSCRIPT n).
 * The branch cut is on the negative axis.
 * Params:
 *      x = base
 *      n = exponent
 * Returns:
 *      `x` raised to the power of `n`
 */
Complex!T pow(T, Int)(Complex!T x, const Int n) @safe pure nothrow @nogc
if (isIntegral!Int)
{
    alias UInt = Unsigned!(Unqual!Int);

    UInt m = (n < 0) ? -cast(UInt) n : n;
    Complex!T y = (m % 2) ? x : Complex!T(1);

    while (m >>= 1)
    {
        x *= x;
        if (m % 2)
            y *= x;
    }

    return (n < 0) ? Complex!T(1) / y : y;
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;

    auto a = complex(1.0, 2.0);
    assert(pow(a, 2) == a * a);
    assert(pow(a, 3) == a * a * a);
    assert(pow(a, -2) == 1.0 / (a * a));
    assert(isClose(pow(a, -3), 1.0 / (a * a * a)));
}

/// ditto
Complex!T pow(T)(Complex!T x, const T n) @trusted pure nothrow @nogc
{
    static import std.math;

    if (x == 0.0)
        return Complex!T(0.0);

    if (x.im == 0 && x.re > 0.0)
        return Complex!T(std.math.pow(x.re, n));

    Complex!T t = log(x);
    return fromPolar!(T, T)(std.math.exp(n * t.re), n * t.im);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    assert(pow(complex(0.0), 2.0) == complex(0.0));
    assert(pow(complex(5.0), 2.0) == complex(25.0));

    auto a = pow(complex(-1.0, 0.0), 0.5);
    assert(isClose(a, complex(0.0, +1.0), 0.0, 1e-16));

    auto b = pow(complex(-1.0, -0.0), 0.5);
    assert(isClose(b, complex(0.0, -1.0), 0.0, 1e-16));
}

/// ditto
Complex!T pow(T)(Complex!T x, Complex!T y) @trusted pure nothrow @nogc
{
    return (x == 0) ? Complex!T(0) : exp(y * log(x));
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.exponential : exp;
    import std.math.constants : PI;
    auto a = complex(0.0);
    auto b = complex(2.0);
    assert(pow(a, b) == complex(0.0));

    auto c = complex(0.0L, 1.0L);
    assert(isClose(pow(c, c), exp((-PI) / 2)));
}

/// ditto
Complex!T pow(T)(const T x, Complex!T n) @trusted pure nothrow @nogc
{
    static import std.math;

    return (x > 0.0)
        ? fromPolar!(T, T)(std.math.pow(x, n.re), n.im * std.math.log(x))
        : pow(Complex!T(x), n);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    assert(pow(2.0, complex(0.0)) == complex(1.0));
    assert(pow(2.0, complex(5.0)) == complex(32.0));

    auto a = pow(-2.0, complex(-1.0));
    assert(isClose(a, complex(-0.5), 0.0, 1e-16));

    auto b = pow(-0.5, complex(-1.0));
    assert(isClose(b, complex(-2.0), 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = pow(complex(3.0, 4.0), 2);
    assert(isClose(a, complex(-7.0, 24.0)));

    auto b = pow(complex(3.0, 4.0), PI);
    assert(ceqrel(b, complex(-152.91512205297134, 35.547499631917738)) >= double.mant_dig - 3);

    auto c = pow(complex(3.0, 4.0), complex(-2.0, 1.0));
    assert(ceqrel(c, complex(0.015351734187477306, -0.0038407695456661503)) >= double.mant_dig - 3);

    auto d = pow(PI, complex(2.0, -1.0));
    assert(ceqrel(d, complex(4.0790296880118296, -8.9872469554541869)) >= double.mant_dig - 1);

    auto e = complex(2.0);
    assert(ceqrel(pow(e, 3), exp(3 * log(e))) >= double.mant_dig - 1);
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    import std.math.traits : floatTraits, RealFormat;
    static foreach (T; AliasSeq!(float, double, real))
    {{
         static if (floatTraits!T.realFormat == RealFormat.ibmExtended)
         {
             /* For IBM real, epsilon is too small (since 1.0 plus any double is
                representable) to be able to expect results within epsilon * 100.  */
         }
         else
         {
             T eps = T.epsilon * 100;

             T a = -1.0;
             T b = 0.5;
             Complex!T ref1 = pow(complex(a), complex(b));
             Complex!T res1 = pow(a, complex(b));
             Complex!T res2 = pow(complex(a), b);
             assert(abs(ref1 - res1) < eps);
             assert(abs(ref1 - res2) < eps);
             assert(abs(res1 - res2) < eps);

             T c = -3.2;
             T d = 1.4;
             Complex!T ref2 = pow(complex(a), complex(b));
             Complex!T res3 = pow(a, complex(b));
             Complex!T res4 = pow(complex(a), b);
             assert(abs(ref2 - res3) < eps);
             assert(abs(ref2 - res4) < eps);
             assert(abs(res3 - res4) < eps);
         }
    }}
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
         auto c = Complex!T(123, 456);
         auto n = c.toNative();
         assert(c.re == n.re && c.im == n.im);
    }}
}

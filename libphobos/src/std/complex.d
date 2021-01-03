// Written in the D programming language.

/** This module contains the $(LREF Complex) type, which is used to represent
    _complex numbers, along with related mathematical operations and functions.

    $(LREF Complex) will eventually
    $(DDLINK deprecate, Deprecated Features, replace)
    the built-in types $(D cfloat), $(D cdouble), $(D creal), $(D ifloat),
    $(D idouble), and $(D ireal).

    Authors:    Lars Tandle Kyllingstad, Don Clugston
    Copyright:  Copyright (c) 2010, Lars T. Kyllingstad.
    License:    $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Source:     $(PHOBOSSRC std/_complex.d)
*/
module std.complex;

import std.traits;

/** Helper function that returns a _complex number with the specified
    real and imaginary parts.

    Params:
        R = (template parameter) type of real part of complex number
        I = (template parameter) type of imaginary part of complex number

        re = real part of complex number to be constructed
        im = (optional) imaginary part of complex number, 0 if omitted.

    Returns:
        $(D Complex) instance with real and imaginary parts set
        to the values provided as input.  If neither $(D re) nor
        $(D im) are floating-point numbers, the return type will
        be $(D Complex!double).  Otherwise, the return type is
        deduced using $(D std.traits.CommonType!(R, I)).
*/
auto complex(R)(R re)  @safe pure nothrow @nogc
if (is(R : double))
{
    static if (isFloatingPoint!R)
        return Complex!R(re, 0);
    else
        return Complex!double(re, 0);
}

/// ditto
auto complex(R, I)(R re, I im)  @safe pure nothrow @nogc
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


/** A complex number parametrised by a type $(D T), which must be either
    $(D float), $(D double) or $(D real).
*/
struct Complex(T)
if (isFloatingPoint!T)
{
    import std.format : FormatSpec;
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
    void toString(Writer, Char)(scope Writer w,
                        FormatSpec!Char formatSpec) const
        if (isOutputRange!(Writer, const(Char)[]))
    {
        import std.format : formatValue;
        import std.math : signbit;
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
    this(Rx : T, Ry : T)(Rx x, Ry y)
    {
        re = x;
        im = y;
    }

    /// ditto
    this(R : T)(R r)
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
    ref Complex opAssign(R : T)(R r)
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
    bool opEquals(R : T)(R r) const
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
    Complex!(CommonType!(T,R)) opBinary(string op, R)(R r) const
        if (isNumeric!R)
    {
        alias C = typeof(return);
        auto w = C(this.re, this.im);
        return w.opOpAssign!(op)(r);
    }

    // numeric + complex,  numeric * complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(R r) const
        if ((op == "+" || op == "*") && (isNumeric!R))
    {
        return opBinary!(op)(r);
    }

    // numeric - complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(R r) const
        if (op == "-" && isNumeric!R)
    {
        return Complex(r - re, -im);
    }

    // numeric / complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(R r) const
        if (op == "/" && isNumeric!R)
    {
        import std.math : fabs;
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

    // numeric ^^ complex
    Complex!(CommonType!(T, R)) opBinaryRight(string op, R)(R lhs) const
        if (op == "^^" && isNumeric!R)
    {
        import std.math : cos, exp, log, sin, PI;
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
    ref Complex opOpAssign(string op, C)(C z)
        if ((op == "+" || op == "-") && is(C R == Complex!R))
    {
        mixin ("re "~op~"= z.re;");
        mixin ("im "~op~"= z.im;");
        return this;
    }

    // complex *= complex
    ref Complex opOpAssign(string op, C)(C z)
        if (op == "*" && is(C R == Complex!R))
    {
        auto temp = re*z.re - im*z.im;
        im = im*z.re + re*z.im;
        re = temp;
        return this;
    }

    // complex /= complex
    ref Complex opOpAssign(string op, C)(C z)
        if (op == "/" && is(C R == Complex!R))
    {
        import std.math : fabs;
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

    // complex ^^= complex
    ref Complex opOpAssign(string op, C)(C z)
        if (op == "^^" && is(C R == Complex!R))
    {
        import std.math : exp, log, cos, sin;
        immutable r = abs(this);
        immutable t = arg(this);
        immutable ab = r^^z.re * exp(-t*z.im);
        immutable ar = t*z.re + log(r)*z.im;

        re = ab*cos(ar);
        im = ab*sin(ar);
        return this;
    }

    // complex += numeric,  complex -= numeric
    ref Complex opOpAssign(string op, U : T)(U a)
        if (op == "+" || op == "-")
    {
        mixin ("re "~op~"= a;");
        return this;
    }

    // complex *= numeric,  complex /= numeric
    ref Complex opOpAssign(string op, U : T)(U a)
        if (op == "*" || op == "/")
    {
        mixin ("re "~op~"= a;");
        mixin ("im "~op~"= a;");
        return this;
    }

    // complex ^^= real
    ref Complex opOpAssign(string op, R)(R r)
        if (op == "^^" && isFloatingPoint!R)
    {
        import std.math : cos, sin;
        immutable ab = abs(this)^^r;
        immutable ar = arg(this)*r;
        re = ab*cos(ar);
        im = ab*sin(ar);
        return this;
    }

    // complex ^^= int
    ref Complex opOpAssign(string op, U)(U i)
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
}

@safe pure nothrow unittest
{
    import std.complex;
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
    assert(approxEqual(abs(ctc), abs(c1)*abs(c2), EPS));
    assert(approxEqual(arg(ctc), arg(c1)+arg(c2), EPS));

    auto cdc = c1 / c2;
    assert(approxEqual(abs(cdc), abs(c1)/abs(c2), EPS));
    assert(approxEqual(arg(cdc), arg(c1)-arg(c2), EPS));

    auto cec = c1^^c2;
    assert(approxEqual(cec.re, 0.11524131979943839881, EPS));
    assert(approxEqual(cec.im, 0.21870790452746026696, EPS));

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
    assert(approxEqual(abs(cdr), abs(c1)/a, EPS));
    assert(approxEqual(arg(cdr), arg(c1), EPS));

    auto cer = c1^^3.0;
    assert(approxEqual(abs(cer), abs(c1)^^3, EPS));
    assert(approxEqual(arg(cer), arg(c1)*3, EPS));

    auto rpc = a + c1;
    assert(rpc == cpr);

    auto rmc = a - c1;
    assert(rmc.re == a-c1.re);
    assert(rmc.im == -c1.im);

    auto rtc = a * c1;
    assert(rtc == ctr);

    auto rdc = a / c1;
    assert(approxEqual(abs(rdc), a/abs(c1), EPS));
    assert(approxEqual(arg(rdc), -arg(c1), EPS));

    rdc = a / c2;
    assert(approxEqual(abs(rdc), a/abs(c2), EPS));
    assert(approxEqual(arg(rdc), -arg(c2), EPS));

    auto rec1a = 1.0 ^^ c1;
    assert(rec1a.re == 1.0);
    assert(rec1a.im == 0.0);

    auto rec2a = 1.0 ^^ c2;
    assert(rec2a.re == 1.0);
    assert(rec2a.im == 0.0);

    auto rec1b = (-1.0) ^^ c1;
    assert(approxEqual(abs(rec1b), std.math.exp(-PI * c1.im), EPS));
    auto arg1b = arg(rec1b);
    /* The argument _should_ be PI, but floating-point rounding error
     * means that in fact the imaginary part is very slightly negative.
     */
    assert(approxEqual(arg1b, PI, EPS) || approxEqual(arg1b, -PI, EPS));

    auto rec2b = (-1.0) ^^ c2;
    assert(approxEqual(abs(rec2b), std.math.exp(-2 * PI), EPS));
    assert(approxEqual(arg(rec2b), PI_2, EPS));

    auto rec3a = 0.79 ^^ complex(6.8, 5.7);
    auto rec3b = complex(0.79, 0.0) ^^ complex(6.8, 5.7);
    assert(approxEqual(rec3a.re, rec3b.re, EPS));
    assert(approxEqual(rec3a.im, rec3b.im, EPS));

    auto rec4a = (-0.79) ^^ complex(6.8, 5.7);
    auto rec4b = complex(-0.79, 0.0) ^^ complex(6.8, 5.7);
    assert(approxEqual(rec4a.re, rec4b.re, EPS));
    assert(approxEqual(rec4a.im, rec4b.im, EPS));

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
    assert(approxEqual(rer2.im, 0.0, EPS));

    auto rer3 = (-a) ^^ complex(-2.0, 0.0);
    rcheck = (-a) ^^ (-2.0);
    assert(feqrel(rer3.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer3.re, rcheck));
    assert(approxEqual(rer3.im, 0.0, EPS));

    auto rer4 = a ^^ complex(-2.0, 0.0);
    rcheck = a ^^ (-2.0);
    assert(feqrel(rer4.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer4.re, rcheck));
    assert(rer4.im == 0.0);

    // Check Complex-int operations.
    foreach (i; 0 .. 6)
    {
        auto cei = c1^^i;
        assert(approxEqual(abs(cei), abs(c1)^^i, EPS));
        // Use cos() here to deal with arguments that go outside
        // the (-pi,pi] interval (only an issue for i>3).
        assert(approxEqual(std.math.cos(arg(cei)), std.math.cos(arg(c1)*i), EPS));
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
    assert(approxEqual(c1c.re, 1.0, EPS));
    assert(approxEqual(c1c.im, 0.0, EPS));

    c1c = c1;
    c1c /= c2;
    assert(approxEqual(c1c.re, 0.588235, EPS));
    assert(approxEqual(c1c.im, -0.352941, EPS));

    c2c /= c1;
    assert(approxEqual(c2c.re, 1.25, EPS));
    assert(approxEqual(c2c.im, 0.75, EPS));

    c2c = c2;
    c2c /= c2;
    assert(approxEqual(c2c.re, 1.0, EPS));
    assert(approxEqual(c2c.im, 0.0, EPS));
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
    import std.math : hypot;
    return hypot(z.re, z.im);
}

///
@safe pure nothrow unittest
{
    static import std.math;
    assert(abs(complex(1.0)) == 1.0);
    assert(abs(complex(0.0, 1.0)) == 1.0);
    assert(abs(complex(1.0L, -2.0L)) == std.math.sqrt(5.0L));
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
    import std.math;
    assert(sqAbs(complex(0.0)) == 0.0);
    assert(sqAbs(complex(1.0)) == 1.0);
    assert(sqAbs(complex(0.0, 1.0)) == 1.0);
    assert(approxEqual(sqAbs(complex(1.0L, -2.0L)), 5.0L));
    assert(approxEqual(sqAbs(complex(-3.0L, 1.0L)), 10.0L));
    assert(approxEqual(sqAbs(complex(1.0f,-1.0f)), 2.0f));
}

/// ditto
T sqAbs(T)(T x) @safe pure nothrow @nogc
if (isFloatingPoint!T)
{
    return x*x;
}

@safe pure nothrow unittest
{
    import std.math;
    assert(sqAbs(0.0) == 0.0);
    assert(sqAbs(-1.0) == 1.0);
    assert(approxEqual(sqAbs(-3.0L), 9.0L));
    assert(approxEqual(sqAbs(-5.0f), 25.0f));
}


/**
 Params: z = A complex number.
 Returns: The argument (or phase) of `z`.
 */
T arg(T)(Complex!T z) @safe pure nothrow @nogc
{
    import std.math : atan2;
    return atan2(z.im, z.re);
}

///
@safe pure nothrow unittest
{
    import std.math;
    assert(arg(complex(1.0)) == 0.0);
    assert(arg(complex(0.0L, 1.0L)) == PI_2);
    assert(arg(complex(1.0L, 1.0L)) == PI_4);
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


/**
  Constructs a complex number given its absolute value and argument.
  Params:
    modulus = The modulus
    argument = The argument
  Returns: The complex number with the given modulus and argument.
*/
Complex!(CommonType!(T, U)) fromPolar(T, U)(T modulus, U argument)
    @safe pure nothrow @nogc
{
    import std.math : sin, cos;
    return Complex!(CommonType!(T,U))
        (modulus*cos(argument), modulus*sin(argument));
}

///
@safe pure nothrow unittest
{
    import std.math;
    auto z = fromPolar(std.math.sqrt(2.0), PI_4);
    assert(approxEqual(z.re, 1.0L, real.epsilon));
    assert(approxEqual(z.im, 1.0L, real.epsilon));
}


/**
    Trigonometric functions on complex numbers.

    Params: z = A complex number.
    Returns: The sine and cosine of `z`, respectively.
*/
Complex!T sin(T)(Complex!T z)  @safe pure nothrow @nogc
{
    import std.math : expi, coshisinh;
    auto cs = expi(z.re);
    auto csh = coshisinh(z.im);
    return typeof(return)(cs.im * csh.re, cs.re * csh.im);
}

///
@safe pure nothrow unittest
{
    static import std.math;
    import std.math : feqrel;
    assert(sin(complex(0.0)) == 0.0);
    assert(sin(complex(2.0, 0)) == std.math.sin(2.0));
    auto c1 = sin(complex(2.0L, 0));
    auto c2 = complex(std.math.sin(2.0L), 0);
    assert(feqrel(c1.re, c2.re) >= real.mant_dig - 1 &&
        feqrel(c1.im, c2.im) >= real.mant_dig - 1);
}


/// ditto
Complex!T cos(T)(Complex!T z)  @safe pure nothrow @nogc
{
    import std.math : expi, coshisinh;
    auto cs = expi(z.re);
    auto csh = coshisinh(z.im);
    return typeof(return)(cs.re * csh.re, - cs.im * csh.im);
}

///
@safe pure nothrow unittest
{
    static import std.math;
    import std.math : feqrel;
    assert(cos(complex(0.0)) == 1.0);
    assert(cos(complex(1.3)) == std.math.cos(1.3));
    auto c1 = cos(complex(0, 5.2L));
    auto c2 = complex(std.math.cosh(5.2L), 0.0L);
    assert(feqrel(c1.re, c2.re) >= real.mant_dig - 1 &&
        feqrel(c1.im, c2.im) >= real.mant_dig - 1);
    auto c3 = cos(complex(1.3L));
    auto c4 = complex(std.math.cos(1.3L), 0.0L);
    assert(feqrel(c3.re, c4.re) >= real.mant_dig - 1 &&
        feqrel(c3.im, c4.im) >= real.mant_dig - 1);
}

/**
    Params: y = A real number.
    Returns: The value of cos(y) + i sin(y).

    Note:
    $(D expi) is included here for convenience and for easy migration of code
    that uses $(REF _expi, std,math).  Unlike $(REF _expi, std,math), which uses the
    x87 $(I fsincos) instruction when possible, this function is no faster
    than calculating cos(y) and sin(y) separately.
*/
Complex!real expi(real y)  @trusted pure nothrow @nogc
{
    import std.math : cos, sin;
    return Complex!real(cos(y), sin(y));
}

///
@safe pure nothrow unittest
{
    static import std.math;

    assert(expi(1.3e5L) == complex(std.math.cos(1.3e5L), std.math.sin(1.3e5L)));
    assert(expi(0.0L) == 1.0L);
    auto z1 = expi(1.234);
    auto z2 = std.math.expi(1.234);
    assert(z1.re == z2.re && z1.im == z2.im);
}


/**
    Params: z = A complex number.
    Returns: The square root of `z`.
*/
Complex!T sqrt(T)(Complex!T z)  @safe pure nothrow @nogc
{
    static import std.math;
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

        x = std.math.fabs(z_re);
        y = std.math.fabs(z_im);
        if (x >= y)
        {
            r = y / x;
            w = std.math.sqrt(x)
                * std.math.sqrt(0.5 * (1 + std.math.sqrt(1 + r * r)));
        }
        else
        {
            r = x / y;
            w = std.math.sqrt(y)
                * std.math.sqrt(0.5 * (r + std.math.sqrt(1 + r * r)));
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
    static import std.math;
    assert(sqrt(complex(0.0)) == 0.0);
    assert(sqrt(complex(1.0L, 0)) == std.math.sqrt(1.0L));
    assert(sqrt(complex(-1.0L, 0)) == complex(0, 1.0L));
}

@safe pure nothrow unittest
{
    import std.math : approxEqual;

    auto c1 = complex(1.0, 1.0);
    auto c2 = Complex!double(0.5, 2.0);

    auto c1s = sqrt(c1);
    assert(approxEqual(c1s.re, 1.09868411));
    assert(approxEqual(c1s.im, 0.45508986));

    auto c2s = sqrt(c2);
    assert(approxEqual(c2s.re, 1.1317134));
    assert(approxEqual(c2s.im, 0.8836155));
}

// Issue 10881: support %f formatting of complex numbers
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
    import std.format;
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

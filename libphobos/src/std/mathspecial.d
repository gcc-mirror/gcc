// Written in the D programming language.

/**
 * Mathematical Special Functions
 *
 * The technical term 'Special Functions' includes several families of
 * transcendental functions, which have important applications in particular
 * branches of mathematics and physics.
 *
 * The gamma and related functions, and the error function are crucial for
 * mathematical statistics.
 * The Bessel and related functions arise in problems involving wave propagation
 * (especially in optics).
 * Other major categories of special functions include the elliptic integrals
 * (related to the arc length of an ellipse), and the hypergeometric functions.
 *
 * Status:
 *  Many more functions will be added to this module.
 *  The naming convention for the distribution functions (gammaIncomplete, etc)
 *  is not yet finalized and will probably change.
 *
 * Macros:
 *      TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
 *              <caption>Special Values</caption>
 *              $0</table>
 *      SVH = $(TR $(TH $1) $(TH $2))
 *      SV  = $(TR $(TD $1) $(TD $2))
 *
 *      NAN = $(RED NAN)
 *      SUP = <span style="vertical-align:super;font-size:smaller">$0</span>
 *      GAMMA = &#915;
 *      THETA = &theta;
 *      INTEGRAL = &#8747;
 *      INTEGRATE = $(BIG &#8747;<sub>$(SMALL $1)</sub><sup>$2</sup>)
 *      POWER = $1<sup>$2</sup>
 *      SUB = $1<sub>$2</sub>
 *      BIGSUM = $(BIG &Sigma; <sup>$2</sup><sub>$(SMALL $1)</sub>)
 *      CHOOSE = $(BIG &#40;) <sup>$(SMALL $1)</sup><sub>$(SMALL $2)</sub> $(BIG &#41;)
 *      PLUSMN = &plusmn;
 *      INFIN = &infin;
 *      PLUSMNINF = &plusmn;&infin;
 *      PI = &pi;
 *      LT = &lt;
 *      GT = &gt;
 *      SQRT = &radic;
 *      HALF = &frac12;
 *
 *
 * Copyright: Based on the CEPHES math library, which is
 *            Copyright (C) 1994 Stephen L. Moshier (moshier@world.std.com).
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Stephen L. Moshier (original C code). Conversion to D by Don Clugston
 * Source:    $(PHOBOSSRC std/_mathspecial.d)
 */
module std.mathspecial;
import std.internal.math.errorfunction;
import std.internal.math.gammafunction;
public import std.math;

/* ***********************************************
 *            GAMMA AND RELATED FUNCTIONS        *
 * ***********************************************/

pure:
nothrow:
@safe:
@nogc:

/** The Gamma function, $(GAMMA)(x)
 *
 *  $(GAMMA)(x) is a generalisation of the factorial function
 *  to real and complex numbers.
 *  Like x!, $(GAMMA)(x+1) = x * $(GAMMA)(x).
 *
 *  Mathematically, if z.re > 0 then
 *   $(GAMMA)(z) = $(INTEGRATE 0, $(INFIN)) $(POWER t, z-1)$(POWER e, -t) dt
 *
 *  $(TABLE_SV
 *    $(SVH  x,           $(GAMMA)(x) )
 *    $(SV  $(NAN),       $(NAN)      )
 *    $(SV  $(PLUSMN)0.0, $(PLUSMNINF))
 *    $(SV integer > 0,   (x-1)!      )
 *    $(SV integer < 0,   $(NAN)      )
 *    $(SV +$(INFIN),      +$(INFIN)   )
 *    $(SV -$(INFIN),      $(NAN)      )
 *  )
 */
real gamma(real x)
{
    return std.internal.math.gammafunction.gamma(x);
}

/** Natural logarithm of the gamma function, $(GAMMA)(x)
 *
 * Returns the base e (2.718...) logarithm of the absolute
 * value of the gamma function of the argument.
 *
 * For reals, logGamma is equivalent to log(fabs(gamma(x))).
 *
 *  $(TABLE_SV
 *    $(SVH  x,             logGamma(x)   )
 *    $(SV  $(NAN),         $(NAN)      )
 *    $(SV integer <= 0,    +$(INFIN)    )
 *    $(SV $(PLUSMNINF),    +$(INFIN)    )
 *  )
 */
real logGamma(real x)
{
    return std.internal.math.gammafunction.logGamma(x);
}

/** The sign of $(GAMMA)(x).
 *
 * Returns -1 if $(GAMMA)(x) < 0,  +1 if $(GAMMA)(x) > 0,
 * $(NAN) if sign is indeterminate.
 *
 * Note that this function can be used in conjunction with logGamma(x) to
 * evaluate gamma for very large values of x.
 */
real sgnGamma(real x)
{
    /* Author: Don Clugston. */
    if (isNaN(x)) return x;
    if (x > 0) return 1.0;
    if (x < -1/real.epsilon)
    {
        // Large negatives lose all precision
        return real.nan;
    }
    long n = rndtol(x);
    if (x == n)
    {
        return x == 0 ?  copysign(1, x) : real.nan;
    }
    return n & 1 ? 1.0 : -1.0;
}

@safe unittest
{
    assert(sgnGamma(5.0) == 1.0);
    assert(isNaN(sgnGamma(-3.0)));
    assert(sgnGamma(-0.1) == -1.0);
    assert(sgnGamma(-55.1) == 1.0);
    assert(isNaN(sgnGamma(-real.infinity)));
    assert(isIdentical(sgnGamma(NaN(0xABC)), NaN(0xABC)));
}

/** Beta function
 *
 * The beta function is defined as
 *
 * beta(x, y) = ($(GAMMA)(x) * $(GAMMA)(y)) / $(GAMMA)(x + y)
 */
real beta(real x, real y)
{
    if ((x+y)> MAXGAMMA)
    {
        return exp(logGamma(x) + logGamma(y) - logGamma(x+y));
    } else return gamma(x) * gamma(y) / gamma(x+y);
}

@safe unittest
{
    assert(isIdentical(beta(NaN(0xABC), 4), NaN(0xABC)));
    assert(isIdentical(beta(2, NaN(0xABC)), NaN(0xABC)));
}

/** Digamma function
 *
 *  The digamma function is the logarithmic derivative of the gamma function.
 *
 *  digamma(x) = d/dx logGamma(x)
 *
 *  See_Also: $(LREF logmdigamma), $(LREF logmdigammaInverse).
 */
real digamma(real x)
{
    return std.internal.math.gammafunction.digamma(x);
}

/** Log Minus Digamma function
 *
 *  logmdigamma(x) = log(x) - digamma(x)
 *
 *  See_Also: $(LREF digamma), $(LREF logmdigammaInverse).
 */
real logmdigamma(real x)
{
    return std.internal.math.gammafunction.logmdigamma(x);
}

/** Inverse of the Log Minus Digamma function
 *
 *  Given y, the function finds x such log(x) - digamma(x) = y.
 *
 *  See_Also: $(LREF logmdigamma), $(LREF digamma).
 */
real logmdigammaInverse(real x)
{
    return std.internal.math.gammafunction.logmdigammaInverse(x);
}

/** Incomplete beta integral
 *
 * Returns incomplete beta integral of the arguments, evaluated
 * from zero to x. The regularized incomplete beta function is defined as
 *
 * betaIncomplete(a, b, x) = $(GAMMA)(a + b) / ( $(GAMMA)(a) $(GAMMA)(b) ) *
 * $(INTEGRATE 0, x) $(POWER t, a-1)$(POWER (1-t), b-1) dt
 *
 * and is the same as the the cumulative distribution function.
 *
 * The domain of definition is 0 <= x <= 1.  In this
 * implementation a and b are restricted to positive values.
 * The integral from x to 1 may be obtained by the symmetry
 * relation
 *
 *    betaIncompleteCompl(a, b, x )  =  betaIncomplete( b, a, 1-x )
 *
 * The integral is evaluated by a continued fraction expansion
 * or, when b * x is small, by a power series.
 */
real betaIncomplete(real a, real b, real x )
{
    return std.internal.math.gammafunction.betaIncomplete(a, b, x);
}

/** Inverse of incomplete beta integral
 *
 * Given y, the function finds x such that
 *
 *  betaIncomplete(a, b, x) == y
 *
 *  Newton iterations or interval halving is used.
 */
real betaIncompleteInverse(real a, real b, real y )
{
    return std.internal.math.gammafunction.betaIncompleteInv(a, b, y);
}

/** Incomplete gamma integral and its complement
 *
 * These functions are defined by
 *
 *   gammaIncomplete = ( $(INTEGRATE 0, x) $(POWER e, -t) $(POWER t, a-1) dt )/ $(GAMMA)(a)
 *
 *  gammaIncompleteCompl(a,x)   =   1 - gammaIncomplete(a,x)
 * = ($(INTEGRATE x, $(INFIN)) $(POWER e, -t) $(POWER t, a-1) dt )/ $(GAMMA)(a)
 *
 * In this implementation both arguments must be positive.
 * The integral is evaluated by either a power series or
 * continued fraction expansion, depending on the relative
 * values of a and x.
 */
real gammaIncomplete(real a, real x )
in {
   assert(x >= 0);
   assert(a > 0);
}
body {
    return std.internal.math.gammafunction.gammaIncomplete(a, x);
}

/** ditto */
real gammaIncompleteCompl(real a, real x )
in {
   assert(x >= 0);
   assert(a > 0);
}
body {
    return std.internal.math.gammafunction.gammaIncompleteCompl(a, x);
}

/** Inverse of complemented incomplete gamma integral
 *
 * Given a and p, the function finds x such that
 *
 *  gammaIncompleteCompl( a, x ) = p.
 */
real gammaIncompleteComplInverse(real a, real p)
in {
  assert(p >= 0 && p <= 1);
  assert(a > 0);
}
body {
    return std.internal.math.gammafunction.gammaIncompleteComplInv(a, p);
}


/* ***********************************************
 *     ERROR FUNCTIONS & NORMAL DISTRIBUTION     *
 * ***********************************************/

 /** Error function
 *
 * The integral is
 *
 *  erf(x) =  2/ $(SQRT)($(PI))
 *     $(INTEGRATE 0, x) exp( - $(POWER t, 2)) dt
 *
 * The magnitude of x is limited to about 106.56 for IEEE 80-bit
 * arithmetic; 1 or -1 is returned outside this range.
 */
real erf(real x)
{
    return std.internal.math.errorfunction.erf(x);
}

/** Complementary error function
 *
 * erfc(x) = 1 - erf(x)
 *         = 2/ $(SQRT)($(PI))
 *     $(INTEGRATE x, $(INFIN)) exp( - $(POWER t, 2)) dt
 *
 * This function has high relative accuracy for
 * values of x far from zero. (For values near zero, use erf(x)).
 */
real erfc(real x)
{
    return std.internal.math.errorfunction.erfc(x);
}


/** Normal distribution function.
 *
 * The normal (or Gaussian, or bell-shaped) distribution is
 * defined as:
 *
 * normalDist(x) = 1/$(SQRT)(2$(PI)) $(INTEGRATE -$(INFIN), x) exp( - $(POWER t, 2)/2) dt
 *   = 0.5 + 0.5 * erf(x/sqrt(2))
 *   = 0.5 * erfc(- x/sqrt(2))
 *
 * To maintain accuracy at values of x near 1.0, use
 *      normalDistribution(x) = 1.0 - normalDistribution(-x).
 *
 * References:
 * $(LINK http://www.netlib.org/cephes/ldoubdoc.html),
 * G. Marsaglia, "Evaluating the Normal Distribution",
 * Journal of Statistical Software <b>11</b>, (July 2004).
 */
real normalDistribution(real x)
{
    return std.internal.math.errorfunction.normalDistributionImpl(x);
}

/** Inverse of Normal distribution function
 *
 * Returns the argument, x, for which the area under the
 * Normal probability density function (integrated from
 * minus infinity to x) is equal to p.
 *
 * Note: This function is only implemented to 80 bit precision.
 */
real normalDistributionInverse(real p)
in {
  assert(p >= 0.0L && p <= 1.0L, "Domain error");
}
body
{
    return std.internal.math.errorfunction.normalDistributionInvImpl(p);
}

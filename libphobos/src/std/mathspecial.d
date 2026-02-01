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
 *      PSI = &Psi;
 *      THETA = &theta;
 *      INTEGRAL = &#8747;
 *      INTEGRATE = $(BIG &#8747;<sub>$(SMALL $1)</sub><sup>$2</sup>)
 *      POWER = $1<sup>$2</sup>
 *      SUB = $1<sub>$2</sub>
 *      BIGSUM = $(BIG &Sigma; <sup>$2</sup><sub>$(SMALL $1)</sub>)
 *      CHOOSE = $(BIG &#40;) <sup>$(SMALL $1)</sup><sub>$(SMALL $2)</sub> $(BIG &#41;)
 *      CEIL = &#8968;$1&#8969;
 *      PLUSMN = &plusmn;
 *      MNPLUS = &mnplus;
 *      INFIN = &infin;
 *      PLUSMNINF = &plusmn;&infin;
 *      MNPLUSINF = &mnplus;&infin;
 *      PI = &pi;
 *      LT = &lt;
 *      LE = &le;
 *      GT = &gt;
 *      SQRT = &radic;
 *      HALF = &frac12;
 *      COMPLEX = &#8450;
 *
 * Copyright: Based on the CEPHES math library, which is
 *            Copyright (C) 1994 Stephen L. Moshier (moshier@world.std.com).
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Stephen L. Moshier (original C code). Conversion to D by Don Clugston
 * Source:    $(PHOBOSSRC std/mathspecial.d)
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
 * Params:
 *   x = the argument of $(GAMMA)
 *
 * Returns:
 *   -1 if $(GAMMA)(x) < 0, +1 if $(GAMMA)(x) > 0, and $(NAN) if $(GAMMA)(x)
 *   does not exist.
 *
 * Note:
 *   This function can be used in conjunction with `logGamma` to evaluate
 *   $(GAMMA)(x) when `gamma(x)` is too large to be represented as a `real`.
 */
pragma(inline, true) real sgnGamma(real x)
{
    return std.internal.math.gammafunction.sgnGamma(x);
}

///
@safe unittest
{
    assert(sgnGamma(10_000) == 1);
}

/** Beta function, B(x,y)
 *
 * Mathematically, if x $(GT) 0 and y $(GT) 0 then
 * B(x,y) = $(INTEGRATE 0, 1)$(POWER t, x-1)$(POWER (l-t), y-1)dt. Through analytic continuation, it
 * is extended to $(COMPLEX)$(SUP 2) where it can be expressed in terms of $(GAMMA)(z).
 *
 * B(x,y) = $(GAMMA)(x)$(GAMMA)(y) / $(GAMMA)(x+y).
 *
 * This implementation restricts x and y to the set of real numbers.
 *
 * Params:
 *   x = the first argument of B
 *   y = the second argument of B
 *
 * Returns:
 *   It returns B(x,y) if it can be computed, otherwise $(NAN).
 *
 * $(TABLE_SV
 *   $(TR $(TH x)                                   $(TH y)                $(TH beta(x, y))   )
 *   $(TR $(TD $(NAN))                              $(TD y)                $(TD $(NAN))       )
 *   $(TR $(TD -$(INFIN))                           $(TD y)                $(TD $(NAN))       )
 *   $(TR $(TD integer $(LT) 0)                     $(TD y)                $(TD $(NAN))       )
 *   $(TR $(TD noninteger and x+y even $(LE) 0)     $(TD noninteger)       $(TD -0)           )
 *   $(TR $(TD noninteger and x+y odd $(LE) 0)      $(TD noninteger)       $(TD +0)           )
 *   $(TR $(TD +0)                                  $(TD positive finite)  $(TD +$(INFIN))    )
 *   $(TR $(TD +0)                                  $(TD +$(INFIN))        $(TD $(NAN))       )
 *   $(TR $(TD $(GT) 0)                             $(TD +$(INFIN))        $(TD +0)           )
 *   $(TR $(TD -0)                                  $(TD +0)               $(TD $(NAN))       )
 *   $(TR $(TD -0)                                  $(TD $(GT) 0)          $(TD -$(INFIN))    )
 *   $(TR $(TD noninteger $(LT) 0, $(CEIL x) odd)   $(TD +$(INFIN))        $(TD -$(INFIN))    )
 *   $(TR $(TD noninteger $(LT) 0, $(CEIL x) even)  $(TD +$(INFIN))        $(TD +$(INFIN))    )
 *   $(TR $(TD noninteger $(LT) 0)                  $(TD $(PLUSMN)0)       $(TD $(PLUSMNINF)) )
 * )
 *
 * Since B(x,y) = B(y,x), if the table states that beta(x, y) is a special value, then beta(y, x) is
 * one as well.
 */
pragma(inline, true) real beta(real x, real y)
{
    return std.internal.math.gammafunction.beta(x, y);
}

///
@safe unittest
{
    assert(beta(1, 2) == 0.5);
    assert(isIdentical(beta(NaN(0xABC), 4), NaN(0xABC)));
    assert(beta(3, 4) == beta(4, 3));
    assert(isNaN(beta(-real.infinity, +0.)));
    assert(isNaN(beta(-1, 2)));
    assert(beta(-0.5, 0.5) is -0.0L);
    assert(beta(-1.5, 0.5) is +0.0L);
    assert(beta(+0., +0.) == +real.infinity);
    assert(isNaN(beta(+0., +real.infinity)));
    assert(beta(1, +real.infinity) is +0.0L);
    assert(isNaN(beta(-0., +0.)));
    assert(beta(-0., nextUp(+0.0L)) == -real.infinity);
    assert(beta(-0.5, +real.infinity) == -real.infinity);
    assert(beta(nextDown(-1.0L), real.infinity) == real.infinity);
    assert(beta(nextDown(-0.0L), +0.) == +real.infinity);
    assert(beta(-0.5, -0.) == -real.infinity);
}

/** Digamma function, $(PSI)(x)
 *
 * $(PSI)(x), is the logarithmic derivative of the gamma function, $(GAMMA)(x).
 *
 * $(PSI)(x) = $(SUP d)$(SUB /, dx) ln|$(GAMMA)(x)|  (the derivative of `logGamma(x)`)
 *
 * Params:
 *   x = the domain value
 *
 * Returns:
 *   It returns $(PSI)(x).
 *
 * $(TABLE_SV
 *   $(SVH x,             digamma(x)   )
 *   $(SV  integer < 0,   $(NAN)       )
 *   $(SV  $(PLUSMN)0.0,  $(MNPLUSINF) )
 *   $(SV  +$(INFIN),     +$(INFIN)    )
 *   $(SV  -$(INFIN),     $(NAN)       )
 *   $(SV  $(NAN),        $(NAN)       )
 * )
 *
 * See_Also: $(LREF logmdigamma), $(LREF logmdigammaInverse).
 */
real digamma(real x)
{
    return std.internal.math.gammafunction.digamma(x);
}

///
@safe unittest
{
    const euler = 0.57721_56649_01532_86060_65121L;

    assert(isClose(digamma(1), -euler));
    assert(digamma(+0.) == -real.infinity);
    assert(digamma(-0.) == +real.infinity);
    assert(digamma(+real.infinity) == +real.infinity);
    assert(isNaN(digamma(-1)));
    assert(isNaN(digamma(-real.infinity)));
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

/** Regularized incomplete beta function $(SUB I, x)(a,b)
 *
 * Mathematically, if a and b are positive real numbers, and 0 $(LE) x $(LE) 1, then
 * $(SUB I, x)(a,b) = $(INTEGRATE 0, x)$(POWER t, a-1)$(POWER (1-t), b-1)dt/B(a,b) where B is the
 * beta function. It is also the cumulative distribution function of the beta distribution.
 *
 * `betaIncomplete(a, b, x)` evaluates $(SUB I, `x`)(`a`,`b`).
 *
 * Params:
 *   a = the first argument of B, must be positive
 *   b = the second argument of B, must be positive
 *   x = the fraction of integration completion from below, 0 $(LE) x $(LE) 1
 *
 * Returns:
 *   It returns $(SUB I, x)(a,b), an element of [0,1].
 *
  * $(TABLE_SV
 *   $(TR $(TH a)         $(TH b)         $(TH x)        $(TH betaIncomplete(a, b, x)) )
 *   $(TR $(TD negative)  $(TD b)         $(TD x)        $(TD $(NAN))                  )
 *   $(TR $(TD a)         $(TD negative)  $(TD x)        $(TD $(NAN))                  )
 *   $(TR $(TD a)         $(TD b)         $(TD $(LT) 0)  $(TD $(NAN))                  )
 *   $(TR $(TD a)         $(TD b)         $(TD $(GT) 1)  $(TD $(NAN))                  )
 *   $(TR $(TD +0)        $(TD +0)        $(TD (0,1))    $(TD $(NAN))                  )
 *   $(TR $(TD $(INFIN))  $(TD $(INFIN))  $(TD (0,1))    $(TD $(NAN))                  )
 * )
 *
 * If one or more of the input parameters are $(NAN), the one with the largest payload is returned.
 * For equal payloads but with possibly different signs, the order of preference is x, a, b.
 *
 * Note:
 *   The integral is evaluated by a continued fraction expansion or, when `b * x` is small, by a
 *   power series.
 *
 * See_Also: $(LREF beta) $(LREF betaIncompleteCompl)
 */
real betaIncomplete(real a, real b, real x )
in
{
    if (!isNaN(a) && !isNaN(b) && !isNaN(x))
    {
        assert(signbit(a) == 0, "a must be positive");
        assert(signbit(b) == 0, "b must be positive");
        assert(x >= 0 && x <= 1, "x must be in [0,1]");
    }
}
out(i; isNaN(i) || (i >=0 && i <= 1))
do
{
    return std.internal.math.gammafunction.betaIncomplete(a, b, x);
}

///
@safe unittest
{
    assert(betaIncomplete(1, 1, .5) == .5);
    assert(betaIncomplete(+0., +0., 0) == 0);
    assert(isNaN(betaIncomplete(+0., +0., .5)));
    assert(isNaN(betaIncomplete(real.infinity, real.infinity, .5)));
    assert(betaIncomplete(real.infinity, real.infinity, 1) == 1);
    assert(betaIncomplete(NaN(0x1), 1, NaN(0x2)) is NaN(0x2));
    assert(betaIncomplete(1, NaN(0x3), -NaN(0x3)) is -NaN(0x3));
}

/** Regularized incomplete beta function complement $(SUB I$(SUP C), x)(a,b)
 *
 * Mathematically, if a $(GT) 0, b $(GT) 0, and 0 $(LE) x $(LE) 1, then
 * $(SUB I$(SUP C), x)(a,b) = $(INTEGRATE x, 1)$(POWER t, a-1)$(POWER (1-t), b-1)dt/B(a,b) where B
 * is the beta function. It is also the complement of the cumulative distribution function of the
 * beta distribution. It can be shown that $(SUB I$(SUP C), x)(a,b) = $(SUB I, 1-x)(b,a).
 *
 * `betaIncompleteCompl(a, b, x)` evaluates $(SUB I$(SUP C), `x`)(`a`,`b`).
 *
 * Params:
 *   a = the first argument of B, must be positive
 *   b = the second argument of B, must be positive
 *   x = the fraction of integration completion from above, 0 $(LE) x $(LE) 1
 *
 * Returns:
 *   It returns $(SUB I$(SUP C), x)(a,b), an element of [0,1].
 *
   * $(TABLE_SV
 *   $(TR $(TH a)         $(TH b)         $(TH x)        $(TH betaIncompleteCompl(a, b, x)) )
 *   $(TR $(TD negative)  $(TD b)         $(TD x)        $(TD $(NAN))                       )
 *   $(TR $(TD a)         $(TD negative)  $(TD x)        $(TD $(NAN))                       )
 *   $(TR $(TD a)         $(TD b)         $(TD $(LT) 0)  $(TD $(NAN))                       )
 *   $(TR $(TD a)         $(TD b)         $(TD $(GT) 1)  $(TD $(NAN))                       )
 *   $(TR $(TD +0)        $(TD +0)        $(TD (0,1))    $(TD $(NAN))                       )
 *   $(TR $(TD $(INFIN))  $(TD $(INFIN))  $(TD (0,1))    $(TD $(NAN))                       )
 * )
 *
 * If one or more of the input parameters are $(NAN), the one with the largest payload is returned.
 * For equal payloads but with possibly different signs, the order of preference is x, a, b.
 *
 * See_Also: $(LREF beta) $(LREF betaIncomplete)
 */
real betaIncompleteCompl(real a, real b, real x)
in
{
    // allow NaN input to pass through so that it can be addressed by the
    // internal NaN payload propagation logic
    if (!isNaN(a) && !isNaN(b) && !isNaN(x))
    {
        assert(signbit(a) == 0, "a must be positive");
        assert(signbit(b) == 0, "b must be positive");
        assert(x >= 0 && x <= 1, "x must be in [0, 1]");
    }
}
out(i; isNaN(i) || (i >=0 && i <= 1))
do
{
    return std.internal.math.gammafunction.betaIncomplete(b, a, 1-x);
}

///
@safe unittest
{
    assert(betaIncompleteCompl(.1, .2, 0) == betaIncomplete(.2, .1, 1));
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
in
{
   assert(x >= 0);
   assert(a > 0);
}
do
{
    return std.internal.math.gammafunction.gammaIncomplete(a, x);
}

/** ditto */
real gammaIncompleteCompl(real a, real x )
in
{
   assert(x >= 0);
   assert(a > 0);
}
do
{
    return std.internal.math.gammafunction.gammaIncompleteCompl(a, x);
}

/** Inverse of complemented incomplete gamma integral
 *
 * Given a and p, the function finds x such that
 *
 *  gammaIncompleteCompl( a, x ) = p.
 */
real gammaIncompleteComplInverse(real a, real p)
in
{
  assert(p >= 0 && p <= 1);
  assert(a > 0);
}
do
{
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


/** Standard normal distribution function.
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

/** Inverse of Standard normal distribution function
 *
 * Returns the argument, x, for which the area under the
 * Normal probability density function (integrated from
 * minus infinity to x) is equal to p.
 *
 * Note: This function is only implemented to 80 bit precision.
 */
real normalDistributionInverse(real p)
in
{
  assert(p >= 0.0L && p <= 1.0L, "Domain error");
}
do
{
    return std.internal.math.errorfunction.normalDistributionInvImpl(p);
}

/**
 * Implementation of the gamma and beta functions, and their integrals.
 *
 * License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Copyright: Based on the CEPHES math library, which is
 *            Copyright (C) 1994 Stephen L. Moshier (moshier@world.std.com).
 * Authors:   Stephen L. Moshier (original C code). Conversion to D by Don Clugston
 *
 *
Macros:
 *  TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
 *      <caption>Special Values</caption>
 *      $0</table>
 *  SVH = $(TR $(TH $1) $(TH $2))
 *  SV  = $(TR $(TD $1) $(TD $2))
 *  GAMMA =  &#915;
 *  INTEGRATE = $(BIG &#8747;<sub>$(SMALL $1)</sub><sup>$2</sup>)
 *  POWER = $1<sup>$2</sup>
 *  NAN = $(RED NAN)
 */
module std.internal.math.gammafunction;
import std.internal.math.errorfunction;
import std.math;

pure:
nothrow:
@safe:
@nogc:

private {

enum real SQRT2PI = 2.50662827463100050242E0L; // sqrt(2pi)
immutable real EULERGAMMA = 0.57721_56649_01532_86060_65120_90082_40243_10421_59335_93992L; /** Euler-Mascheroni constant 0.57721566.. */

// Polynomial approximations for gamma and loggamma.

immutable real[8] GammaNumeratorCoeffs = [ 1.0,
    0x1.acf42d903366539ep-1, 0x1.73a991c8475f1aeap-2, 0x1.c7e918751d6b2a92p-4,
    0x1.86d162cca32cfe86p-6, 0x1.0c378e2e6eaf7cd8p-8, 0x1.dc5c66b7d05feb54p-12,
    0x1.616457b47e448694p-15
];

immutable real[9] GammaDenominatorCoeffs = [ 1.0,
  0x1.a8f9faae5d8fc8bp-2,  -0x1.cb7895a6756eebdep-3,  -0x1.7b9bab006d30652ap-5,
  0x1.c671af78f312082ep-6, -0x1.a11ebbfaf96252dcp-11, -0x1.447b4d2230a77ddap-10,
  0x1.ec1d45bb85e06696p-13,-0x1.d4ce24d05bd0a8e6p-17
];

immutable real[9] GammaSmallCoeffs = [ 1.0,
    0x1.2788cfc6fb618f52p-1, -0x1.4fcf4026afa2f7ecp-1, -0x1.5815e8fa24d7e306p-5,
    0x1.5512320aea2ad71ap-3, -0x1.59af0fb9d82e216p-5,  -0x1.3b4b61d3bfdf244ap-7,
    0x1.d9358e9d9d69fd34p-8, -0x1.38fc4bcbada775d6p-10
];

immutable real[9] GammaSmallNegCoeffs = [ -1.0,
    0x1.2788cfc6fb618f54p-1, 0x1.4fcf4026afa2bc4cp-1, -0x1.5815e8fa2468fec8p-5,
    -0x1.5512320baedaf4b6p-3, -0x1.59af0fa283baf07ep-5, 0x1.3b4a70de31e05942p-7,
    0x1.d9398be3bad13136p-8, 0x1.291b73ee05bcbba2p-10
];

immutable real[7] logGammaStirlingCoeffs = [
    0x1.5555555555553f98p-4, -0x1.6c16c16c07509b1p-9, 0x1.a01a012461cbf1e4p-11,
    -0x1.3813089d3f9d164p-11, 0x1.b911a92555a277b8p-11, -0x1.ed0a7b4206087b22p-10,
    0x1.402523859811b308p-8
];

immutable real[7] logGammaNumerator = [
    -0x1.0edd25913aaa40a2p+23, -0x1.31c6ce2e58842d1ep+24, -0x1.f015814039477c3p+23,
    -0x1.74ffe40c4b184b34p+22, -0x1.0d9c6d08f9eab55p+20,  -0x1.54c6b71935f1fc88p+16,
    -0x1.0e761b42932b2aaep+11
];

immutable real[8] logGammaDenominator = [
    -0x1.4055572d75d08c56p+24, -0x1.deeb6013998e4d76p+24, -0x1.106f7cded5dcc79ep+24,
    -0x1.25e17184848c66d2p+22, -0x1.301303b99a614a0ap+19, -0x1.09e76ab41ae965p+15,
    -0x1.00f95ced9e5f54eep+9, 1.0
];

/*
 * Helper function: Gamma function computed by Stirling's formula.
 *
 * Stirling's formula for the gamma function is:
 *
 * $(GAMMA)(x) = sqrt(2 &pi;) x<sup>x-0.5</sup> exp(-x) (1 + 1/x P(1/x))
 *
 */
real gammaStirling(real x)
{
    // CEPHES code Copyright 1994 by Stephen L. Moshier

    static immutable real[9] SmallStirlingCoeffs = [
        0x1.55555555555543aap-4, 0x1.c71c71c720dd8792p-9, -0x1.5f7268f0b5907438p-9,
        -0x1.e13cd410e0477de6p-13, 0x1.9b0f31643442616ep-11, 0x1.2527623a3472ae08p-14,
        -0x1.37f6bc8ef8b374dep-11,-0x1.8c968886052b872ap-16, 0x1.76baa9c6d3eeddbcp-11
    ];

    static immutable real[7] LargeStirlingCoeffs = [ 1.0L,
        8.33333333333333333333E-2L, 3.47222222222222222222E-3L,
        -2.68132716049382716049E-3L, -2.29472093621399176955E-4L,
        7.84039221720066627474E-4L, 6.97281375836585777429E-5L
    ];

    real w = 1.0L/x;
    real y = exp(x);
    if ( x > 1024.0L )
    {
        // For large x, use rational coefficients from the analytical expansion.
        w = poly(w, LargeStirlingCoeffs);
        // Avoid overflow in pow()
        real v = pow( x, 0.5L * x - 0.25L );
        y = v * (v / y);
    }
    else
    {
        w = 1.0L + w * poly( w, SmallStirlingCoeffs);
        static if (floatTraits!(real).realFormat == RealFormat.ieeeDouble)
        {
            // Avoid overflow in pow() for 64-bit reals
            if (x > 143.0)
            {
                real v = pow( x, 0.5 * x - 0.25 );
                y = v * (v / y);
            }
            else
            {
                y = pow( x, x - 0.5 ) / y;
            }
        }
        else
        {
            y = pow( x, x - 0.5L ) / y;
        }
    }
    y = SQRT2PI * y * w;
    return  y;
}

/*
 * Helper function: Incomplete gamma function computed by Temme's expansion.
 *
 * This is a port of igamma_temme_large from Boost.
 *
 */
real igammaTemmeLarge(real a, real x)
{
    static immutable real[][13] coef = [
        [ -0.333333333333333333333, 0.0833333333333333333333,
          -0.0148148148148148148148, 0.00115740740740740740741,
          0.000352733686067019400353, -0.0001787551440329218107,
          0.39192631785224377817e-4, -0.218544851067999216147e-5,
          -0.18540622107151599607e-5, 0.829671134095308600502e-6,
          -0.176659527368260793044e-6, 0.670785354340149858037e-8,
          0.102618097842403080426e-7, -0.438203601845335318655e-8,
          0.914769958223679023418e-9, -0.255141939949462497669e-10,
          -0.583077213255042506746e-10, 0.243619480206674162437e-10,
          -0.502766928011417558909e-11 ],
        [ -0.00185185185185185185185, -0.00347222222222222222222,
          0.00264550264550264550265, -0.000990226337448559670782,
          0.000205761316872427983539, -0.40187757201646090535e-6,
          -0.18098550334489977837e-4, 0.764916091608111008464e-5,
          -0.161209008945634460038e-5, 0.464712780280743434226e-8,
          0.137863344691572095931e-6, -0.575254560351770496402e-7,
          0.119516285997781473243e-7, -0.175432417197476476238e-10,
          -0.100915437106004126275e-8, 0.416279299184258263623e-9,
          -0.856390702649298063807e-10 ],
        [ 0.00413359788359788359788, -0.00268132716049382716049,
          0.000771604938271604938272, 0.200938786008230452675e-5,
          -0.000107366532263651605215, 0.529234488291201254164e-4,
          -0.127606351886187277134e-4, 0.342357873409613807419e-7,
          0.137219573090629332056e-5, -0.629899213838005502291e-6,
          0.142806142060642417916e-6, -0.204770984219908660149e-9,
          -0.140925299108675210533e-7, 0.622897408492202203356e-8,
          -0.136704883966171134993e-8 ],
        [ 0.000649434156378600823045, 0.000229472093621399176955,
          -0.000469189494395255712128, 0.000267720632062838852962,
          -0.756180167188397641073e-4, -0.239650511386729665193e-6,
          0.110826541153473023615e-4, -0.56749528269915965675e-5,
          0.142309007324358839146e-5, -0.278610802915281422406e-10,
          -0.169584040919302772899e-6, 0.809946490538808236335e-7,
          -0.191111684859736540607e-7 ],
        [ -0.000861888290916711698605, 0.000784039221720066627474,
          -0.000299072480303190179733, -0.146384525788434181781e-5,
          0.664149821546512218666e-4, -0.396836504717943466443e-4,
          0.113757269706784190981e-4, 0.250749722623753280165e-9,
          -0.169541495365583060147e-5, 0.890750753220530968883e-6,
          -0.229293483400080487057e-6],
        [ -0.000336798553366358150309, -0.697281375836585777429e-4,
          0.000277275324495939207873, -0.000199325705161888477003,
          0.679778047793720783882e-4, 0.141906292064396701483e-6,
          -0.135940481897686932785e-4, 0.801847025633420153972e-5,
          -0.229148117650809517038e-5 ],
        [ 0.000531307936463992223166, -0.000592166437353693882865,
          0.000270878209671804482771, 0.790235323266032787212e-6,
          -0.815396936756196875093e-4, 0.561168275310624965004e-4,
          -0.183291165828433755673e-4, -0.307961345060330478256e-8,
          0.346515536880360908674e-5, -0.20291327396058603727e-5,
          0.57887928631490037089e-6 ],
        [ 0.000344367606892377671254, 0.517179090826059219337e-4,
          -0.000334931610811422363117, 0.000281269515476323702274,
          -0.000109765822446847310235, -0.127410090954844853795e-6,
          0.277444515115636441571e-4, -0.182634888057113326614e-4,
          0.578769494973505239894e-5 ],
        [ -0.000652623918595309418922, 0.000839498720672087279993,
          -0.000438297098541721005061, -0.696909145842055197137e-6,
          0.000166448466420675478374, -0.000127835176797692185853,
          0.462995326369130429061e-4 ],
        [ -0.000596761290192746250124, -0.720489541602001055909e-4,
          0.000678230883766732836162, -0.0006401475260262758451,
          0.000277501076343287044992 ],
        [ 0.00133244544948006563713, -0.0019144384985654775265,
          0.00110893691345966373396 ],
        [ 0.00157972766073083495909, 0.000162516262783915816899,
          -0.00206334210355432762645, 0.00213896861856890981541,
          -0.00101085593912630031708 ],
        [ -0.00407251211951401664727, 0.00640336283380806979482,
          -0.00404101610816766177474 ]
    ];

    // avoid nans when one of the arguments is inf:
    if (x == real.infinity && a != real.infinity)
        return 0;

    if (x != real.infinity && a == real.infinity)
        return 1;

    real sigma = (x - a) / a;
    real phi = sigma - log(sigma + 1);

    real y = a * phi;
    real z = sqrt(2 * phi);
    if (x < a)
        z = -z;

    real[13] workspace;
    foreach (i; 0 .. coef.length)
        workspace[i] = poly(z, coef[i]);

    real result = poly(1 / a, workspace);
    result *= exp(-y) / sqrt(2 * PI * a);
    if (x < a)
        result = -result;

    result += erfc(sqrt(y)) / 2;

    return result;
}

} // private

public:
/// The maximum value of x for which gamma(x) < real.infinity.
static if (floatTraits!(real).realFormat == RealFormat.ieeeQuadruple)
    enum real MAXGAMMA = 1755.5483429L;
else static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended)
    enum real MAXGAMMA = 1755.5483429L;
else static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended53)
    enum real MAXGAMMA = 1755.5483429L;
else static if (floatTraits!(real).realFormat == RealFormat.ieeeDouble)
    enum real MAXGAMMA = 171.6243769L;
else
    static assert(0, "missing MAXGAMMA for other real types");


/*****************************************************
 *  The Gamma function, $(GAMMA)(x)
 *
 *  $(GAMMA)(x) is a generalisation of the factorial function
 *  to real and complex numbers.
 *  Like x!, $(GAMMA)(x+1) = x*$(GAMMA)(x).
 *
 *  Mathematically, if z.re > 0 then
 *   $(GAMMA)(z) = $(INTEGRATE 0, &infin;) $(POWER t, z-1)$(POWER e, -t) dt
 *
 *  $(TABLE_SV
 *    $(SVH  x,          $(GAMMA)(x) )
 *    $(SV  $(NAN),      $(NAN)      )
 *    $(SV  &plusmn;0.0, &plusmn;&infin;)
 *    $(SV integer > 0,  (x-1)!      )
 *    $(SV integer < 0,  $(NAN)      )
 *    $(SV +&infin;,     +&infin;    )
 *    $(SV -&infin;,     $(NAN)      )
 *  )
 */
real gamma(real x)
{
/* Based on code from the CEPHES library.
 * CEPHES code Copyright 1994 by Stephen L. Moshier
 *
 * Arguments |x| <= 13 are reduced by recurrence and the function
 * approximated by a rational function of degree 7/8 in the
 * interval (2,3).  Large arguments are handled by Stirling's
 * formula. Large negative arguments are made positive using
 * a reflection formula.
 */

    real q, z;
    if (isNaN(x)) return x;
    if (x == -x.infinity) return real.nan;
    if ( fabs(x) > MAXGAMMA ) return real.infinity;
    if (x == 0) return 1.0 / x; // +- infinity depending on sign of x, create an exception.

    q = fabs(x);

    if ( q > 13.0L )
    {
        // Large arguments are handled by Stirling's
        // formula. Large negative arguments are made positive using
        // the reflection formula.

        if ( x < 0.0L )
        {
            if (x < -1/real.epsilon)
            {
                // Large negatives lose all precision
                return real.nan;
            }
            int sgngam = 1; // sign of gamma.
            long intpart = cast(long)(q);
            if (q == intpart)
                  return real.nan; // poles for all integers <0.
            real p = intpart;
            if ( (intpart & 1) == 0 )
                sgngam = -1;
            z = q - p;
            if ( z > 0.5L )
            {
                p += 1.0L;
                z = q - p;
            }
            z = q * sin( PI * z );
            z = fabs(z) * gammaStirling(q);
            if ( z <= PI/real.max ) return sgngam * real.infinity;
            return sgngam * PI/z;
        }
        else
        {
            return gammaStirling(x);
        }
    }

    // Arguments |x| <= 13 are reduced by recurrence and the function
    // approximated by a rational function of degree 7/8 in the
    // interval (2,3).

    z = 1.0L;
    while ( x >= 3.0L )
    {
        x -= 1.0L;
        z *= x;
    }

    while ( x < -0.03125L )
    {
        z /= x;
        x += 1.0L;
    }

    if ( x <= 0.03125L )
    {
        if ( x == 0.0L )
            return real.nan;
        else
        {
            if ( x < 0.0L )
            {
                x = -x;
                return z / (x * poly( x, GammaSmallNegCoeffs ));
            }
            else
            {
                return z / (x * poly( x, GammaSmallCoeffs ));
            }
        }
    }

    while ( x < 2.0L )
    {
        z /= x;
        x += 1.0L;
    }
    if ( x == 2.0L ) return z;

    x -= 2.0L;
    return z * poly( x, GammaNumeratorCoeffs ) / poly( x, GammaDenominatorCoeffs );
}

@safe unittest
{
    // gamma(n) = factorial(n-1) if n is an integer.
    real fact = 1.0L;
    for (int i=1; fact<real.max; ++i)
    {
        // Require exact equality for small factorials
        if (i<14) assert(gamma(i*1.0L) == fact);
        assert(feqrel(gamma(i*1.0L), fact) >= real.mant_dig-15);
        fact *= (i*1.0L);
    }
    assert(gamma(0.0) == real.infinity);
    assert(gamma(-0.0) == -real.infinity);
    assert(isNaN(gamma(-1.0)));
    assert(isNaN(gamma(-15.0)));
    assert(isIdentical(gamma(NaN(0xABC)), NaN(0xABC)));
    assert(gamma(real.infinity) == real.infinity);
    assert(gamma(real.max) == real.infinity);
    assert(isNaN(gamma(-real.infinity)));
    assert(gamma(real.min_normal*real.epsilon) == real.infinity);
    assert(gamma(MAXGAMMA)< real.infinity);
    assert(gamma(MAXGAMMA*2) == real.infinity);

    // Test some high-precision values (50 decimal digits)
    real SQRT_PI = 1.77245385090551602729816748334114518279754945612238L;


    assert(feqrel(gamma(0.5L), SQRT_PI) >= real.mant_dig-1);
    assert(feqrel(gamma(17.25L), 4.224986665692703551570937158682064589938e13L) >= real.mant_dig-4);

    assert(feqrel(gamma(1.0 / 3.0L),  2.67893853470774763365569294097467764412868937795730L) >= real.mant_dig-2);
    assert(feqrel(gamma(0.25L),
        3.62560990822190831193068515586767200299516768288006L) >= real.mant_dig-1);
    assert(feqrel(gamma(1.0 / 5.0L),
        4.59084371199880305320475827592915200343410999829340L) >= real.mant_dig-1);
}

/*****************************************************
 * Natural logarithm of gamma function.
 *
 * Returns the base e (2.718...) logarithm of the absolute
 * value of the gamma function of the argument.
 *
 * For reals, logGamma is equivalent to log(fabs(gamma(x))).
 *
 *  $(TABLE_SV
 *    $(SVH  x,             logGamma(x)   )
 *    $(SV  $(NAN),         $(NAN)      )
 *    $(SV integer <= 0,    +&infin;    )
 *    $(SV &plusmn;&infin;, +&infin;    )
 *  )
 */
real logGamma(real x)
{
    /* Based on code from the CEPHES library.
     * CEPHES code Copyright 1994 by Stephen L. Moshier
     *
     * For arguments greater than 33, the logarithm of the gamma
     * function is approximated by the logarithmic version of
     * Stirling's formula using a polynomial approximation of
     * degree 4. Arguments between -33 and +33 are reduced by
     * recurrence to the interval [2,3] of a rational approximation.
     * The cosecant reflection formula is employed for arguments
     * less than -33.
     */
    real q, w, z, f, nx;

    if (isNaN(x)) return x;
    if (fabs(x) == x.infinity) return x.infinity;

    if ( x < -34.0L )
    {
        q = -x;
        w = logGamma(q);
        real p = floor(q);
        if ( p == q )
            return real.infinity;
        int intpart = cast(int)(p);
        real sgngam = 1;
        if ( (intpart & 1) == 0 )
            sgngam = -1;
        z = q - p;
        if ( z > 0.5L )
        {
            p += 1.0L;
            z = p - q;
        }
        z = q * sin( PI * z );
        if ( z == 0.0L )
            return sgngam * real.infinity;
    /*  z = LOGPI - logl( z ) - w; */
        z = log( PI/z ) - w;
        return z;
    }

    if ( x < 13.0L )
    {
        z = 1.0L;
        nx = floor( x +  0.5L );
        f = x - nx;
        while ( x >= 3.0L )
        {
            nx -= 1.0L;
            x = nx + f;
            z *= x;
        }
        while ( x < 2.0L )
        {
            if ( fabs(x) <= 0.03125 )
            {
                if ( x == 0.0L )
                    return real.infinity;
                if ( x < 0.0L )
                {
                    x = -x;
                    q = z / (x * poly( x, GammaSmallNegCoeffs));
                } else
                    q = z / (x * poly( x, GammaSmallCoeffs));
                return log( fabs(q) );
            }
            z /= nx +  f;
            nx += 1.0L;
            x = nx + f;
        }
        z = fabs(z);
        if ( x == 2.0L )
            return log(z);
        x = (nx - 2.0L) + f;
        real p = x * rationalPoly( x, logGammaNumerator, logGammaDenominator);
        return log(z) + p;
    }

    // const real MAXLGM = 1.04848146839019521116e+4928L;
    //  if ( x > MAXLGM ) return sgngaml * real.infinity;

    const real LOGSQRT2PI  =  0.91893853320467274178L; // log( sqrt( 2*pi ) )

    q = ( x - 0.5L ) * log(x) - x + LOGSQRT2PI;
    if (x > 1.0e10L) return q;
    real p = 1.0L / (x*x);
    q += poly( p, logGammaStirlingCoeffs ) / x;
    return q ;
}

@safe unittest
{
    assert(isIdentical(logGamma(NaN(0xDEF)), NaN(0xDEF)));
    assert(logGamma(real.infinity) == real.infinity);
    assert(logGamma(-1.0) == real.infinity);
    assert(logGamma(0.0) == real.infinity);
    assert(logGamma(-50.0) == real.infinity);
    assert(isIdentical(0.0L, logGamma(1.0L)));
    assert(isIdentical(0.0L, logGamma(2.0L)));
    assert(logGamma(real.min_normal*real.epsilon) == real.infinity);
    assert(logGamma(-real.min_normal*real.epsilon) == real.infinity);

    // x, correct loggamma(x), correct d/dx loggamma(x).
    immutable static real[] testpoints = [
    8.0L,                    8.525146484375L      + 1.48766904143001655310E-5,   2.01564147795560999654E0L,
    8.99993896484375e-1L,    6.6375732421875e-2L  + 5.11505711292524166220E-6L, -7.54938684259372234258E-1,
    7.31597900390625e-1L,    2.2369384765625e-1   + 5.21506341809849792422E-6L, -1.13355566660398608343E0L,
    2.31639862060546875e-1L, 1.3686676025390625L  + 1.12609441752996145670E-5L, -4.56670961813812679012E0,
    1.73162841796875L,      -8.88214111328125e-2L + 3.36207740803753034508E-6L, 2.33339034686200586920E-1L,
    1.23162841796875L,      -9.3902587890625e-2L  + 1.28765089229009648104E-5L, -2.49677345775751390414E-1L,
    7.3786976294838206464e19L,   3.301798506038663053312e21L - 1.656137564136932662487046269677E5L,
                          4.57477139169563904215E1L,
    1.08420217248550443401E-19L, 4.36682586669921875e1L + 1.37082843669932230418E-5L,
                         -9.22337203685477580858E18L,
    1.0L, 0.0L, -5.77215664901532860607E-1L,
    2.0L, 0.0L, 4.22784335098467139393E-1L,
    -0.5L,  1.2655029296875L    + 9.19379714539648894580E-6L, 3.64899739785765205590E-2L,
    -1.5L,  8.6004638671875e-1L + 6.28657731014510932682E-7L, 7.03156640645243187226E-1L,
    -2.5L, -5.6243896484375E-2L + 1.79986700949327405470E-7,  1.10315664064524318723E0L,
    -3.5L,  -1.30902099609375L  + 1.43111007079536392848E-5L, 1.38887092635952890151E0L
    ];
   // TODO: test derivatives as well.
    for (int i=0; i<testpoints.length; i+=3)
    {
        assert( feqrel(logGamma(testpoints[i]), testpoints[i+1]) > real.mant_dig-5);
        if (testpoints[i]<MAXGAMMA)
        {
            assert( feqrel(log(fabs(gamma(testpoints[i]))), testpoints[i+1]) > real.mant_dig-5);
        }
    }
    assert(logGamma(-50.2) == log(fabs(gamma(-50.2))));
    assert(logGamma(-0.008) == log(fabs(gamma(-0.008))));
    assert(feqrel(logGamma(-38.8),log(fabs(gamma(-38.8)))) > real.mant_dig-4);
    static if (real.mant_dig >= 64) // incl. 80-bit reals
        assert(feqrel(logGamma(1500.0L),log(gamma(1500.0L))) > real.mant_dig-2);
    else static if (real.mant_dig >= 53) // incl. 64-bit reals
        assert(feqrel(logGamma(150.0L),log(gamma(150.0L))) > real.mant_dig-2);
}


private {
/*
 * These value can be calculated like this:
 * 1) Get exact real.max/min_normal/epsilon from compiler:
 *    writefln!"%a"(real.max/min_normal_epsilon)
 * 2) Convert for Wolfram Alpha
 *    0xf.fffffffffffffffp+16380 ==> (f.fffffffffffffff base 16) * 2^16380
 * 3) Calculate result on wofram alpha:
 *    http://www.wolframalpha.com/input/?i=ln((1.ffffffffffffffffffffffffffff+base+16)+*+2%5E16383)+in+base+2
 * 4) Convert to proper format:
 *    string mantissa = "1.011...";
 *    write(mantissa[0 .. 2]); mantissa = mantissa[2 .. $];
 *    for (size_t i = 0; i < mantissa.length/4; i++)
 *    {
 *        writef!"%x"(to!ubyte(mantissa[0 .. 4], 2)); mantissa = mantissa[4 .. $];
 *    }
 */
static if (floatTraits!(real).realFormat == RealFormat.ieeeQuadruple)
{
    enum real MAXLOG = 0x1.62e42fefa39ef35793c7673007e6p+13;  // log(real.max)
    enum real MINLOG = -0x1.6546282207802c89d24d65e96274p+13; // log(real.min_normal*real.epsilon) = log(smallest denormal)
}
else static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended)
{
    enum real MAXLOG = 0x1.62e42fefa39ef358p+13L;  // log(real.max)
    enum real MINLOG = -0x1.6436716d5406e6d8p+13L; // log(real.min_normal*real.epsilon) = log(smallest denormal)
}
else static if (floatTraits!(real).realFormat == RealFormat.ieeeExtended53)
{
    enum real MAXLOG = 0x1.62e42fefa39ef358p+13L;  // log(real.max)
    enum real MINLOG = -0x1.6436716d5406e6d8p+13L; // log(real.min_normal*real.epsilon) = log(smallest denormal)
}
else static if (floatTraits!(real).realFormat == RealFormat.ieeeDouble)
{
    enum real MAXLOG = 0x1.62e42fefa39efp+9L;  // log(real.max)
    enum real MINLOG = -0x1.74385446d71c3p+9L; // log(real.min_normal*real.epsilon) = log(smallest denormal)
}
else
    static assert(0, "missing MAXLOG and MINLOG for other real types");

enum real BETA_BIG = 9.223372036854775808e18L;
enum real BETA_BIGINV = 1.084202172485504434007e-19L;
}

/** Incomplete beta integral
 *
 * Returns incomplete beta integral of the arguments, evaluated
 * from zero to x. The regularized incomplete beta function is defined as
 *
 * betaIncomplete(a, b, x) = &Gamma;(a+b)/(&Gamma;(a) &Gamma;(b)) *
 * $(INTEGRATE 0, x) $(POWER t, a-1)$(POWER (1-t),b-1) dt
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
 * or, when b*x is small, by a power series.
 */
real betaIncomplete(real aa, real bb, real xx )
{
    if ( !(aa>0 && bb>0) )
    {
         if ( isNaN(aa) ) return aa;
         if ( isNaN(bb) ) return bb;
         return real.nan; // domain error
    }
    if (!(xx>0 && xx<1.0))
    {
        if (isNaN(xx)) return xx;
        if ( xx == 0.0L ) return 0.0;
        if ( xx == 1.0L )  return 1.0;
        return real.nan; // domain error
    }
    if ( (bb * xx) <= 1.0L && xx <= 0.95L)
    {
        return betaDistPowerSeries(aa, bb, xx);
    }
    real x;
    real xc; // = 1 - x

    real a, b;
    int flag = 0;

    /* Reverse a and b if x is greater than the mean. */
    if ( xx > (aa/(aa+bb)) )
    {
        // here x > aa/(aa+bb) and (bb*x>1 or x>0.95)
        flag = 1;
        a = bb;
        b = aa;
        xc = xx;
        x = 1.0L - xx;
    }
    else
    {
        a = aa;
        b = bb;
        xc = 1.0L - xx;
        x = xx;
    }

    if ( flag == 1 && (b * x) <= 1.0L && x <= 0.95L)
    {
        // here xx > aa/(aa+bb) and  ((bb*xx>1) or xx>0.95) and (aa*(1-xx)<=1) and xx > 0.05
        return 1.0 - betaDistPowerSeries(a, b, x); // note loss of precision
    }

    real w;
    // Choose expansion for optimal convergence
    // One is for x * (a+b+2) < (a+1),
    // the other is for x * (a+b+2) > (a+1).
    real y = x * (a+b-2.0L) - (a-1.0L);
    if ( y < 0.0L )
    {
        w = betaDistExpansion1( a, b, x );
    }
    else
    {
        w = betaDistExpansion2( a, b, x ) / xc;
    }

    /* Multiply w by the factor
         a      b
        x  (1-x)   Gamma(a+b) / ( a Gamma(a) Gamma(b) ) .   */

    y = a * log(x);
    real t = b * log(xc);
    if ( (a+b) < MAXGAMMA && fabs(y) < MAXLOG && fabs(t) < MAXLOG )
    {
        t = pow(xc,b);
        t *= pow(x,a);
        t /= a;
        t *= w;
        t *= gamma(a+b) / (gamma(a) * gamma(b));
    }
    else
    {
        /* Resort to logarithms.  */
        y += t + logGamma(a+b) - logGamma(a) - logGamma(b);
        y += log(w/a);

        t = exp(y);
/+
        // There seems to be a bug in Cephes at this point.
        // Problems occur for y > MAXLOG, not y < MINLOG.
        if ( y < MINLOG )
        {
            t = 0.0L;
        }
        else
        {
            t = exp(y);
        }
+/
    }
    if ( flag == 1 )
    {
/+   // CEPHES includes this code, but I think it is erroneous.
        if ( t <= real.epsilon )
        {
            t = 1.0L - real.epsilon;
        } else
+/
        t = 1.0L - t;
    }
    return t;
}

/** Inverse of incomplete beta integral
 *
 * Given y, the function finds x such that
 *
 *  betaIncomplete(a, b, x) == y
 *
 *  Newton iterations or interval halving is used.
 */
real betaIncompleteInv(real aa, real bb, real yy0 )
{
    real a, b, y0, d, y, x, x0, x1, lgm, yp, di, dithresh, yl, yh, xt;
    int i, rflg, dir, nflg;

    if (isNaN(yy0)) return yy0;
    if (isNaN(aa)) return aa;
    if (isNaN(bb)) return bb;
    if ( yy0 <= 0.0L )
        return 0.0L;
    if ( yy0 >= 1.0L )
        return 1.0L;
    x0 = 0.0L;
    yl = 0.0L;
    x1 = 1.0L;
    yh = 1.0L;
    if ( aa <= 1.0L || bb <= 1.0L )
    {
        dithresh = 1.0e-7L;
        rflg = 0;
        a = aa;
        b = bb;
        y0 = yy0;
        x = a/(a+b);
        y = betaIncomplete( a, b, x );
        nflg = 0;
        goto ihalve;
    }
    else
    {
        nflg = 0;
        dithresh = 1.0e-4L;
    }

    // approximation to inverse function

    yp = -normalDistributionInvImpl( yy0 );

    if ( yy0 > 0.5L )
    {
        rflg = 1;
        a = bb;
        b = aa;
        y0 = 1.0L - yy0;
        yp = -yp;
    }
    else
    {
        rflg = 0;
        a = aa;
        b = bb;
        y0 = yy0;
    }

    lgm = (yp * yp - 3.0L)/6.0L;
    x = 2.0L/( 1.0L/(2.0L * a-1.0L)  +  1.0L/(2.0L * b - 1.0L) );
    d = yp * sqrt( x + lgm ) / x
        - ( 1.0L/(2.0L * b - 1.0L) - 1.0L/(2.0L * a - 1.0L) )
        * (lgm + (5.0L/6.0L) - 2.0L/(3.0L * x));
    d = 2.0L * d;
    if ( d < MINLOG )
    {
        x = 1.0L;
        goto under;
    }
    x = a/( a + b * exp(d) );
    y = betaIncomplete( a, b, x );
    yp = (y - y0)/y0;
    if ( fabs(yp) < 0.2 )
        goto newt;

    /* Resort to interval halving if not close enough. */
ihalve:

    dir = 0;
    di = 0.5L;
    for ( i=0; i<400; i++ )
    {
        if ( i != 0 )
        {
            x = x0  +  di * (x1 - x0);
            if ( x == 1.0L )
            {
                x = 1.0L - real.epsilon;
            }
            if ( x == 0.0L )
            {
                di = 0.5;
                x = x0  +  di * (x1 - x0);
                if ( x == 0.0 )
                    goto under;
            }
            y = betaIncomplete( a, b, x );
            yp = (x1 - x0)/(x1 + x0);
            if ( fabs(yp) < dithresh )
                goto newt;
            yp = (y-y0)/y0;
            if ( fabs(yp) < dithresh )
                goto newt;
        }
        if ( y < y0 )
        {
            x0 = x;
            yl = y;
            if ( dir < 0 )
            {
                dir = 0;
                di = 0.5L;
            } else if ( dir > 3 )
                di = 1.0L - (1.0L - di) * (1.0L - di);
            else if ( dir > 1 )
                di = 0.5L * di + 0.5L;
            else
                di = (y0 - y)/(yh - yl);
            dir += 1;
            if ( x0 > 0.95L )
            {
                if ( rflg == 1 )
                {
                    rflg = 0;
                    a = aa;
                    b = bb;
                    y0 = yy0;
                }
                else
                {
                    rflg = 1;
                    a = bb;
                    b = aa;
                    y0 = 1.0 - yy0;
                }
                x = 1.0L - x;
                y = betaIncomplete( a, b, x );
                x0 = 0.0;
                yl = 0.0;
                x1 = 1.0;
                yh = 1.0;
                goto ihalve;
            }
        }
        else
        {
            x1 = x;
            if ( rflg == 1 && x1 < real.epsilon )
            {
                x = 0.0L;
                goto done;
            }
            yh = y;
            if ( dir > 0 )
            {
                dir = 0;
                di = 0.5L;
            }
            else if ( dir < -3 )
                di = di * di;
            else if ( dir < -1 )
                di = 0.5L * di;
            else
                di = (y - y0)/(yh - yl);
            dir -= 1;
            }
        }
    if ( x0 >= 1.0L )
    {
        // partial loss of precision
        x = 1.0L - real.epsilon;
        goto done;
    }
    if ( x <= 0.0L )
    {
under:
        // underflow has occurred
        x = real.min_normal * real.min_normal;
        goto done;
    }

newt:

    if ( nflg )
    {
        goto done;
    }
    nflg = 1;
    lgm = logGamma(a+b) - logGamma(a) - logGamma(b);

    for ( i=0; i<15; i++ )
    {
        /* Compute the function at this point. */
        if ( i != 0 )
            y = betaIncomplete(a,b,x);
        if ( y < yl )
        {
            x = x0;
            y = yl;
        }
        else if ( y > yh )
        {
            x = x1;
            y = yh;
        }
        else if ( y < y0 )
        {
            x0 = x;
            yl = y;
        }
        else
        {
            x1 = x;
            yh = y;
        }
        if ( x == 1.0L || x == 0.0L )
            break;
        /* Compute the derivative of the function at this point. */
        d = (a - 1.0L) * log(x) + (b - 1.0L) * log(1.0L - x) + lgm;
        if ( d < MINLOG )
        {
            goto done;
        }
        if ( d > MAXLOG )
        {
            break;
        }
        d = exp(d);
        /* Compute the step to the next approximation of x. */
        d = (y - y0)/d;
        xt = x - d;
        if ( xt <= x0 )
        {
            y = (x - x0) / (x1 - x0);
            xt = x0 + 0.5L * y * (x - x0);
            if ( xt <= 0.0L )
                break;
        }
        if ( xt >= x1 )
        {
            y = (x1 - x) / (x1 - x0);
            xt = x1 - 0.5L * y * (x1 - x);
            if ( xt >= 1.0L )
                break;
        }
        x = xt;
        if ( fabs(d/x) < (128.0L * real.epsilon) )
            goto done;
    }
    /* Did not converge.  */
    dithresh = 256.0L * real.epsilon;
    goto ihalve;

done:
    if ( rflg )
    {
        if ( x <= real.epsilon )
            x = 1.0L - real.epsilon;
        else
            x = 1.0L - x;
    }
    return x;
}

@safe unittest { // also tested by the normal distribution
    // check NaN propagation
    assert(isIdentical(betaIncomplete(NaN(0xABC),2,3), NaN(0xABC)));
    assert(isIdentical(betaIncomplete(7,NaN(0xABC),3), NaN(0xABC)));
    assert(isIdentical(betaIncomplete(7,15,NaN(0xABC)), NaN(0xABC)));
    assert(isIdentical(betaIncompleteInv(NaN(0xABC),1,17), NaN(0xABC)));
    assert(isIdentical(betaIncompleteInv(2,NaN(0xABC),8), NaN(0xABC)));
    assert(isIdentical(betaIncompleteInv(2,3, NaN(0xABC)), NaN(0xABC)));

    assert(isNaN(betaIncomplete(-1, 2, 3)));

    assert(betaIncomplete(1, 2, 0)==0);
    assert(betaIncomplete(1, 2, 1)==1);
    assert(isNaN(betaIncomplete(1, 2, 3)));
    assert(betaIncompleteInv(1, 1, 0)==0);
    assert(betaIncompleteInv(1, 1, 1)==1);

    // Test against Mathematica   betaRegularized[z,a,b]
    // These arbitrary points are chosen to give good code coverage.
    assert(feqrel(betaIncomplete(8, 10, 0.2), 0.010_934_315_234_099_2L) >=  real.mant_dig - 5);
    assert(feqrel(betaIncomplete(2, 2.5, 0.9), 0.989_722_597_604_452_767_171_003_59L) >= real.mant_dig - 1);
    static if (real.mant_dig >= 64) // incl. 80-bit reals
        assert(feqrel(betaIncomplete(1000, 800, 0.5), 1.179140859734704555102808541457164E-06L) >= real.mant_dig - 13);
    else
        assert(feqrel(betaIncomplete(1000, 800, 0.5), 1.179140859734704555102808541457164E-06L) >= real.mant_dig - 14);
    assert(feqrel(betaIncomplete(0.0001, 10000, 0.0001), 0.999978059362107134278786L) >= real.mant_dig - 18);
    assert(betaIncomplete(0.01, 327726.7, 0.545113) == 1.0);
    assert(feqrel(betaIncompleteInv(8, 10, 0.010_934_315_234_099_2L), 0.2L) >= real.mant_dig - 2);
    assert(feqrel(betaIncomplete(0.01, 498.437, 0.0121433), 0.99999664562033077636065L) >= real.mant_dig - 1);
    assert(feqrel(betaIncompleteInv(5, 10, 0.2000002972865658842), 0.229121208190918L) >= real.mant_dig - 3);
    assert(feqrel(betaIncompleteInv(4, 7, 0.8000002209179505L), 0.483657360076904L) >= real.mant_dig - 3);

    // Coverage tests. I don't have correct values for these tests, but
    // these values cover most of the code, so they are useful for
    // regression testing.
    // Extensive testing failed to increase the coverage. It seems likely that about
    // half the code in this function is unnecessary; there is potential for
    // significant improvement over the original CEPHES code.
    static if (real.mant_dig == 64) // 80-bit reals
    {
        assert(betaIncompleteInv(0.01, 8e-48, 5.45464e-20) == 1-real.epsilon);
        assert(betaIncompleteInv(0.01, 8e-48, 9e-26) == 1-real.epsilon);

        // Beware: a one-bit change in pow() changes almost all digits in the result!
        assert(feqrel(
            betaIncompleteInv(0x1.b3d151fbba0eb18p+1, 1.2265e-19, 2.44859e-18),
            0x1.c0110c8531d0952cp-1L
        ) > 10);
        // This next case uncovered a one-bit difference in the FYL2X instruction
        // between Intel and AMD processors. This difference gets magnified by 2^^38.
        // WolframAlpha crashes attempting to calculate this.
        assert(feqrel(betaIncompleteInv(0x1.ff1275ae5b939bcap-41, 4.6713e18, 0.0813601),
            0x1.f97749d90c7adba8p-63L) >= real.mant_dig - 39);
        real a1 = 3.40483;
        assert(betaIncompleteInv(a1, 4.0640301659679627772e19L, 0.545113) == 0x1.ba8c08108aaf5d14p-109);
        real b1 = 2.82847e-25;
        assert(feqrel(betaIncompleteInv(0.01, b1, 9e-26), 0x1.549696104490aa9p-830L) >= real.mant_dig-10);

        // --- Problematic cases ---
        // This is a situation where the series expansion fails to converge
        assert( isNaN(betaIncompleteInv(0.12167, 4.0640301659679627772e19L, 0.0813601)));
        // This next result is almost certainly erroneous.
        // Mathematica states: "(cannot be determined by current methods)"
        assert(betaIncomplete(1.16251e20, 2.18e39, 5.45e-20) == -real.infinity);
        // WolframAlpha gives no result for this, though indicates that it approximately 1.0 - 1.3e-9
        assert(1 - betaIncomplete(0.01, 328222, 4.0375e-5) == 0x1.5f62926b4p-30);
    }
}


private {
// Implementation functions

// Continued fraction expansion #1 for incomplete beta integral
// Use when x < (a+1)/(a+b+2)
real betaDistExpansion1(real a, real b, real x )
{
    real xk, pk, pkm1, pkm2, qk, qkm1, qkm2;
    real k1, k2, k3, k4, k5, k6, k7, k8;
    real r, t, ans;
    int n;

    k1 = a;
    k2 = a + b;
    k3 = a;
    k4 = a + 1.0L;
    k5 = 1.0L;
    k6 = b - 1.0L;
    k7 = k4;
    k8 = a + 2.0L;

    pkm2 = 0.0L;
    qkm2 = 1.0L;
    pkm1 = 1.0L;
    qkm1 = 1.0L;
    ans = 1.0L;
    r = 1.0L;
    n = 0;
    const real thresh = 3.0L * real.epsilon;
    do
    {
        xk = -( x * k1 * k2 )/( k3 * k4 );
        pk = pkm1 +  pkm2 * xk;
        qk = qkm1 +  qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        xk = ( x * k5 * k6 )/( k7 * k8 );
        pk = pkm1 +  pkm2 * xk;
        qk = qkm1 +  qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        if ( qk != 0.0L )
            r = pk/qk;
        if ( r != 0.0L )
        {
            t = fabs( (ans - r)/r );
            ans = r;
        }
        else
        {
           t = 1.0L;
        }

        if ( t < thresh )
            return ans;

        k1 += 1.0L;
        k2 += 1.0L;
        k3 += 2.0L;
        k4 += 2.0L;
        k5 += 1.0L;
        k6 -= 1.0L;
        k7 += 2.0L;
        k8 += 2.0L;

        if ( (fabs(qk) + fabs(pk)) > BETA_BIG )
        {
            pkm2 *= BETA_BIGINV;
            pkm1 *= BETA_BIGINV;
            qkm2 *= BETA_BIGINV;
            qkm1 *= BETA_BIGINV;
            }
        if ( (fabs(qk) < BETA_BIGINV) || (fabs(pk) < BETA_BIGINV) )
        {
            pkm2 *= BETA_BIG;
            pkm1 *= BETA_BIG;
            qkm2 *= BETA_BIG;
            qkm1 *= BETA_BIG;
            }
        }
    while ( ++n < 400 );
// loss of precision has occurred
// mtherr( "incbetl", PLOSS );
    return ans;
}

// Continued fraction expansion #2 for incomplete beta integral
// Use when x > (a+1)/(a+b+2)
real betaDistExpansion2(real a, real b, real x )
{
    real  xk, pk, pkm1, pkm2, qk, qkm1, qkm2;
    real k1, k2, k3, k4, k5, k6, k7, k8;
    real r, t, ans, z;

    k1 = a;
    k2 = b - 1.0L;
    k3 = a;
    k4 = a + 1.0L;
    k5 = 1.0L;
    k6 = a + b;
    k7 = a + 1.0L;
    k8 = a + 2.0L;

    pkm2 = 0.0L;
    qkm2 = 1.0L;
    pkm1 = 1.0L;
    qkm1 = 1.0L;
    z = x / (1.0L-x);
    ans = 1.0L;
    r = 1.0L;
    int n = 0;
    const real thresh = 3.0L * real.epsilon;
    do
    {
        xk = -( z * k1 * k2 )/( k3 * k4 );
        pk = pkm1 +  pkm2 * xk;
        qk = qkm1 +  qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        xk = ( z * k5 * k6 )/( k7 * k8 );
        pk = pkm1 +  pkm2 * xk;
        qk = qkm1 +  qkm2 * xk;
        pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        if ( qk != 0.0L )
            r = pk/qk;
        if ( r != 0.0L )
        {
            t = fabs( (ans - r)/r );
            ans = r;
        } else
            t = 1.0L;

        if ( t < thresh )
            return ans;
        k1 += 1.0L;
        k2 -= 1.0L;
        k3 += 2.0L;
        k4 += 2.0L;
        k5 += 1.0L;
        k6 += 1.0L;
        k7 += 2.0L;
        k8 += 2.0L;

        if ( (fabs(qk) + fabs(pk)) > BETA_BIG )
        {
            pkm2 *= BETA_BIGINV;
            pkm1 *= BETA_BIGINV;
            qkm2 *= BETA_BIGINV;
            qkm1 *= BETA_BIGINV;
        }
        if ( (fabs(qk) < BETA_BIGINV) || (fabs(pk) < BETA_BIGINV) )
        {
            pkm2 *= BETA_BIG;
            pkm1 *= BETA_BIG;
            qkm2 *= BETA_BIG;
            qkm1 *= BETA_BIG;
        }
    } while ( ++n < 400 );
// loss of precision has occurred
//mtherr( "incbetl", PLOSS );
    return ans;
}

/* Power series for incomplete gamma integral.
   Use when b*x is small.  */
real betaDistPowerSeries(real a, real b, real x )
{
    real ai = 1.0L / a;
    real u = (1.0L - b) * x;
    real v = u / (a + 1.0L);
    real t1 = v;
    real t = u;
    real n = 2.0L;
    real s = 0.0L;
    real z = real.epsilon * ai;
    while ( fabs(v) > z )
    {
        u = (n - b) * x / n;
        t *= u;
        v = t / (a + n);
        s += v;
        n += 1.0L;
    }
    s += t1;
    s += ai;

    u = a * log(x);
    if ( (a+b) < MAXGAMMA && fabs(u) < MAXLOG )
    {
        t = gamma(a+b)/(gamma(a)*gamma(b));
        s = s * t * pow(x,a);
    }
    else
    {
        t = logGamma(a+b) - logGamma(a) - logGamma(b) + u + log(s);

        if ( t < MINLOG )
        {
            s = 0.0L;
        } else
            s = exp(t);
    }
    return s;
}

}

/***************************************
 *  Incomplete gamma integral and its complement
 *
 * These functions are defined by
 *
 *   gammaIncomplete = ( $(INTEGRATE 0, x) $(POWER e, -t) $(POWER t, a-1) dt )/ $(GAMMA)(a)
 *
 *  gammaIncompleteCompl(a,x)   =   1 - gammaIncomplete(a,x)
 * = ($(INTEGRATE x, &infin;) $(POWER e, -t) $(POWER t, a-1) dt )/ $(GAMMA)(a)
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
    /* left tail of incomplete gamma function:
     *
     *          inf.      k
     *   a  -x   -       x
     *  x  e     >   ----------
     *           -     -
     *          k=0   | (a+k+1)
     *
     */
    if (x == 0)
       return 0.0L;

    if ( (x > 1.0L) && (x > a ) )
        return 1.0L - gammaIncompleteCompl(a,x);

    real ax = a * log(x) - x - logGamma(a);
/+
    if ( ax < MINLOGL ) return 0; // underflow
    //  { mtherr( "igaml", UNDERFLOW ); return( 0.0L ); }
+/
    ax = exp(ax);

    /* power series */
    real r = a;
    real c = 1.0L;
    real ans = 1.0L;

    do
    {
        r += 1.0L;
        c *= x/r;
        ans += c;
    } while ( c/ans > real.epsilon );

    return ans * ax/a;
}

/** ditto */
real gammaIncompleteCompl(real a, real x )
in {
   assert(x >= 0);
   assert(a > 0);
}
body {
    if (x == 0)
        return 1.0L;
    if ( (x < 1.0L) || (x < a) )
        return 1.0L - gammaIncomplete(a,x);

    // DAC (Cephes bug fix): This is necessary to avoid
    // spurious nans, eg
    // log(x)-x = NaN when x = real.infinity
    const real MAXLOGL =  1.1356523406294143949492E4L;
    if (x > MAXLOGL)
        return igammaTemmeLarge(a, x);

    real ax = a * log(x) - x - logGamma(a);
//const real MINLOGL = -1.1355137111933024058873E4L;
//  if ( ax < MINLOGL ) return 0; // underflow;
    ax = exp(ax);


    /* continued fraction */
    real y = 1.0L - a;
    real z = x + y + 1.0L;
    real c = 0.0L;

    real pk, qk, t;

    real pkm2 = 1.0L;
    real qkm2 = x;
    real pkm1 = x + 1.0L;
    real qkm1 = z * x;
    real ans = pkm1/qkm1;

    do
    {
        c += 1.0L;
        y += 1.0L;
        z += 2.0L;
        real yc = y * c;
        pk = pkm1 * z  -  pkm2 * yc;
        qk = qkm1 * z  -  qkm2 * yc;
        if ( qk != 0.0L )
        {
            real r = pk/qk;
            t = fabs( (ans - r)/r );
            ans = r;
        }
        else
        {
            t = 1.0L;
        }
    pkm2 = pkm1;
        pkm1 = pk;
        qkm2 = qkm1;
        qkm1 = qk;

        const real BIG = 9.223372036854775808e18L;

        if ( fabs(pk) > BIG )
        {
            pkm2 /= BIG;
            pkm1 /= BIG;
            qkm2 /= BIG;
            qkm1 /= BIG;
        }
    } while ( t > real.epsilon );

    return ans * ax;
}

/** Inverse of complemented incomplete gamma integral
 *
 * Given a and p, the function finds x such that
 *
 *  gammaIncompleteCompl( a, x ) = p.
 *
 * Starting with the approximate value x = a $(POWER t, 3), where
 * t = 1 - d - normalDistributionInv(p) sqrt(d),
 * and d = 1/9a,
 * the routine performs up to 10 Newton iterations to find the
 * root of incompleteGammaCompl(a,x) - p = 0.
 */
real gammaIncompleteComplInv(real a, real p)
in {
  assert(p >= 0 && p <= 1);
  assert(a>0);
}
body {
    if (p == 0) return real.infinity;

    real y0 = p;
    const real MAXLOGL =  1.1356523406294143949492E4L;
    real x0, x1, x, yl, yh, y, d, lgm, dithresh;
    int i, dir;

    /* bound the solution */
    x0 = real.max;
    yl = 0.0L;
    x1 = 0.0L;
    yh = 1.0L;
    dithresh = 4.0 * real.epsilon;

    /* approximation to inverse function */
    d = 1.0L/(9.0L*a);
    y = 1.0L - d - normalDistributionInvImpl(y0) * sqrt(d);
    x = a * y * y * y;

    lgm = logGamma(a);

    for ( i=0; i<10; i++ )
    {
        if ( x > x0 || x < x1 )
            goto ihalve;
        y = gammaIncompleteCompl(a,x);
        if ( y < yl || y > yh )
            goto ihalve;
        if ( y < y0 )
        {
            x0 = x;
            yl = y;
        }
        else
        {
            x1 = x;
            yh = y;
        }
    /* compute the derivative of the function at this point */
        d = (a - 1.0L) * log(x0) - x0 - lgm;
        if ( d < -MAXLOGL )
            goto ihalve;
        d = -exp(d);
    /* compute the step to the next approximation of x */
        d = (y - y0)/d;
        x = x - d;
        if ( i < 3 ) continue;
        if ( fabs(d/x) < dithresh ) return x;
    }

    /* Resort to interval halving if Newton iteration did not converge. */
ihalve:
    d = 0.0625L;
    if ( x0 == real.max )
    {
        if ( x <= 0.0L )
            x = 1.0L;
        while ( x0 == real.max )
        {
            x = (1.0L + d) * x;
            y = gammaIncompleteCompl( a, x );
            if ( y < y0 )
            {
                x0 = x;
                yl = y;
                break;
            }
            d = d + d;
        }
    }
    d = 0.5L;
    dir = 0;

    for ( i=0; i<400; i++ )
    {
        x = x1  +  d * (x0 - x1);
        y = gammaIncompleteCompl( a, x );
        lgm = (x0 - x1)/(x1 + x0);
        if ( fabs(lgm) < dithresh )
            break;
        lgm = (y - y0)/y0;
        if ( fabs(lgm) < dithresh )
            break;
        if ( x <= 0.0L )
            break;
        if ( y > y0 )
        {
            x1 = x;
            yh = y;
            if ( dir < 0 )
            {
                dir = 0;
                d = 0.5L;
            } else if ( dir > 1 )
                d = 0.5L * d + 0.5L;
            else
                d = (y0 - yl)/(yh - yl);
            dir += 1;
        }
        else
        {
            x0 = x;
            yl = y;
            if ( dir > 0 )
            {
                dir = 0;
                d = 0.5L;
            } else if ( dir < -1 )
                d = 0.5L * d;
            else
                d = (y0 - yl)/(yh - yl);
            dir -= 1;
        }
    }
    /+
    if ( x == 0.0L )
        mtherr( "igamil", UNDERFLOW );
    +/
    return x;
}

@safe unittest
{
//Values from Excel's GammaInv(1-p, x, 1)
assert(fabs(gammaIncompleteComplInv(1, 0.5) - 0.693147188044814) < 0.00000005);
assert(fabs(gammaIncompleteComplInv(12, 0.99) - 5.42818075054289) < 0.00000005);
assert(fabs(gammaIncompleteComplInv(100, 0.8) - 91.5013985848288L) < 0.000005);
assert(gammaIncomplete(1, 0)==0);
assert(gammaIncompleteCompl(1, 0)==1);
assert(gammaIncomplete(4545, real.infinity)==1);

// Values from Excel's (1-GammaDist(x, alpha, 1, TRUE))

assert(fabs(1.0L-gammaIncompleteCompl(0.5, 2) - 0.954499729507309L) < 0.00000005);
assert(fabs(gammaIncomplete(0.5, 2) - 0.954499729507309L) < 0.00000005);
// Fixed Cephes bug:
assert(gammaIncompleteCompl(384, real.infinity)==0);
assert(gammaIncompleteComplInv(3, 0)==real.infinity);
// Fixed a bug that caused gammaIncompleteCompl to return a wrong value when
// x was larger than a, but not by much, and both were large:
// The value is from WolframAlpha (Gamma[100000, 100001, inf] / Gamma[100000])
static if (real.mant_dig >= 64) // incl. 80-bit reals
    assert(fabs(gammaIncompleteCompl(100000, 100001) - 0.49831792109) < 0.000000000005);
else
    assert(fabs(gammaIncompleteCompl(100000, 100001) - 0.49831792109) < 0.00000005);
}


// DAC: These values are Bn / n for n=2,4,6,8,10,12,14.
immutable real [7] Bn_n  = [
    1.0L/(6*2), -1.0L/(30*4), 1.0L/(42*6), -1.0L/(30*8),
    5.0L/(66*10), -691.0L/(2730*12), 7.0L/(6*14) ];

/** Digamma function
*
*  The digamma function is the logarithmic derivative of the gamma function.
*
*  digamma(x) = d/dx logGamma(x)
*
* References:
*   1. Abramowitz, M., and Stegun, I. A. (1970).
*      Handbook of mathematical functions. Dover, New York,
*      pages 258-259, equations 6.3.6 and 6.3.18.
*/
real digamma(real x)
{
   // Based on CEPHES, Stephen L. Moshier.

    real p, q, nz, s, w, y, z;
    long i, n;
    int negative;

    negative = 0;
    nz = 0.0;

    if ( x <= 0.0 )
    {
        negative = 1;
        q = x;
        p = floor(q);
        if ( p == q )
        {
            return real.nan; // singularity.
        }
    /* Remove the zeros of tan(PI x)
     * by subtracting the nearest integer from x
     */
        nz = q - p;
        if ( nz != 0.5 )
        {
            if ( nz > 0.5 )
            {
                p += 1.0;
                nz = q - p;
            }
            nz = PI/tan(PI*nz);
        }
        else
        {
            nz = 0.0;
        }
        x = 1.0 - x;
    }

    // check for small positive integer
    if ((x <= 13.0) && (x == floor(x)) )
    {
        y = 0.0;
        n = lrint(x);
        // DAC: CEPHES bugfix. Cephes did this in reverse order, which
        // created a larger roundoff error.
        for (i=n-1; i>0; --i)
        {
            y+=1.0L/i;
        }
        y -= EULERGAMMA;
        goto done;
    }

    s = x;
    w = 0.0;
    while ( s < 10.0 )
    {
        w += 1.0/s;
        s += 1.0;
    }

    if ( s < 1.0e17 )
    {
        z = 1.0/(s * s);
        y = z * poly(z, Bn_n);
    } else
        y = 0.0;

    y = log(s)  -  0.5L/s  -  y  -  w;

done:
    if ( negative )
    {
        y -= nz;
    }
    return y;
}

@safe unittest
{
    // Exact values
    assert(digamma(1.0)== -EULERGAMMA);
    assert(feqrel(digamma(0.25), -PI/2 - 3* LN2 - EULERGAMMA) >= real.mant_dig-7);
    assert(feqrel(digamma(1.0L/6), -PI/2 *sqrt(3.0L) - 2* LN2 -1.5*log(3.0L) - EULERGAMMA) >= real.mant_dig-7);
    assert(digamma(-5.0).isNaN());
    assert(feqrel(digamma(2.5), -EULERGAMMA - 2*LN2 + 2.0 + 2.0L/3) >= real.mant_dig-9);
    assert(isIdentical(digamma(NaN(0xABC)), NaN(0xABC)));

    for (int k=1; k<40; ++k)
    {
        real y=0;
        for (int u=k; u >= 1; --u)
        {
            y += 1.0L/u;
        }
        assert(feqrel(digamma(k+1.0), -EULERGAMMA + y) >= real.mant_dig-2);
    }
}

/** Log Minus Digamma function
*
*  logmdigamma(x) = log(x) - digamma(x)
*
* References:
*   1. Abramowitz, M., and Stegun, I. A. (1970).
*      Handbook of mathematical functions. Dover, New York,
*      pages 258-259, equations 6.3.6 and 6.3.18.
*/
real logmdigamma(real x)
{
    if (x <= 0.0)
    {
        if (x == 0.0)
        {
            return real.infinity;
        }
        return real.nan;
    }

    real s = x;
    real w = 0.0;
    while ( s < 10.0 )
    {
        w += 1.0/s;
        s += 1.0;
    }

    real y;
    if ( s < 1.0e17 )
    {
        immutable real z = 1.0/(s * s);
        y = z * poly(z, Bn_n);
    } else
        y = 0.0;

    return x == s ? y + 0.5L/s : (log(x/s) + 0.5L/s + y + w);
}

@safe unittest
{
    assert(logmdigamma(-5.0).isNaN());
    assert(isIdentical(logmdigamma(NaN(0xABC)), NaN(0xABC)));
    assert(logmdigamma(0.0) == real.infinity);
    for (auto x = 0.01; x < 1.0; x += 0.1)
        assert(approxEqual(digamma(x), log(x) - logmdigamma(x)));
    for (auto x = 1.0; x < 15.0; x += 1.0)
        assert(approxEqual(digamma(x), log(x) - logmdigamma(x)));
}

/** Inverse of the Log Minus Digamma function
 *
 *   Returns x such $(D log(x) - digamma(x) == y).
 *
 * References:
 *   1. Abramowitz, M., and Stegun, I. A. (1970).
 *      Handbook of mathematical functions. Dover, New York,
 *      pages 258-259, equation 6.3.18.
 *
 * Authors: Ilya Yaroshenko
 */
real logmdigammaInverse(real y)
{
    import std.numeric : findRoot;
    // FIXME: should be returned back to enum.
    // Fix requires CTFEable `log` on non-x86 targets (check both LDC and GDC).
    immutable maxY = logmdigamma(real.min_normal);
    assert(maxY > 0 && maxY <= real.max);

    if (y >= maxY)
    {
        //lim x->0 (log(x)-digamma(x))*x == 1
        return 1 / y;
    }
    if (y < 0)
    {
        return real.nan;
    }
    if (y < real.min_normal)
    {
        //6.3.18
        return 0.5 / y;
    }
    if (y > 0)
    {
        // x/2 <= logmdigamma(1 / x) <= x, x > 0
        // calls logmdigamma ~6 times
        return 1 / findRoot((real x) => logmdigamma(1 / x) - y, y,  2*y);
    }
    return y; //NaN
}

@safe unittest
{
    import std.typecons;
    //WolframAlpha, 22.02.2015
    immutable Tuple!(real, real)[5] testData = [
        tuple(1.0L, 0.615556766479594378978099158335549201923L),
        tuple(1.0L/8, 4.15937801516894947161054974029150730555L),
        tuple(1.0L/1024, 512.166612384991507850643277924243523243L),
        tuple(0.000500083333325000003968249801594877323784632117L, 1000.0L),
        tuple(1017.644138623741168814449776695062817947092468536L, 1.0L/1024),
    ];
    foreach (test; testData)
        assert(approxEqual(logmdigammaInverse(test[0]), test[1], 2e-15, 0));

    assert(approxEqual(logmdigamma(logmdigammaInverse(1)), 1, 1e-15, 0));
    assert(approxEqual(logmdigamma(logmdigammaInverse(real.min_normal)), real.min_normal, 1e-15, 0));
    assert(approxEqual(logmdigamma(logmdigammaInverse(real.max/2)), real.max/2, 1e-15, 0));
    assert(approxEqual(logmdigammaInverse(logmdigamma(1)), 1, 1e-15, 0));
    assert(approxEqual(logmdigammaInverse(logmdigamma(real.min_normal)), real.min_normal, 1e-15, 0));
    assert(approxEqual(logmdigammaInverse(logmdigamma(real.max/2)), real.max/2, 1e-15, 0));
}

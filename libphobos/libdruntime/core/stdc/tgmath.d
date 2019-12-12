/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_tgmath.h.html, _tgmath.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_tgmath.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.stdc.tgmath;

private import core.stdc.config;
private static import core.stdc.math;
private static import core.stdc.complex;

extern (C):
@trusted: // Everything here operates on floating point and integer values.
nothrow:
@nogc:

version (FreeBSD)
{
    ///
    alias core.stdc.math.acos          acos;
    ///
    alias core.stdc.math.acosf         acos;
    ///
    alias core.stdc.math.acosl         acos;

    ///
    alias core.stdc.complex.cacos      acos;
    ///
    alias core.stdc.complex.cacosf     acos;
    ///
    alias core.stdc.complex.cacosl     acos;

    ///
    alias core.stdc.math.asin          asin;
    ///
    alias core.stdc.math.asinf         asin;
    ///
    alias core.stdc.math.asinl         asin;

    ///
    alias core.stdc.complex.casin      asin;
    ///
    alias core.stdc.complex.casinf     asin;
    ///
    alias core.stdc.complex.casinl     asin;

    ///
    alias core.stdc.math.atan          atan;
    ///
    alias core.stdc.math.atanf         atan;
    ///
    alias core.stdc.math.atanl         atan;

    ///
    alias core.stdc.complex.catan      atan;
    ///
    alias core.stdc.complex.catanf     atan;
    ///
    alias core.stdc.complex.catanl     atan;

    ///
    alias core.stdc.math.atan2         atan2;
    ///
    alias core.stdc.math.atan2f        atan2;
    ///
    alias core.stdc.math.atan2l        atan2;

    ///
    alias core.stdc.math.cos           cos;
    ///
    alias core.stdc.math.cosf          cos;
    ///
    alias core.stdc.math.cosl          cos;

    ///
    alias core.stdc.complex.ccos       cos;
    ///
    alias core.stdc.complex.ccosf      cos;
    ///
    alias core.stdc.complex.ccosl      cos;

    ///
    alias core.stdc.math.sin           sin;
    ///
    alias core.stdc.math.sinf          sin;
    ///
    alias core.stdc.math.sinl          sin;

    ///
    alias core.stdc.complex.csin       csin;
    ///
    alias core.stdc.complex.csinf      csin;
    ///
    alias core.stdc.complex.csinl      csin;

    ///
    alias core.stdc.math.tan           tan;
    ///
    alias core.stdc.math.tanf          tan;
    ///
    alias core.stdc.math.tanl          tan;

    ///
    alias core.stdc.complex.ctan       tan;
    ///
    alias core.stdc.complex.ctanf      tan;
    ///
    alias core.stdc.complex.ctanl      tan;

    ///
    alias core.stdc.math.acosh         acosh;
    ///
    alias core.stdc.math.acoshf        acosh;
    ///
    alias core.stdc.math.acoshl        acosh;

    ///
    alias core.stdc.complex.cacosh     acosh;
    ///
    alias core.stdc.complex.cacoshf    acosh;
    ///
    alias core.stdc.complex.cacoshl    acosh;

    ///
    alias core.stdc.math.asinh         asinh;
    ///
    alias core.stdc.math.asinhf        asinh;
    ///
    alias core.stdc.math.asinhl        asinh;

    ///
    alias core.stdc.complex.casinh     asinh;
    ///
    alias core.stdc.complex.casinhf    asinh;
    ///
    alias core.stdc.complex.casinhl    asinh;

    ///
    alias core.stdc.math.atanh         atanh;
    ///
    alias core.stdc.math.atanhf        atanh;
    ///
    alias core.stdc.math.atanhl        atanh;

    ///
    alias core.stdc.complex.catanh     atanh;
    ///
    alias core.stdc.complex.catanhf    atanh;
    ///
    alias core.stdc.complex.catanhl    atanh;

    ///
    alias core.stdc.math.cosh          cosh;
    ///
    alias core.stdc.math.coshf         cosh;
    ///
    alias core.stdc.math.coshl         cosh;

    ///
    alias core.stdc.complex.ccosh      cosh;
    ///
    alias core.stdc.complex.ccoshf     cosh;
    ///
    alias core.stdc.complex.ccoshl     cosh;

    ///
    alias core.stdc.math.sinh          sinh;
    ///
    alias core.stdc.math.sinhf         sinh;
    ///
    alias core.stdc.math.sinhl         sinh;

    ///
    alias core.stdc.complex.csinh      sinh;
    ///
    alias core.stdc.complex.csinhf     sinh;
    ///
    alias core.stdc.complex.csinhl     sinh;

    ///
    alias core.stdc.math.tanh          tanh;
    ///
    alias core.stdc.math.tanhf         tanh;
    ///
    alias core.stdc.math.tanhl         tanh;

    ///
    alias core.stdc.complex.ctanh      tanh;
    ///
    alias core.stdc.complex.ctanhf     tanh;
    ///
    alias core.stdc.complex.ctanhl     tanh;

    ///
    alias core.stdc.math.exp           exp;
    ///
    alias core.stdc.math.expf          exp;
    ///
    alias core.stdc.math.expl          exp;

    ///
    alias core.stdc.complex.cexp       exp;
    ///
    alias core.stdc.complex.cexpf      exp;
    ///
    alias core.stdc.complex.cexpl      exp;

    ///
    alias core.stdc.math.exp2          exp2;
    ///
    alias core.stdc.math.exp2f         exp2;
    ///
    alias core.stdc.math.exp2l         exp2;

    ///
    alias core.stdc.math.expm1         expm1;
    ///
    alias core.stdc.math.expm1f        expm1;
    ///
    alias core.stdc.math.expm1l        expm1;

    ///
    alias core.stdc.math.frexp         frexp;
    ///
    alias core.stdc.math.frexpf        frexp;
    ///
    alias core.stdc.math.frexpl        frexp;

    ///
    alias core.stdc.math.ilogb         ilogb;
    ///
    alias core.stdc.math.ilogbf        ilogb;
    ///
    alias core.stdc.math.ilogbl        ilogb;

    ///
    alias core.stdc.math.ldexp         ldexp;
    ///
    alias core.stdc.math.ldexpf        ldexp;
    ///
    alias core.stdc.math.ldexpl        ldexp;

    ///
    alias core.stdc.math.log           log;
    ///
    alias core.stdc.math.logf          log;
    ///
    alias core.stdc.math.logl          log;

    ///
    alias core.stdc.complex.clog       log;
    ///
    alias core.stdc.complex.clogf      log;
    ///
    alias core.stdc.complex.clogl      log;

    ///
    alias core.stdc.math.log10         log10;
    ///
    alias core.stdc.math.log10f        log10;
    ///
    alias core.stdc.math.log10l        log10;

    ///
    alias core.stdc.math.log1p         log1p;
    ///
    alias core.stdc.math.log1pf        log1p;
    ///
    alias core.stdc.math.log1pl        log1p;

    ///
    alias core.stdc.math.log2          log2;
    ///
    alias core.stdc.math.log2f         log2;
    ///
    alias core.stdc.math.log2l         log2;

    ///
    alias core.stdc.math.logb          logb;
    ///
    alias core.stdc.math.logbf         logb;
    ///
    alias core.stdc.math.logbl         logb;

    ///
    alias core.stdc.math.modf          modf;
    ///
    alias core.stdc.math.modff         modf;
//  alias core.stdc.math.modfl         modf;

    ///
    alias core.stdc.math.scalbn        scalbn;
    ///
    alias core.stdc.math.scalbnf       scalbn;
    ///
    alias core.stdc.math.scalbnl       scalbn;

    ///
    alias core.stdc.math.scalbln       scalbln;
    ///
    alias core.stdc.math.scalblnf      scalbln;
    ///
    alias core.stdc.math.scalblnl      scalbln;

    ///
    alias core.stdc.math.cbrt          cbrt;
    ///
    alias core.stdc.math.cbrtf         cbrt;
    ///
    alias core.stdc.math.cbrtl         cbrt;

    ///
    alias core.stdc.math.fabs          fabs;
    ///
    alias core.stdc.math.fabsf         fabs;
    ///
    alias core.stdc.math.fabsl         fabs;

    ///
    alias core.stdc.complex.cabs       fabs;
    ///
    alias core.stdc.complex.cabsf      fabs;
    ///
    alias core.stdc.complex.cabsl      fabs;

    ///
    alias core.stdc.math.hypot         hypot;
    ///
    alias core.stdc.math.hypotf        hypot;
    ///
    alias core.stdc.math.hypotl        hypot;

    ///
    alias core.stdc.math.pow           pow;
    ///
    alias core.stdc.math.powf          pow;
    ///
    alias core.stdc.math.powl          pow;

    ///
    alias core.stdc.complex.cpow       pow;
    ///
    alias core.stdc.complex.cpowf      pow;
    ///
    alias core.stdc.complex.cpowl      pow;

    ///
    alias core.stdc.math.sqrt          sqrt;
    ///
    alias core.stdc.math.sqrtf         sqrt;
    ///
    alias core.stdc.math.sqrtl         sqrt;

    ///
    alias core.stdc.complex.csqrt      sqrt;
    ///
    alias core.stdc.complex.csqrtf     sqrt;
    ///
    alias core.stdc.complex.csqrtl     sqrt;

    ///
    alias core.stdc.math.erf           erf;
    ///
    alias core.stdc.math.erff          erf;
    ///
    alias core.stdc.math.erfl          erf;

    ///
    alias core.stdc.math.erfc          erfc;
    ///
    alias core.stdc.math.erfcf         erfc;
    ///
    alias core.stdc.math.erfcl         erfc;

    ///
    alias core.stdc.math.lgamma        lgamma;
    ///
    alias core.stdc.math.lgammaf       lgamma;
    ///
    alias core.stdc.math.lgammal       lgamma;

    ///
    alias core.stdc.math.tgamma        tgamma;
    ///
    alias core.stdc.math.tgammaf       tgamma;
    ///
    alias core.stdc.math.tgammal       tgamma;

    ///
    alias core.stdc.math.ceil          ceil;
    ///
    alias core.stdc.math.ceilf         ceil;
    ///
    alias core.stdc.math.ceill         ceil;

    ///
    alias core.stdc.math.floor         floor;
    ///
    alias core.stdc.math.floorf        floor;
    ///
    alias core.stdc.math.floorl        floor;

    ///
    alias core.stdc.math.nearbyint     nearbyint;
    ///
    alias core.stdc.math.nearbyintf    nearbyint;
    ///
    alias core.stdc.math.nearbyintl    nearbyint;

    ///
    alias core.stdc.math.rint          rint;
    ///
    alias core.stdc.math.rintf         rint;
    ///
    alias core.stdc.math.rintl         rint;

    ///
    alias core.stdc.math.lrint         lrint;
    ///
    alias core.stdc.math.lrintf        lrint;
    ///
    alias core.stdc.math.lrintl        lrint;

    ///
    alias core.stdc.math.llrint        llrint;
    ///
    alias core.stdc.math.llrintf       llrint;
    ///
    alias core.stdc.math.llrintl       llrint;

    ///
    alias core.stdc.math.round         round;
    ///
    alias core.stdc.math.roundf        round;
    ///
    alias core.stdc.math.roundl        round;

    ///
    alias core.stdc.math.lround        lround;
    ///
    alias core.stdc.math.lroundf       lround;
    ///
    alias core.stdc.math.lroundl       lround;

    ///
    alias core.stdc.math.llround       llround;
    ///
    alias core.stdc.math.llroundf      llround;
    ///
    alias core.stdc.math.llroundl      llround;

    ///
    alias core.stdc.math.trunc         trunc;
    ///
    alias core.stdc.math.truncf        trunc;
    ///
    alias core.stdc.math.truncl        trunc;

    ///
    alias core.stdc.math.fmod          fmod;
    ///
    alias core.stdc.math.fmodf         fmod;
    ///
    alias core.stdc.math.fmodl         fmod;

    ///
    alias core.stdc.math.remainder     remainder;
    ///
    alias core.stdc.math.remainderf    remainder;
    ///
    alias core.stdc.math.remainderl    remainder;

    ///
    alias core.stdc.math.remquo        remquo;
    ///
    alias core.stdc.math.remquof       remquo;
    ///
    alias core.stdc.math.remquol       remquo;

    ///
    alias core.stdc.math.copysign      copysign;
    ///
    alias core.stdc.math.copysignf     copysign;
    ///
    alias core.stdc.math.copysignl     copysign;

//  alias core.stdc.math.nan           nan;
//  alias core.stdc.math.nanf          nan;
//  alias core.stdc.math.nanl          nan;

    ///
    alias core.stdc.math.nextafter     nextafter;
    ///
    alias core.stdc.math.nextafterf    nextafter;
    ///
    alias core.stdc.math.nextafterl    nextafter;

    ///
    alias core.stdc.math.nexttoward    nexttoward;
    ///
    alias core.stdc.math.nexttowardf   nexttoward;
    ///
    alias core.stdc.math.nexttowardl   nexttoward;

    ///
    alias core.stdc.math.fdim          fdim;
    ///
    alias core.stdc.math.fdimf         fdim;
    ///
    alias core.stdc.math.fdiml         fdim;

    ///
    alias core.stdc.math.fmax          fmax;
    ///
    alias core.stdc.math.fmaxf         fmax;
    ///
    alias core.stdc.math.fmaxl         fmax;

    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fminl         fmin;

    ///
    alias core.stdc.math.fma           fma;
    ///
    alias core.stdc.math.fmaf          fma;
    ///
    alias core.stdc.math.fmal          fma;

    ///
    alias core.stdc.complex.carg       carg;
    ///
    alias core.stdc.complex.cargf      carg;
    ///
    alias core.stdc.complex.cargl      carg;

    ///
    alias core.stdc.complex.cimag      cimag;
    ///
    alias core.stdc.complex.cimagf     cimag;
    ///
    alias core.stdc.complex.cimagl     cimag;

    ///
    alias core.stdc.complex.conj       conj;
    ///
    alias core.stdc.complex.conjf      conj;
    ///
    alias core.stdc.complex.conjl      conj;

    ///
    alias core.stdc.complex.cproj      cproj;
    ///
    alias core.stdc.complex.cprojf     cproj;
    ///
    alias core.stdc.complex.cprojl     cproj;

//  alias core.stdc.complex.creal      creal;
//  alias core.stdc.complex.crealf     creal;
//  alias core.stdc.complex.creall     creal;
}
version (NetBSD)
{
    ///
    alias core.stdc.math.acos          acos;
    ///
    alias core.stdc.math.acosf         acos;
    ///
    alias core.stdc.math.acosl         acos;

    ///
    alias core.stdc.complex.cacos      acos;
    ///
    alias core.stdc.complex.cacosf     acos;
    ///
    alias core.stdc.complex.cacosl     acos;

    ///
    alias core.stdc.math.asin          asin;
    ///
    alias core.stdc.math.asinf         asin;
    ///
    alias core.stdc.math.asinl         asin;

    ///
    alias core.stdc.complex.casin      asin;
    ///
    alias core.stdc.complex.casinf     asin;
    ///
    alias core.stdc.complex.casinl     asin;

    ///
    alias core.stdc.math.atan          atan;
    ///
    alias core.stdc.math.atanf         atan;
    ///
    alias core.stdc.math.atanl         atan;

    ///
    alias core.stdc.complex.catan      atan;
    ///
    alias core.stdc.complex.catanf     atan;
    ///
    alias core.stdc.complex.catanl     atan;

    ///
    alias core.stdc.math.atan2         atan2;
    ///
    alias core.stdc.math.atan2f        atan2;
    ///
    alias core.stdc.math.atan2l        atan2;

    ///
    alias core.stdc.math.cos           cos;
    ///
    alias core.stdc.math.cosf          cos;
    ///
    alias core.stdc.math.cosl          cos;

    ///
    alias core.stdc.complex.ccos       cos;
    ///
    alias core.stdc.complex.ccosf      cos;
    ///
    alias core.stdc.complex.ccosl      cos;

    ///
    alias core.stdc.math.sin           sin;
    ///
    alias core.stdc.math.sinf          sin;
    ///
    alias core.stdc.math.sinl          sin;

    ///
    alias core.stdc.complex.csin       csin;
    ///
    alias core.stdc.complex.csinf      csin;
    ///
    alias core.stdc.complex.csinl      csin;

    ///
    alias core.stdc.math.tan           tan;
    ///
    alias core.stdc.math.tanf          tan;
    ///
    alias core.stdc.math.tanl          tan;

    ///
    alias core.stdc.complex.ctan       tan;
    ///
    alias core.stdc.complex.ctanf      tan;
    ///
    alias core.stdc.complex.ctanl      tan;

    ///
    alias core.stdc.math.acosh         acosh;
    ///
    alias core.stdc.math.acoshf        acosh;
    ///
    alias core.stdc.math.acoshl        acosh;

    ///
    alias core.stdc.complex.cacosh     acosh;
    ///
    alias core.stdc.complex.cacoshf    acosh;
    ///
    alias core.stdc.complex.cacoshl    acosh;

    ///
    alias core.stdc.math.asinh         asinh;
    ///
    alias core.stdc.math.asinhf        asinh;
    ///
    alias core.stdc.math.asinhl        asinh;

    ///
    alias core.stdc.complex.casinh     asinh;
    ///
    alias core.stdc.complex.casinhf    asinh;
    ///
    alias core.stdc.complex.casinhl    asinh;

    ///
    alias core.stdc.math.atanh         atanh;
    ///
    alias core.stdc.math.atanhf        atanh;
    ///
    alias core.stdc.math.atanhl        atanh;

    ///
    alias core.stdc.complex.catanh     atanh;
    ///
    alias core.stdc.complex.catanhf    atanh;
    ///
    alias core.stdc.complex.catanhl    atanh;

    ///
    alias core.stdc.math.cosh          cosh;
    ///
    alias core.stdc.math.coshf         cosh;
    ///
    alias core.stdc.math.coshl         cosh;

    ///
    alias core.stdc.complex.ccosh      cosh;
    ///
    alias core.stdc.complex.ccoshf     cosh;
    ///
    alias core.stdc.complex.ccoshl     cosh;

    ///
    alias core.stdc.math.sinh          sinh;
    ///
    alias core.stdc.math.sinhf         sinh;
    ///
    alias core.stdc.math.sinhl         sinh;

    ///
    alias core.stdc.complex.csinh      sinh;
    ///
    alias core.stdc.complex.csinhf     sinh;
    ///
    alias core.stdc.complex.csinhl     sinh;

    ///
    alias core.stdc.math.tanh          tanh;
    ///
    alias core.stdc.math.tanhf         tanh;
    ///
    alias core.stdc.math.tanhl         tanh;

    ///
    alias core.stdc.complex.ctanh      tanh;
    ///
    alias core.stdc.complex.ctanhf     tanh;
    ///
    alias core.stdc.complex.ctanhl     tanh;

    ///
    alias core.stdc.math.exp           exp;
    ///
    alias core.stdc.math.expf          exp;
    ///
    alias core.stdc.math.expl          exp;

    ///
    alias core.stdc.complex.cexp       exp;
    ///
    alias core.stdc.complex.cexpf      exp;
    ///
    alias core.stdc.complex.cexpl      exp;

    ///
    alias core.stdc.math.exp2          exp2;
    ///
    alias core.stdc.math.exp2f         exp2;
    ///
    alias core.stdc.math.exp2l         exp2;

    ///
    alias core.stdc.math.expm1         expm1;
    ///
    alias core.stdc.math.expm1f        expm1;
    ///
    alias core.stdc.math.expm1l        expm1;

    ///
    alias core.stdc.math.frexp         frexp;
    ///
    alias core.stdc.math.frexpf        frexp;
    ///
    alias core.stdc.math.frexpl        frexp;

    ///
    alias core.stdc.math.ilogb         ilogb;
    ///
    alias core.stdc.math.ilogbf        ilogb;
    ///
    alias core.stdc.math.ilogbl        ilogb;

    ///
    alias core.stdc.math.ldexp         ldexp;
    ///
    alias core.stdc.math.ldexpf        ldexp;
    ///
    alias core.stdc.math.ldexpl        ldexp;

    ///
    alias core.stdc.math.log           log;
    ///
    alias core.stdc.math.logf          log;
    ///
    alias core.stdc.math.logl          log;

    ///
    alias core.stdc.complex.clog       log;
    ///
    alias core.stdc.complex.clogf      log;
    ///
    alias core.stdc.complex.clogl      log;

    ///
    alias core.stdc.math.log10         log10;
    ///
    alias core.stdc.math.log10f        log10;
    ///
    alias core.stdc.math.log10l        log10;

    ///
    alias core.stdc.math.log1p         log1p;
    ///
    alias core.stdc.math.log1pf        log1p;
    ///
    alias core.stdc.math.log1pl        log1p;

    ///
    alias core.stdc.math.log2          log2;
    ///
    alias core.stdc.math.log2f         log2;
    ///
    alias core.stdc.math.log2l         log2;

    ///
    alias core.stdc.math.logb          logb;
    ///
    alias core.stdc.math.logbf         logb;
    ///
    alias core.stdc.math.logbl         logb;

    ///
    alias core.stdc.math.modf          modf;
    ///
    alias core.stdc.math.modff         modf;
//  alias core.stdc.math.modfl         modf;

    ///
    alias core.stdc.math.scalbn        scalbn;
    ///
    alias core.stdc.math.scalbnf       scalbn;
    ///
    alias core.stdc.math.scalbnl       scalbn;

    ///
    alias core.stdc.math.scalbln       scalbln;
    ///
    alias core.stdc.math.scalblnf      scalbln;
    ///
    alias core.stdc.math.scalblnl      scalbln;

    ///
    alias core.stdc.math.cbrt          cbrt;
    ///
    alias core.stdc.math.cbrtf         cbrt;
    ///
    alias core.stdc.math.cbrtl         cbrt;

    ///
    alias core.stdc.math.fabs          fabs;
    ///
    alias core.stdc.math.fabsf         fabs;
    ///
    alias core.stdc.math.fabsl         fabs;

    ///
    alias core.stdc.complex.cabs       fabs;
    ///
    alias core.stdc.complex.cabsf      fabs;
    ///
    alias core.stdc.complex.cabsl      fabs;

    ///
    alias core.stdc.math.hypot         hypot;
    ///
    alias core.stdc.math.hypotf        hypot;
    ///
    alias core.stdc.math.hypotl        hypot;

    ///
    alias core.stdc.math.pow           pow;
    ///
    alias core.stdc.math.powf          pow;
    ///
    alias core.stdc.math.powl          pow;

    ///
    alias core.stdc.complex.cpow       pow;
    ///
    alias core.stdc.complex.cpowf      pow;
    ///
    alias core.stdc.complex.cpowl      pow;

    ///
    alias core.stdc.math.sqrt          sqrt;
    ///
    alias core.stdc.math.sqrtf         sqrt;
    ///
    alias core.stdc.math.sqrtl         sqrt;

    ///
    alias core.stdc.complex.csqrt      sqrt;
    ///
    alias core.stdc.complex.csqrtf     sqrt;
    ///
    alias core.stdc.complex.csqrtl     sqrt;

    ///
    alias core.stdc.math.erf           erf;
    ///
    alias core.stdc.math.erff          erf;
    ///
    alias core.stdc.math.erfl          erf;

    ///
    alias core.stdc.math.erfc          erfc;
    ///
    alias core.stdc.math.erfcf         erfc;
    ///
    alias core.stdc.math.erfcl         erfc;

    ///
    alias core.stdc.math.lgamma        lgamma;
    ///
    alias core.stdc.math.lgammaf       lgamma;
    ///
    alias core.stdc.math.lgammal       lgamma;

    ///
    alias core.stdc.math.tgamma        tgamma;
    ///
    alias core.stdc.math.tgammaf       tgamma;
    ///
    alias core.stdc.math.tgammal       tgamma;

    ///
    alias core.stdc.math.ceil          ceil;
    ///
    alias core.stdc.math.ceilf         ceil;
    ///
    alias core.stdc.math.ceill         ceil;

    ///
    alias core.stdc.math.floor         floor;
    ///
    alias core.stdc.math.floorf        floor;
    ///
    alias core.stdc.math.floorl        floor;

    ///
    alias core.stdc.math.nearbyint     nearbyint;
    ///
    alias core.stdc.math.nearbyintf    nearbyint;
    ///
    alias core.stdc.math.nearbyintl    nearbyint;

    ///
    alias core.stdc.math.rint          rint;
    ///
    alias core.stdc.math.rintf         rint;
    ///
    alias core.stdc.math.rintl         rint;

    ///
    alias core.stdc.math.lrint         lrint;
    ///
    alias core.stdc.math.lrintf        lrint;
    ///
    alias core.stdc.math.lrintl        lrint;

    ///
    alias core.stdc.math.llrint        llrint;
    ///
    alias core.stdc.math.llrintf       llrint;
    ///
    alias core.stdc.math.llrintl       llrint;

    ///
    alias core.stdc.math.round         round;
    ///
    alias core.stdc.math.roundf        round;
    ///
    alias core.stdc.math.roundl        round;

    ///
    alias core.stdc.math.lround        lround;
    ///
    alias core.stdc.math.lroundf       lround;
    ///
    alias core.stdc.math.lroundl       lround;

    ///
    alias core.stdc.math.llround       llroundl;
    ///
    alias core.stdc.math.llroundf      llroundl;
    ///
    alias core.stdc.math.llroundl      llroundl;

    ///
    alias core.stdc.math.trunc         trunc;
    ///
    alias core.stdc.math.truncf        trunc;
    ///
    alias core.stdc.math.truncl        trunc;

    ///
    alias core.stdc.math.fmod          fmod;
    ///
    alias core.stdc.math.fmodf         fmod;
    ///
    alias core.stdc.math.fmodl         fmod;

    ///
    alias core.stdc.math.remainder     remainder;
    ///
    alias core.stdc.math.remainderf    remainder;
    ///
    alias core.stdc.math.remainderl    remainder;

    ///
    alias core.stdc.math.remquo        remquo;
    ///
    alias core.stdc.math.remquof       remquo;
    ///
    alias core.stdc.math.remquol       remquo;

    ///
    alias core.stdc.math.copysign      copysign;
    ///
    alias core.stdc.math.copysignf     copysign;
    ///
    alias core.stdc.math.copysignl     copysign;

//  alias core.stdc.math.nan           nan;
//  alias core.stdc.math.nanf          nan;
//  alias core.stdc.math.nanl          nan;

    ///
    alias core.stdc.math.nextafter     nextafter;
    ///
    alias core.stdc.math.nextafterf    nextafter;
    ///
    alias core.stdc.math.nextafterl    nextafter;

    ///
    alias core.stdc.math.nexttoward    nexttoward;
    ///
    alias core.stdc.math.nexttowardf   nexttoward;
    ///
    alias core.stdc.math.nexttowardl   nexttoward;

    ///
    alias core.stdc.math.fdim          fdim;
    ///
    alias core.stdc.math.fdimf         fdim;
    ///
    alias core.stdc.math.fdiml         fdim;

    ///
    alias core.stdc.math.fmax          fmax;
    ///
    alias core.stdc.math.fmaxf         fmax;
    ///
    alias core.stdc.math.fmaxl         fmax;

    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fminl         fmin;

    ///
    alias core.stdc.math.fma           fma;
    ///
    alias core.stdc.math.fmaf          fma;
    ///
    alias core.stdc.math.fmal          fma;

    ///
    alias core.stdc.complex.carg       carg;
    ///
    alias core.stdc.complex.cargf      carg;
    ///
    alias core.stdc.complex.cargl      carg;

    ///
    alias core.stdc.complex.cimag      cimag;
    ///
    alias core.stdc.complex.cimagf     cimag;
    ///
    alias core.stdc.complex.cimagl     cimag;

    ///
    alias core.stdc.complex.conj       conj;
    ///
    alias core.stdc.complex.conjf      conj;
    ///
    alias core.stdc.complex.conjl      conj;

    ///
    alias core.stdc.complex.cproj      cproj;
    ///
    alias core.stdc.complex.cprojf     cproj;
    ///
    alias core.stdc.complex.cprojl     cproj;

//  alias core.stdc.complex.creal      creal;
//  alias core.stdc.complex.crealf     creal;
//  alias core.stdc.complex.creall     creal;
}
else version (OpenBSD)
{
    ///
    alias core.stdc.math.acos          acos;
    ///
    alias core.stdc.math.acosf         acos;
    ///
    alias core.stdc.math.acosl         acos;

    ///
    alias core.stdc.complex.cacos      acos;
    ///
    alias core.stdc.complex.cacosf     acos;
    ///
    alias core.stdc.complex.cacosl     acos;

    ///
    alias core.stdc.math.asin          asin;
    ///
    alias core.stdc.math.asinf         asin;
    ///
    alias core.stdc.math.asinl         asin;

    ///
    alias core.stdc.complex.casin      asin;
    ///
    alias core.stdc.complex.casinf     asin;
    ///
    alias core.stdc.complex.casinl     asin;

    ///
    alias core.stdc.math.atan          atan;
    ///
    alias core.stdc.math.atanf         atan;
    ///
    alias core.stdc.math.atanl         atan;

    ///
    alias core.stdc.complex.catan      atan;
    ///
    alias core.stdc.complex.catanf     atan;
    ///
    alias core.stdc.complex.catanl     atan;

    ///
    alias core.stdc.math.atan2         atan2;
    ///
    alias core.stdc.math.atan2f        atan2;
    ///
    alias core.stdc.math.atan2l        atan2;

    ///
    alias core.stdc.math.cos           cos;
    ///
    alias core.stdc.math.cosf          cos;
    ///
    alias core.stdc.math.cosl          cos;

    ///
    alias core.stdc.complex.ccos       cos;
    ///
    alias core.stdc.complex.ccosf      cos;
    ///
    alias core.stdc.complex.ccosl      cos;

    ///
    alias core.stdc.math.sin           sin;
    ///
    alias core.stdc.math.sinf          sin;
    ///
    alias core.stdc.math.sinl          sin;

    ///
    alias core.stdc.complex.csin       csin;
    ///
    alias core.stdc.complex.csinf      csin;
    ///
    alias core.stdc.complex.csinl      csin;

    ///
    alias core.stdc.math.tan           tan;
    ///
    alias core.stdc.math.tanf          tan;
    ///
    alias core.stdc.math.tanl          tan;

    ///
    alias core.stdc.complex.ctan       tan;
    ///
    alias core.stdc.complex.ctanf      tan;
    ///
    alias core.stdc.complex.ctanl      tan;

    ///
    alias core.stdc.math.acosh         acosh;
    ///
    alias core.stdc.math.acoshf        acosh;
    ///
    alias core.stdc.math.acoshl        acosh;

    ///
    alias core.stdc.complex.cacosh     acosh;
    ///
    alias core.stdc.complex.cacoshf    acosh;
    ///
    alias core.stdc.complex.cacoshl    acosh;

    ///
    alias core.stdc.math.asinh         asinh;
    ///
    alias core.stdc.math.asinhf        asinh;
    ///
    alias core.stdc.math.asinhl        asinh;

    ///
    alias core.stdc.complex.casinh     asinh;
    ///
    alias core.stdc.complex.casinhf    asinh;
    ///
    alias core.stdc.complex.casinhl    asinh;

    ///
    alias core.stdc.math.atanh         atanh;
    ///
    alias core.stdc.math.atanhf        atanh;
    ///
    alias core.stdc.math.atanhl        atanh;

    ///
    alias core.stdc.complex.catanh     atanh;
    ///
    alias core.stdc.complex.catanhf    atanh;
    ///
    alias core.stdc.complex.catanhl    atanh;

    ///
    alias core.stdc.math.cosh          cosh;
    ///
    alias core.stdc.math.coshf         cosh;
    ///
    alias core.stdc.math.coshl         cosh;

    ///
    alias core.stdc.complex.ccosh      cosh;
    ///
    alias core.stdc.complex.ccoshf     cosh;
    ///
    alias core.stdc.complex.ccoshl     cosh;

    ///
    alias core.stdc.math.sinh          sinh;
    ///
    alias core.stdc.math.sinhf         sinh;
    ///
    alias core.stdc.math.sinhl         sinh;

    ///
    alias core.stdc.complex.csinh      sinh;
    ///
    alias core.stdc.complex.csinhf     sinh;
    ///
    alias core.stdc.complex.csinhl     sinh;

    ///
    alias core.stdc.math.tanh          tanh;
    ///
    alias core.stdc.math.tanhf         tanh;
    ///
    alias core.stdc.math.tanhl         tanh;

    ///
    alias core.stdc.complex.ctanh      tanh;
    ///
    alias core.stdc.complex.ctanhf     tanh;
    ///
    alias core.stdc.complex.ctanhl     tanh;

    ///
    alias core.stdc.math.exp           exp;
    ///
    alias core.stdc.math.expf          exp;
    ///
    alias core.stdc.math.expl          exp;

    ///
    alias core.stdc.complex.cexp       exp;
    ///
    alias core.stdc.complex.cexpf      exp;
    ///
    alias core.stdc.complex.cexpl      exp;

    ///
    alias core.stdc.math.exp2          exp2;
    ///
    alias core.stdc.math.exp2f         exp2;
    ///
    alias core.stdc.math.exp2l         exp2;

    ///
    alias core.stdc.math.expm1         expm1;
    ///
    alias core.stdc.math.expm1f        expm1;
    ///
    alias core.stdc.math.expm1l        expm1;

    ///
    alias core.stdc.math.frexp         frexp;
    ///
    alias core.stdc.math.frexpf        frexp;
    ///
    alias core.stdc.math.frexpl        frexp;

    ///
    alias core.stdc.math.ilogb         ilogb;
    ///
    alias core.stdc.math.ilogbf        ilogb;
    ///
    alias core.stdc.math.ilogbl        ilogb;

    ///
    alias core.stdc.math.ldexp         ldexp;
    ///
    alias core.stdc.math.ldexpf        ldexp;
    ///
    alias core.stdc.math.ldexpl        ldexp;

    ///
    alias core.stdc.math.log           log;
    ///
    alias core.stdc.math.logf          log;
    ///
    alias core.stdc.math.logl          log;

    ///
    alias core.stdc.complex.clog       log;
    ///
    alias core.stdc.complex.clogf      log;
    ///
    alias core.stdc.complex.clogl      log;

    ///
    alias core.stdc.math.log10         log10;
    ///
    alias core.stdc.math.log10f        log10;
    ///
    alias core.stdc.math.log10l        log10;

    ///
    alias core.stdc.math.log1p         log1p;
    ///
    alias core.stdc.math.log1pf        log1p;
    ///
    alias core.stdc.math.log1pl        log1p;

    ///
    alias core.stdc.math.log2          log2;
    ///
    alias core.stdc.math.log2f         log2;
    ///
    alias core.stdc.math.log2l         log2;

    ///
    alias core.stdc.math.logb          logb;
    ///
    alias core.stdc.math.logbf         logb;
    ///
    alias core.stdc.math.logbl         logb;

    ///
    alias core.stdc.math.fmod          fmod;
    ///
    alias core.stdc.math.fmodf         fmod;
    ///
    alias core.stdc.math.fmodl         fmod;

    ///
    alias core.stdc.math.scalbn        scalbn;
    ///
    alias core.stdc.math.scalbnf       scalbn;
    ///
    alias core.stdc.math.scalbnl       scalbn;

    ///
    alias core.stdc.math.scalbln       scalbln;
    ///
    alias core.stdc.math.scalblnf      scalbln;
    ///
    alias core.stdc.math.scalblnl      scalbln;

    ///
    alias core.stdc.math.cbrt          cbrt;
    ///
    alias core.stdc.math.cbrtf         cbrt;
    ///
    alias core.stdc.math.cbrtl         cbrt;

    ///
    alias core.stdc.math.fabs          fabs;
    ///
    alias core.stdc.math.fabsf         fabs;
    ///
    alias core.stdc.math.fabsl         fabs;

    ///
    alias core.stdc.complex.cabs       fabs;
    ///
    alias core.stdc.complex.cabsf      fabs;
    ///
    alias core.stdc.complex.cabsl      fabs;

    ///
    alias core.stdc.math.hypot         hypot;
    ///
    alias core.stdc.math.hypotf        hypot;
    ///
    alias core.stdc.math.hypotl        hypot;

    ///
    alias core.stdc.math.pow           pow;
    ///
    alias core.stdc.math.powf          pow;
    ///
    alias core.stdc.math.powl          pow;

    ///
    alias core.stdc.complex.cpow       pow;
    ///
    alias core.stdc.complex.cpowf      pow;
    ///
    alias core.stdc.complex.cpowl      pow;

    ///
    alias core.stdc.math.sqrt          sqrt;
    ///
    alias core.stdc.math.sqrtf         sqrt;
    ///
    alias core.stdc.math.sqrtl         sqrt;

    ///
    alias core.stdc.complex.csqrt      sqrt;
    ///
    alias core.stdc.complex.csqrtf     sqrt;
    ///
    alias core.stdc.complex.csqrtl     sqrt;

    ///
    alias core.stdc.math.erf           erf;
    ///
    alias core.stdc.math.erff          erf;
    ///
    alias core.stdc.math.erfl          erf;

    ///
    alias core.stdc.math.erfc          erfc;
    ///
    alias core.stdc.math.erfcf         erfc;
    ///
    alias core.stdc.math.erfcl         erfc;

    ///
    alias core.stdc.math.lgamma        lgamma;
    ///
    alias core.stdc.math.lgammaf       lgamma;
    ///
    alias core.stdc.math.lgammal       lgamma;

    ///
    alias core.stdc.math.tgamma        tgamma;
    ///
    alias core.stdc.math.tgammaf       tgamma;
    ///
    alias core.stdc.math.tgammal       tgamma;

    ///
    alias core.stdc.math.ceil          ceil;
    ///
    alias core.stdc.math.ceilf         ceil;
    ///
    alias core.stdc.math.ceill         ceil;

    ///
    alias core.stdc.math.floor         floor;
    ///
    alias core.stdc.math.floorf        floor;
    ///
    alias core.stdc.math.floorl        floor;

    ///
    alias core.stdc.math.nearbyint     nearbyint;
    ///
    alias core.stdc.math.nearbyintf    nearbyint;
    ///
    alias core.stdc.math.nearbyintl    nearbyint;

    ///
    alias core.stdc.math.rint          rint;
    ///
    alias core.stdc.math.rintf         rint;
    ///
    alias core.stdc.math.rintl         rint;

    ///
    alias core.stdc.math.lrint         lrint;
    ///
    alias core.stdc.math.lrintf        lrint;
    ///
    alias core.stdc.math.lrintl        lrint;

    ///
    alias core.stdc.math.llrint        llrint;
    ///
    alias core.stdc.math.llrintf       llrint;
    ///
    alias core.stdc.math.llrintl       llrint;

    ///
    alias core.stdc.math.round         round;
    ///
    alias core.stdc.math.roundf        round;
    ///
    alias core.stdc.math.roundl        round;

    ///
    alias core.stdc.math.lround        lround;
    ///
    alias core.stdc.math.lroundf       lround;
    ///
    alias core.stdc.math.lroundl       lround;

    ///
    alias core.stdc.math.llround       llround;
    ///
    alias core.stdc.math.llroundf      llround;
    ///
    alias core.stdc.math.llroundl      llround;

    ///
    alias core.stdc.math.trunc         trunc;
    ///
    alias core.stdc.math.truncf        trunc;
    ///
    alias core.stdc.math.truncl        trunc;

    ///
    alias core.stdc.math.remainder     remainder;
    ///
    alias core.stdc.math.remainderf    remainder;
    ///
    alias core.stdc.math.remainderl    remainder;

    ///
    alias core.stdc.math.remquo        remquo;
    ///
    alias core.stdc.math.remquof       remquo;
    ///
    alias core.stdc.math.remquol       remquo;

    ///
    alias core.stdc.math.copysign      copysign;
    ///
    alias core.stdc.math.copysignf     copysign;
    ///
    alias core.stdc.math.copysignl     copysign;

    ///
    alias core.stdc.math.nextafter     nextafter;
    ///
    alias core.stdc.math.nextafterf    nextafter;
    ///
    alias core.stdc.math.nextafterl    nextafter;

    ///
    alias core.stdc.math.nexttoward    nexttoward;
    ///
    alias core.stdc.math.nexttowardf   nexttoward;
    ///
    alias core.stdc.math.nexttowardl   nexttoward;

    ///
    alias core.stdc.math.fdim          fdim;
    ///
    alias core.stdc.math.fdimf         fdim;
    ///
    alias core.stdc.math.fdiml         fdim;

    ///
    alias core.stdc.math.fmax          fmax;
    ///
    alias core.stdc.math.fmaxf         fmax;
    ///
    alias core.stdc.math.fmaxl         fmax;

    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fminl         fmin;

    ///
    alias core.stdc.math.fma           fma;
    ///
    alias core.stdc.math.fmaf          fma;
    ///
    alias core.stdc.math.fmal          fma;

    ///
    alias core.stdc.complex.carg       carg;
    ///
    alias core.stdc.complex.cargf      carg;
    ///
    alias core.stdc.complex.cargl      carg;

    ///
    alias core.stdc.complex.cimag      cimag;
    ///
    alias core.stdc.complex.cimagf     cimag;
    ///
    alias core.stdc.complex.cimagl     cimag;

    ///
    alias core.stdc.complex.conj       conj;
    ///
    alias core.stdc.complex.conjf      conj;
    ///
    alias core.stdc.complex.conjl      conj;

    ///
    alias core.stdc.complex.cproj      cproj;
    ///
    alias core.stdc.complex.cprojf     cproj;
    ///
    alias core.stdc.complex.cprojl     cproj;

//  alias core.stdc.complex.creal      creal;
//  alias core.stdc.complex.crealf     creal;
//  alias core.stdc.complex.creall     creal;
}
else
{
    ///
    alias core.stdc.math.acos          acos;
    ///
    alias core.stdc.math.acosf         acos;
    ///
    alias core.stdc.math.acosl         acos;

    ///
    alias core.stdc.complex.cacos      acos;
    ///
    alias core.stdc.complex.cacosf     acos;
    ///
    alias core.stdc.complex.cacosl     acos;

    ///
    alias core.stdc.math.asin          asin;
    ///
    alias core.stdc.math.asinf         asin;
    ///
    alias core.stdc.math.asinl         asin;

    ///
    alias core.stdc.complex.casin      asin;
    ///
    alias core.stdc.complex.casinf     asin;
    ///
    alias core.stdc.complex.casinl     asin;

    ///
    alias core.stdc.math.atan          atan;
    ///
    alias core.stdc.math.atanf         atan;
    ///
    alias core.stdc.math.atanl         atan;

    ///
    alias core.stdc.complex.catan      atan;
    ///
    alias core.stdc.complex.catanf     atan;
    ///
    alias core.stdc.complex.catanl     atan;

    ///
    alias core.stdc.math.atan2         atan2;
    ///
    alias core.stdc.math.atan2f        atan2;
    ///
    alias core.stdc.math.atan2l        atan2;

    ///
    alias core.stdc.math.cos           cos;
    ///
    alias core.stdc.math.cosf          cos;
    ///
    alias core.stdc.math.cosl          cos;

    ///
    alias core.stdc.complex.ccos       cos;
    ///
    alias core.stdc.complex.ccosf      cos;
    ///
    alias core.stdc.complex.ccosl      cos;

    ///
    alias core.stdc.math.sin           sin;
    ///
    alias core.stdc.math.sinf          sin;
    ///
    alias core.stdc.math.sinl          sin;

    ///
    alias core.stdc.complex.csin       csin;
    ///
    alias core.stdc.complex.csinf      csin;
    ///
    alias core.stdc.complex.csinl      csin;

    ///
    alias core.stdc.math.tan           tan;
    ///
    alias core.stdc.math.tanf          tan;
    ///
    alias core.stdc.math.tanl          tan;

    ///
    alias core.stdc.complex.ctan       tan;
    ///
    alias core.stdc.complex.ctanf      tan;
    ///
    alias core.stdc.complex.ctanl      tan;

    ///
    alias core.stdc.math.acosh         acosh;
    ///
    alias core.stdc.math.acoshf        acosh;
    ///
    alias core.stdc.math.acoshl        acosh;

    ///
    alias core.stdc.complex.cacosh     acosh;
    ///
    alias core.stdc.complex.cacoshf    acosh;
    ///
    alias core.stdc.complex.cacoshl    acosh;

    ///
    alias core.stdc.math.asinh         asinh;
    ///
    alias core.stdc.math.asinhf        asinh;
    ///
    alias core.stdc.math.asinhl        asinh;

    ///
    alias core.stdc.complex.casinh     asinh;
    ///
    alias core.stdc.complex.casinhf    asinh;
    ///
    alias core.stdc.complex.casinhl    asinh;

    ///
    alias core.stdc.math.atanh         atanh;
    ///
    alias core.stdc.math.atanhf        atanh;
    ///
    alias core.stdc.math.atanhl        atanh;

    ///
    alias core.stdc.complex.catanh     atanh;
    ///
    alias core.stdc.complex.catanhf    atanh;
    ///
    alias core.stdc.complex.catanhl    atanh;

    ///
    alias core.stdc.math.cosh          cosh;
    ///
    alias core.stdc.math.coshf         cosh;
    ///
    alias core.stdc.math.coshl         cosh;

    ///
    alias core.stdc.complex.ccosh      cosh;
    ///
    alias core.stdc.complex.ccoshf     cosh;
    ///
    alias core.stdc.complex.ccoshl     cosh;

    ///
    alias core.stdc.math.sinh          sinh;
    ///
    alias core.stdc.math.sinhf         sinh;
    ///
    alias core.stdc.math.sinhl         sinh;

    ///
    alias core.stdc.complex.csinh      sinh;
    ///
    alias core.stdc.complex.csinhf     sinh;
    ///
    alias core.stdc.complex.csinhl     sinh;

    ///
    alias core.stdc.math.tanh          tanh;
    ///
    alias core.stdc.math.tanhf         tanh;
    ///
    alias core.stdc.math.tanhl         tanh;

    ///
    alias core.stdc.complex.ctanh      tanh;
    ///
    alias core.stdc.complex.ctanhf     tanh;
    ///
    alias core.stdc.complex.ctanhl     tanh;

    ///
    alias core.stdc.math.exp           exp;
    ///
    alias core.stdc.math.expf          exp;
    ///
    alias core.stdc.math.expl          exp;

    ///
    alias core.stdc.complex.cexp       exp;
    ///
    alias core.stdc.complex.cexpf      exp;
    ///
    alias core.stdc.complex.cexpl      exp;

    ///
    alias core.stdc.math.exp2          exp2;
    ///
    alias core.stdc.math.exp2f         exp2;
    ///
    alias core.stdc.math.exp2l         exp2;

    ///
    alias core.stdc.math.expm1         expm1;
    ///
    alias core.stdc.math.expm1f        expm1;
    ///
    alias core.stdc.math.expm1l        expm1;

    ///
    alias core.stdc.math.frexp         frexp;
    ///
    alias core.stdc.math.frexpf        frexp;
    ///
    alias core.stdc.math.frexpl        frexp;

    ///
    alias core.stdc.math.ilogb         ilogb;
    ///
    alias core.stdc.math.ilogbf        ilogb;
    ///
    alias core.stdc.math.ilogbl        ilogb;

    ///
    alias core.stdc.math.ldexp         ldexp;
    ///
    alias core.stdc.math.ldexpf        ldexp;
    ///
    alias core.stdc.math.ldexpl        ldexp;

    ///
    alias core.stdc.math.log           log;
    ///
    alias core.stdc.math.logf          log;
    ///
    alias core.stdc.math.logl          log;

    ///
    alias core.stdc.complex.clog       log;
    ///
    alias core.stdc.complex.clogf      log;
    ///
    alias core.stdc.complex.clogl      log;

    ///
    alias core.stdc.math.log10         log10;
    ///
    alias core.stdc.math.log10f        log10;
    ///
    alias core.stdc.math.log10l        log10;

    ///
    alias core.stdc.math.log1p         log1p;
    ///
    alias core.stdc.math.log1pf        log1p;
    ///
    alias core.stdc.math.log1pl        log1p;

    ///
    alias core.stdc.math.log2          log2;
    ///
    alias core.stdc.math.log2f         log2;
    ///
    alias core.stdc.math.log2l         log2;

    ///
    alias core.stdc.math.logb          logb;
    ///
    alias core.stdc.math.logbf         logb;
    ///
    alias core.stdc.math.logbl         logb;

    ///
    alias core.stdc.math.modf          modf;
    ///
    alias core.stdc.math.modff         modf;
    ///
    alias core.stdc.math.modfl         modf;

    ///
    alias core.stdc.math.scalbn        scalbn;
    ///
    alias core.stdc.math.scalbnf       scalbn;
    ///
    alias core.stdc.math.scalbnl       scalbn;

    ///
    alias core.stdc.math.scalbln       scalbln;
    ///
    alias core.stdc.math.scalblnf      scalbln;
    ///
    alias core.stdc.math.scalblnl      scalbln;

    ///
    alias core.stdc.math.cbrt          cbrt;
    ///
    alias core.stdc.math.cbrtf         cbrt;
    ///
    alias core.stdc.math.cbrtl         cbrt;

    ///
    alias core.stdc.math.fabs          fabs;
    version (CRuntime_Microsoft)
    {
    }
    else
    {
        ///
        alias core.stdc.math.fabsf         fabs;
        ///
        alias core.stdc.math.fabsl         fabs;
    }

    ///
    alias core.stdc.complex.cabs       fabs;
    ///
    alias core.stdc.complex.cabsf      fabs;
    ///
    alias core.stdc.complex.cabsl      fabs;

    ///
    alias core.stdc.math.hypot         hypot;
    ///
    alias core.stdc.math.hypotf        hypot;
    ///
    alias core.stdc.math.hypotl        hypot;

    ///
    alias core.stdc.math.pow           pow;
    ///
    alias core.stdc.math.powf          pow;
    ///
    alias core.stdc.math.powl          pow;

    ///
    alias core.stdc.complex.cpow       pow;
    ///
    alias core.stdc.complex.cpowf      pow;
    ///
    alias core.stdc.complex.cpowl      pow;

    ///
    alias core.stdc.math.sqrt          sqrt;
    ///
    alias core.stdc.math.sqrtf         sqrt;
    ///
    alias core.stdc.math.sqrtl         sqrt;

    ///
    alias core.stdc.complex.csqrt      sqrt;
    ///
    alias core.stdc.complex.csqrtf     sqrt;
    ///
    alias core.stdc.complex.csqrtl     sqrt;

    ///
    alias core.stdc.math.erf           erf;
    ///
    alias core.stdc.math.erff          erf;
    ///
    alias core.stdc.math.erfl          erf;

    ///
    alias core.stdc.math.erfc          erfc;
    ///
    alias core.stdc.math.erfcf         erfc;
    ///
    alias core.stdc.math.erfcl         erfc;

    ///
    alias core.stdc.math.lgamma        lgamma;
    ///
    alias core.stdc.math.lgammaf       lgamma;
    ///
    alias core.stdc.math.lgammal       lgamma;

    ///
    alias core.stdc.math.tgamma        tgamma;
    ///
    alias core.stdc.math.tgammaf       tgamma;
    ///
    alias core.stdc.math.tgammal       tgamma;

    ///
    alias core.stdc.math.ceil          ceil;
    ///
    alias core.stdc.math.ceilf         ceil;
    ///
    alias core.stdc.math.ceill         ceil;

    ///
    alias core.stdc.math.floor         floor;
    ///
    alias core.stdc.math.floorf        floor;
    ///
    alias core.stdc.math.floorl        floor;

    ///
    alias core.stdc.math.nearbyint     nearbyint;
    ///
    alias core.stdc.math.nearbyintf    nearbyint;
    ///
    alias core.stdc.math.nearbyintl    nearbyint;

    ///
    alias core.stdc.math.rint          rint;
    ///
    alias core.stdc.math.rintf         rint;
    ///
    alias core.stdc.math.rintl         rint;

    ///
    alias core.stdc.math.lrint         lrint;
    ///
    alias core.stdc.math.lrintf        lrint;
    ///
    alias core.stdc.math.lrintl        lrint;

    ///
    alias core.stdc.math.llrint        llrint;
    ///
    alias core.stdc.math.llrintf       llrint;
    ///
    alias core.stdc.math.llrintl       llrint;

    ///
    alias core.stdc.math.round         round;
    ///
    alias core.stdc.math.roundf        round;
    ///
    alias core.stdc.math.roundl        round;

    ///
    alias core.stdc.math.lround        lround;
    ///
    alias core.stdc.math.lroundf       lround;
    ///
    alias core.stdc.math.lroundl       lround;

    ///
    alias core.stdc.math.llround       llround;
    ///
    alias core.stdc.math.llroundf      llround;
    ///
    alias core.stdc.math.llroundl      llround;

    ///
    alias core.stdc.math.trunc         trunc;
    ///
    alias core.stdc.math.truncf        trunc;
    ///
    alias core.stdc.math.truncl        trunc;

    ///
    alias core.stdc.math.fmod          fmod;
    ///
    alias core.stdc.math.fmodf         fmod;
    ///
    alias core.stdc.math.fmodl         fmod;

    ///
    alias core.stdc.math.remainder     remainder;
    ///
    alias core.stdc.math.remainderf    remainder;
    ///
    alias core.stdc.math.remainderl    remainder;

    ///
    alias core.stdc.math.remquo        remquo;
    ///
    alias core.stdc.math.remquof       remquo;
    ///
    alias core.stdc.math.remquol       remquo;

    ///
    alias core.stdc.math.copysign      copysign;
    ///
    alias core.stdc.math.copysignf     copysign;
    ///
    alias core.stdc.math.copysignl     copysign;

    ///
    alias core.stdc.math.nan           nan;
    ///
    alias core.stdc.math.nanf          nan;
    ///
    alias core.stdc.math.nanl          nan;

    ///
    alias core.stdc.math.nextafter     nextafter;
    ///
    alias core.stdc.math.nextafterf    nextafter;
    ///
    alias core.stdc.math.nextafterl    nextafter;

    ///
    alias core.stdc.math.nexttoward    nexttoward;
    ///
    alias core.stdc.math.nexttowardf   nexttoward;
    ///
    alias core.stdc.math.nexttowardl   nexttoward;

    ///
    alias core.stdc.math.fdim          fdim;
    ///
    alias core.stdc.math.fdimf         fdim;
    ///
    alias core.stdc.math.fdiml         fdim;

    ///
    alias core.stdc.math.fmax          fmax;
    ///
    alias core.stdc.math.fmaxf         fmax;
    ///
    alias core.stdc.math.fmaxl         fmax;

    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fmin          fmin;
    ///
    alias core.stdc.math.fminl         fmin;

    ///
    alias core.stdc.math.fma           fma;
    ///
    alias core.stdc.math.fmaf          fma;
    ///
    alias core.stdc.math.fmal          fma;

    ///
    alias core.stdc.complex.carg       carg;
    ///
    alias core.stdc.complex.cargf      carg;
    ///
    alias core.stdc.complex.cargl      carg;

    ///
    alias core.stdc.complex.cimag      cimag;
    ///
    alias core.stdc.complex.cimagf     cimag;
    ///
    alias core.stdc.complex.cimagl     cimag;

    ///
    alias core.stdc.complex.conj       conj;
    ///
    alias core.stdc.complex.conjf      conj;
    ///
    alias core.stdc.complex.conjl      conj;

    ///
    alias core.stdc.complex.cproj      cproj;
    ///
    alias core.stdc.complex.cprojf     cproj;
    ///
    alias core.stdc.complex.cprojl     cproj;

//  alias core.stdc.complex.creal      creal;
//  alias core.stdc.complex.crealf     creal;
//  alias core.stdc.complex.creall     creal;
}

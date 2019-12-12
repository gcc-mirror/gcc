/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_complex.h.html, _complex.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/stdc/_complex.d)
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.stdc.complex;

extern (C):
@trusted: // All of these operate on floating point values only.
nothrow:
@nogc:

///
alias creal complex;
///
alias ireal imaginary;
///
cdouble cacos(cdouble z);
///
cfloat  cacosf(cfloat z);
///
creal   cacosl(creal z);

///
cdouble casin(cdouble z);
///
cfloat  casinf(cfloat z);
///
creal   casinl(creal z);

///
cdouble catan(cdouble z);
///
cfloat  catanf(cfloat z);
///
creal   catanl(creal z);

///
cdouble ccos(cdouble z);
///
cfloat  ccosf(cfloat z);
///
creal   ccosl(creal z);

///
cdouble csin(cdouble z);
///
cfloat  csinf(cfloat z);
///
creal   csinl(creal z);

///
cdouble ctan(cdouble z);
///
cfloat  ctanf(cfloat z);
///
creal   ctanl(creal z);

///
cdouble cacosh(cdouble z);
///
cfloat  cacoshf(cfloat z);
///
creal   cacoshl(creal z);

///
cdouble casinh(cdouble z);
///
cfloat  casinhf(cfloat z);
///
creal   casinhl(creal z);

///
cdouble catanh(cdouble z);
///
cfloat  catanhf(cfloat z);
///
creal   catanhl(creal z);

///
cdouble ccosh(cdouble z);
///
cfloat  ccoshf(cfloat z);
///
creal   ccoshl(creal z);

///
cdouble csinh(cdouble z);
///
cfloat  csinhf(cfloat z);
///
creal   csinhl(creal z);

///
cdouble ctanh(cdouble z);
///
cfloat  ctanhf(cfloat z);
///
creal   ctanhl(creal z);

///
cdouble cexp(cdouble z);
///
cfloat  cexpf(cfloat z);
///
creal   cexpl(creal z);

///
cdouble clog(cdouble z);
///
cfloat  clogf(cfloat z);
///
creal   clogl(creal z);

///
 double cabs(cdouble z);
 ///
 float  cabsf(cfloat z);
 ///
 real   cabsl(creal z);

 ///
cdouble cpow(cdouble x, cdouble y);
///
cfloat  cpowf(cfloat x, cfloat y);
///
creal   cpowl(creal x, creal y);

///
cdouble csqrt(cdouble z);
///
cfloat  csqrtf(cfloat z);
///
creal   csqrtl(creal z);

///
 double carg(cdouble z);
 ///
 float  cargf(cfloat z);
 ///
 real   cargl(creal z);

 ///
 double cimag(cdouble z);
 ///
 float  cimagf(cfloat z);
 ///
 real   cimagl(creal z);

 ///
cdouble conj(cdouble z);
///
cfloat  conjf(cfloat z);
///
creal   conjl(creal z);

///
cdouble cproj(cdouble z);
///
cfloat  cprojf(cfloat z);
///
creal   cprojl(creal z);

// double creal(cdouble z);
///
 float  crealf(cfloat z);
 ///
 real   creall(creal z);

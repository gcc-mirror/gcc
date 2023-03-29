// Copyright (C) 2022-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2b" }
// { dg-do link { target c++23 } }

#include <stdfloat>
#include <cmath>

template <typename T>
__attribute__((__noipa__)) void
test_functions(T *p, int *q, long int *r, long long int *s)
{
  p[0] = std::acos(p[0]);
  p[1] = std::asin(p[1]);
  p[2] = std::atan(p[2]);
  p[3] = std::cos(p[3]);
  p[4] = std::sin(p[4]);
  p[5] = std::tan(p[5]);
  p[6] = std::acosh(p[6]);
  p[7] = std::asinh(p[7]);
  p[8] = std::atanh(p[8]);
  p[9] = std::cosh(p[9]);
  p[10] = std::sinh(p[10]);
  p[11] = std::tanh(p[11]);
  p[12] = std::exp(p[12]);
  p[13] = std::exp2(p[13]);
  p[14] = std::expm1(p[14]);
  p[15] = std::log(p[15]);
  p[16] = std::log10(p[16]);
  p[17] = std::log1p(p[17]);
  p[18] = std::log2(p[18]);
  p[19] = std::logb(p[19]);
  p[20] = std::cbrt(p[20]);
  p[21] = std::fabs(p[21]);
  p[22] = std::sqrt(p[22]);
  p[23] = std::erf(p[23]);
  p[24] = std::erfc(p[24]);
  p[25] = std::lgamma(p[25]);
  p[26] = std::tgamma(p[26]);
  p[27] = std::ceil(p[27]);
  p[28] = std::floor(p[28]);
  p[29] = std::nearbyint(p[29]);
  p[30] = std::rint(p[30]);
  p[31] = std::round(p[31]);
  p[32] = std::trunc(p[32]);
  p[33] = std::atan2(p[33], p[100]);
  p[34] = std::hypot(p[34], p[101]);
  p[35] = std::pow(p[35], p[102]);
  p[36] = std::fmod(p[36], p[103]);
  p[37] = std::remainder(p[37], p[104]);
  p[38] = std::copysign(p[38], p[105]);
  p[39] = std::nextafter(p[39], p[106]);
  p[40] = std::fdim(p[40], p[107]);
  p[41] = std::fmax(p[41], p[108]);
  p[42] = std::fmin(p[42], p[109]);
  p[43] = std::atan2(p[43], p[110]);
  p[44] = std::frexp(p[44], q + 0);
  q[1] = std::ilogb(p[45]);
  p[46] = std::ldexp(p[46], q[2]);
  p[47] = std::modf(p[47], p + 111);
  p[48] = std::scalbn(p[48], q[3]);
  p[49] = std::scalbln(p[49], r[0]);
  p[50] = std::hypot(p[50], p[111], p[112]);
  r[1] = std::lrint(p[51]);
  s[0] = std::llrint(p[52]);
  r[2] = std::lround(p[53]);
  s[1] = std::llround(p[54]);
  p[55] = std::remquo(p[55], p[113], q + 4);
  p[56] = std::fma(p[56], p[114], p[115]);
  p[57] = std::lerp(p[57], p[116], p[117]);
  p[58] = std::assoc_laguerre(q[5], q[6], p[58]);
  p[59] = std::assoc_legendre(q[7], q[8], p[59]);
  p[60] = std::beta(p[60], p[118]);
  p[61] = std::comp_ellint_1(p[61]);
  p[62] = std::comp_ellint_2(p[62]);
  p[63] = std::comp_ellint_3(p[63], p[119]);
  p[64] = std::cyl_bessel_i(p[64], p[120]);
  p[65] = std::cyl_bessel_j(p[65], p[121]);
  p[66] = std::cyl_bessel_k(p[66], p[122]);
  p[67] = std::cyl_neumann(p[67], p[123]);
  p[68] = std::ellint_1(p[68], p[124]);
  p[69] = std::ellint_2(p[69], p[125]);
  p[70] = std::ellint_3(p[70], p[126], p[127]);
  p[71] = std::expint(p[71]);
  p[72] = std::hermite(q[9], p[72]);
  p[73] = std::laguerre(q[10], p[73]);
  p[74] = std::legendre(q[11], p[72]);
  p[75] = std::riemann_zeta(p[75]);
  p[76] = std::sph_bessel(q[12], p[76]);
  p[77] = std::sph_legendre(q[13], q[14], p[77]);
  p[78] = std::sph_neumann(q[15], p[78]);
}

int
main()
{
  int q[16] = {};
  long int r[16] = {};
  long long int s[16] = {};
#if defined(__STDCPP_FLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  {
    std::float16_t p[128] = {};
    test_functions(p, q, r, s);
  }
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  {
    std::float32_t p[128] = {};
    test_functions(p, q, r, s);
  }
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
  {
    std::float64_t p[128] = {};
    test_functions(p, q, r, s);
  }
#endif
#if defined(__STDCPP_FLOAT128_T__) \
    && (defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY128) \
	|| defined(_GLIBCXX_HAVE_FLOAT128_MATH))
  {
    std::float128_t p[128] = {};
    test_functions(p, q, r, s);
  }
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  {
    std::bfloat16_t p[128] = {};
    test_functions(p, q, r, s);
  }
#endif
}

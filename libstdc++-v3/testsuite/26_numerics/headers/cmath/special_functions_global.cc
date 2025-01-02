// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#undef __STDCPP_WANT_MATH_SPEC_FUNCS__
#define __STDCPP_WANT_MATH_SPEC_FUNCS__ 0
#include <math.h>

struct R { };

// When __STDCPP_WANT_MATH_SPEC_FUNCS__ == 0, none of these names should be
// declared in the global namespace by <math.h>. Calls to them should find
// the overloads declared in this file, not the ones that are present in
// namespace std for C++17 and later.

R assoc_laguerre(...);
R assoc_laguerref(...);
R assoc_laguerrel(...);
R assoc_legendre(...);
R assoc_legendref(...);
R assoc_legendrel(...);
R beta(...);
R betaf(...);
R betal(...);
R comp_ellint_1(...);
R comp_ellint_1f(...);
R comp_ellint_1l(...);
R comp_ellint_2(...);
R comp_ellint_2f(...);
R comp_ellint_2l(...);
R comp_ellint_3(...);
R comp_ellint_3f(...);
R comp_ellint_3l(...);
R cyl_bessel_i(...);
R cyl_bessel_if(...);
R cyl_bessel_il(...);
R cyl_bessel_j(...);
R cyl_bessel_jf(...);
R cyl_bessel_jl(...);
R cyl_bessel_k(...);
R cyl_bessel_kf(...);
R cyl_bessel_kl(...);
R cyl_neumann(...);
R cyl_neumannf(...);
R cyl_neumannl(...);
R ellint_1(...);
R ellint_1f(...);
R ellint_1l(...);
R ellint_2(...);
R ellint_2f(...);
R ellint_2l(...);
R ellint_3(...);
R ellint_3f(...);
R ellint_3l(...);
R expint(...);
R expintf(...);
R expintl(...);
R hermite(...);
R hermitef(...);
R hermitel(...);
R laguerre(...);
R laguerref(...);
R laguerrel(...);
R legendre(...);
R legendref(...);
R legendrel(...);
R riemann_zeta(...);
R riemann_zetaf(...);
R riemann_zetal(...);
R sph_bessel(...);
R sph_besself(...);
R sph_bessell(...);
R sph_legendre(...);
R sph_legendref(...);
R sph_legendrel(...);
R sph_neumann(...);
R sph_neumannf(...);
R sph_neumannl(...);

void
test01()
{
  R r;
  // Call each function with arguments matching the real special functions
  // in namespace std, to verify that the overloads above are called instead.
  r = assoc_laguerre(1u, 1u, 1.0);
  r = assoc_laguerref(1u, 1u, 1.0f);
  r = assoc_laguerrel(1u, 1u, 1.0l);
  r = assoc_legendre(1u, 1u, 1.0);
  r = assoc_legendref(1u, 1u, 1.0f);
  r = assoc_legendrel(1u, 1u, 1.0l);
  r = beta(1.0, 1.0);
  r = betaf(1.0f, 1.0f);
  r = betal(1.0l, 1.0l);
  r = comp_ellint_1(1.0);
  r = comp_ellint_1f(1.0f);
  r = comp_ellint_1l(1.0l);
  r = comp_ellint_2(1.0);
  r = comp_ellint_2f(1.0f);
  r = comp_ellint_2l(1.0l);
  r = comp_ellint_3(1.0, 1.0);
  r = comp_ellint_3f(1.0f, 1.0f);
  r = comp_ellint_3l(1.0l, 1.0l);
  r = cyl_bessel_i(1.0, 1.0);
  r = cyl_bessel_if(1.0f, 1.0f);
  r = cyl_bessel_il(1.0l, 1.0l);
  r = cyl_bessel_j(1.0, 1.0);
  r = cyl_bessel_jf(1.0f, 1.0f);
  r = cyl_bessel_jl(1.0l, 1.0l);
  r = cyl_bessel_k(1.0, 1.0);
  r = cyl_bessel_kf(1.0f, 1.0f);
  r = cyl_bessel_kl(1.0l, 1.0l);
  r = cyl_neumann(1.0, 1.0);
  r = cyl_neumannf(1.0f, 1.0f);
  r = cyl_neumannl(1.0l, 1.0l);
  r = ellint_1(1.0, 1.0);
  r = ellint_1f(1.0f, 1.0f);
  r = ellint_1l(1.0l, 1.0l);
  r = ellint_2(1.0, 1.0);
  r = ellint_2f(1.0f, 1.0f);
  r = ellint_2l(1.0l, 1.0l);
  r = ellint_3(1.0, 1.0, 1.0);
  r = ellint_3f(1.0f, 1.0f, 1.0f);
  r = ellint_3l(1.0l, 1.0l, 1.0l);
  r = expint(1.0);
  r = expintf(1.0f);
  r = expintl(1.0l);
  r = hermite(1u, 1.0);
  r = hermitef(1u, 1.0f);
  r = hermitel(1u, 1.0l);
  r = laguerre(1u, 1.0);
  r = laguerref(1u, 1.0f);
  r = laguerrel(1u, 1.0l);
  r = legendre(1u, 1.0);
  r = legendref(1u, 1.0f);
  r = legendrel(1u, 1.0l);
  r = riemann_zeta(1.0);
  r = riemann_zetaf(1.0f);
  r = riemann_zetal(1.0l);
  r = sph_bessel(1u, 1.0);
  r = sph_besself(1u, 1.0f);
  r = sph_bessell(1u, 1.0l);
  r = sph_legendre(1u, 1u, 1.0);
  r = sph_legendref(1u, 1u, 1.0f);
  r = sph_legendrel(1u, 1u, 1.0l);
  r = sph_neumann(1u, 1.0);
  r = sph_neumannf(1u, 1.0f);
  r = sph_neumannl(1u, 1.0l);
}

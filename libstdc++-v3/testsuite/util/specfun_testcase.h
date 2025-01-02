// Copyright (C) 2015-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

//  testcase.h

//
//  These are little PODs for special function inputs and
//  expexted results for the testsuite.
//

#ifndef _GLIBCXX_SPECFUN_TESTCASE_H
#define _GLIBCXX_SPECFUN_TESTCASE_H

//  Associated Laguerre polynomials.
template<typename _Tp>
  struct testcase_assoc_laguerre
  {
    _Tp f0;
    unsigned int n;
    unsigned int m;
    _Tp x;
    _Tp f;
  };

//  Associated Legendre functions.
template<typename _Tp>
  struct testcase_assoc_legendre
  {
    _Tp f0;
    unsigned int l;
    unsigned int m;
    _Tp x;
    _Tp f;
  };

//  Beta function.
template<typename _Tp>
  struct testcase_beta
  {
    _Tp f0;
    _Tp x;
    _Tp y;
    _Tp f;
  };

//  Complete elliptic integrals of the first kind.
template<typename _Tp>
  struct testcase_comp_ellint_1
  {
    _Tp f0;
    _Tp k;
    _Tp f;
  };

//  Complete elliptic integrals of the second kind.
template<typename _Tp>
  struct testcase_comp_ellint_2
  {
    _Tp f0;
    _Tp k;
    _Tp f;
  };

//  Complete elliptic integrals of the third kind.
template<typename _Tp>
  struct testcase_comp_ellint_3
  {
    _Tp f0;
    _Tp k;
    _Tp nu;
    _Tp f;
  };

//  Confluent hypergeometric functions.
template<typename _Tp>
  struct testcase_conf_hyperg
  {
    _Tp f0;
    _Tp a;
    _Tp c;
    _Tp x;
    _Tp f;
  };

//  Generic cylindrical Bessel functions.
template<typename _Tp>
  struct testcase_cyl_bessel
  {
    _Tp f0;
    _Tp nu;
    _Tp x;
    _Tp f;
  };

//  Regular modified cylindrical Bessel functions.
template<typename _Tp>
  struct testcase_cyl_bessel_i
  {
    _Tp f0;
    _Tp nu;
    _Tp x;
    _Tp f;
  };

//  Cylindrical Bessel functions (of the first kind).
template<typename _Tp>
  struct testcase_cyl_bessel_j
  {
    _Tp f0;
    _Tp nu;
    _Tp x;
    _Tp f;
  };

//  Irregular modified cylindrical Bessel functions.
template<typename _Tp>
  struct testcase_cyl_bessel_k
  {
    _Tp f0;
    _Tp nu;
    _Tp x;
    _Tp f;
  };

//  Cylindrical Neumann functions.
template<typename _Tp>
  struct testcase_cyl_neumann
  {
    _Tp f0;
    _Tp nu;
    _Tp x;
    _Tp f;
  };

//  Elliptic integrals of the first kind.
template<typename _Tp>
  struct testcase_ellint_1
  {
    _Tp f0;
    _Tp k;
    _Tp phi;
    _Tp f;
  };

//  Elliptic integrals of the second kind.
template<typename _Tp>
  struct testcase_ellint_2
  {
    _Tp f0;
    _Tp k;
    _Tp phi;
    _Tp f;
  };

//  Elliptic integrals of the third kind.
template<typename _Tp>
  struct testcase_ellint_3
  {
    _Tp f0;
    _Tp k;
    _Tp nu;
    _Tp phi;
    _Tp f;
  };

//  Exponential integral.
template<typename _Tp>
  struct testcase_expint
  {
    _Tp f0;
    _Tp x;
    _Tp f;
  };

//  Hermite polynomials
template<typename _Tp>
  struct testcase_hermite
  {
    _Tp f0;
    unsigned int n;
    _Tp x;
    _Tp f;
  };

//  Hypergeometric functions.
template<typename _Tp>
  struct testcase_hyperg
  {
    _Tp f0;
    _Tp a;
    _Tp b;
    _Tp c;
    _Tp x;
    _Tp f;
  };

//  Laguerre polynomials.
template<typename _Tp>
  struct testcase_laguerre
  {
    _Tp f0;
    unsigned int n;
    _Tp x;
    _Tp f;
  };

//  Legendre polynomials.
template<typename _Tp>
  struct testcase_legendre
  {
    _Tp f0;
    unsigned int l;
    _Tp x;
    _Tp f;
  };

//  Riemann zeta function.
template<typename _Tp>
  struct testcase_riemann_zeta
  {
    _Tp f0;
    _Tp s;
    _Tp f;
  };

//  Hurwitz zeta function.
template<typename _Tp>
  struct testcase_hurwitz_zeta
  {
    _Tp f0;
    _Tp s;
    _Tp a;
    _Tp f;
  };

//  Spherical Bessel functions.
template<typename _Tp>
  struct testcase_sph_bessel
  {
    _Tp f0;
    unsigned int n;
    _Tp x;
    _Tp f;
  };

//  Regular modified spherical Bessel functions.
template<typename _Tp>
  struct testcase_sph_bessel_i
  {
    _Tp f0;
    unsigned int n;
    _Tp x;
    _Tp f;
  };

//  Irregular modified spherical Bessel functions.
template<typename _Tp>
  struct testcase_sph_bessel_k
  {
    _Tp f0;
    unsigned int n;
    _Tp x;
    _Tp f;
  };

//  Spherical Legendre functions.
template<typename _Tp>
  struct testcase_sph_legendre
  {
    _Tp f0;
    unsigned int l;
    unsigned int m;
    _Tp theta;
    _Tp f;
  };

//  Spherical Neumann functions.
template<typename _Tp>
  struct testcase_sph_neumann
  {
    _Tp f0;
    unsigned int n;
    _Tp x;
    _Tp f;
  };

//  Airy Ai functions.
template<typename _Tp>
  struct testcase_airy_ai
  {
    _Tp f0;
    _Tp x;
    _Tp f;
  };

//  Airy Bi functions.
template<typename _Tp>
  struct testcase_airy_bi
  {
    _Tp f0;
    _Tp x;
    _Tp f;
  };

//  Upper incomplete gamma functions.
template<typename _Tp>
  struct testcase_gamma_u
  {
    _Tp f0;
    _Tp n;
    _Tp x;
    _Tp f;
  };

//  Lower incomplete gamma functions.
template<typename _Tp>
  struct testcase_gamma_l
  {
    _Tp f0;
    _Tp n;
    _Tp x;
    _Tp f;
  };

//  Dilogarithm functions.
template<typename _Tp>
  struct testcase_dilog
  {
    _Tp f0;
    _Tp x;
    _Tp f;
  };

//  Digamma functions.
template<typename _Tp>
  struct testcase_gamma
  {
    _Tp f0;
    _Tp x;
    _Tp f;
  };

template<typename _Tp>
  struct testcase_comp_ellint_rf
  {
    _Tp f0;
    _Tp x;
    _Tp y;
    _Tp f;
  };

template<typename _Tp>
  struct testcase_ellint_rf
  {
    _Tp f0;
    _Tp x;
    _Tp y;
    _Tp z;
    _Tp f;
  };

template<typename _Tp>
  struct testcase_ellint_rj
  {
    _Tp f0;
    _Tp x;
    _Tp y;
    _Tp z;
    _Tp p;
    _Tp f;
  };

template<typename _Tp>
  struct testcase_ellint_rd
  {
    _Tp f0;
    _Tp x;
    _Tp y;
    _Tp z;
    _Tp f;
  };

template<typename _Tp>
  struct testcase_comp_ellint_rg
  {
    _Tp f0;
    _Tp x;
    _Tp y;
    _Tp f;
  };

template<typename _Tp>
  struct testcase_ellint_rg
  {
    _Tp f0;
    _Tp x;
    _Tp y;
    _Tp z;
    _Tp f;
  };

#endif // _GLIBCXX_SPECFUN_TESTCASE_H

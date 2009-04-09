// 2007-02-04  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
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

//  5.2.1.1  Associated Laguerre polynomials.
template <typename _Tp>
struct testcase_assoc_laguerre
{
  _Tp f0;
  unsigned int n;
  unsigned int m;
  _Tp x;
  _Tp f;
};

//  5.2.1.2  Associated Legendre functions.
template <typename _Tp>
struct testcase_assoc_legendre
{
  _Tp f0;
  unsigned int l;
  unsigned int m;
  _Tp x;
  _Tp f;
};

//  5.2.1.3  Beta function.
template <typename _Tp>
struct testcase_beta
{
  _Tp f0;
  _Tp x;
  _Tp y;
  _Tp f;
};

//  5.2.1.4  Complete elliptic integrals of the first kind.
template <typename _Tp>
struct testcase_comp_ellint_1
{
  _Tp f0;
  _Tp k;
  _Tp f;
};

//  5.2.1.5  Complete elliptic integrals of the second kind.
template <typename _Tp>
struct testcase_comp_ellint_2
{
  _Tp f0;
  _Tp k;
  _Tp f;
};

//  5.2.1.6  Complete elliptic integrals of the third kind.
template <typename _Tp>
struct testcase_comp_ellint_3
{
  _Tp f0;
  _Tp k;
  _Tp nu;
  _Tp f;
};

//  5.2.1.7  Confluent hypergeometric functions.
template <typename _Tp>
struct testcase_conf_hyperg
{
  _Tp f0;
  _Tp a;
  _Tp c;
  _Tp x;
  _Tp f;
};

//  5.2.1.8  Regular modified cylindrical Bessel functions.
template <typename _Tp>
struct testcase_cyl_bessel_i
{
  _Tp f0;
  _Tp nu;
  _Tp x;
  _Tp f;
};

//  5.2.1.9  Cylindrical Bessel functions (of the first kind).
template <typename _Tp>
struct testcase_cyl_bessel_j
{
  _Tp f0;
  _Tp nu;
  _Tp x;
  _Tp f;
};

//  5.2.1.10  Irregular modified cylindrical Bessel functions.
template <typename _Tp>
struct testcase_cyl_bessel_k
{
  _Tp f0;
  _Tp nu;
  _Tp x;
  _Tp f;
};

//  5.2.1.11  Cylindrical Neumann functions.
template <typename _Tp>
struct testcase_cyl_neumann
{
  _Tp f0;
  _Tp nu;
  _Tp x;
  _Tp f;
};

//  5.2.1.12  Elliptic integrals of the first kind.
template <typename _Tp>
struct testcase_ellint_1
{
  _Tp f0;
  _Tp k;
  _Tp phi;
  _Tp f;
};

//  5.2.1.13  Elliptic integrals of the second kind.
template <typename _Tp>
struct testcase_ellint_2
{
  _Tp f0;
  _Tp k;
  _Tp phi;
  _Tp f;
};

//  5.2.1.14  Elliptic integrals of the third kind.
template <typename _Tp>
struct testcase_ellint_3
{
  _Tp f0;
  _Tp k;
  _Tp nu;
  _Tp phi;
  _Tp f;
};

//  5.2.1.15  Exponential integral.
template <typename _Tp>
struct testcase_expint
{
  _Tp f0;
  _Tp x;
  _Tp f;
};

//  5.2.1.16  Hermite polynomials
template <typename _Tp>
struct testcase_hermite
{
  _Tp f0;
  unsigned int n;
  _Tp x;
  _Tp f;
};

//  5.2.1.17  Hypergeometric functions.
template <typename _Tp>
struct testcase_hyperg
{
  _Tp f0;
  _Tp a;
  _Tp b;
  _Tp c;
  _Tp x;
  _Tp f;
};

//  5.2.1.18  Laguerre polynomials.
template <typename _Tp>
struct testcase_laguerre
{
  _Tp f0;
  unsigned int n;
  _Tp x;
  _Tp f;
};

//  5.2.1.19  Legendre polynomials.
template <typename _Tp>
struct testcase_legendre
{
  _Tp f0;
  unsigned int l;
  _Tp x;
  _Tp f;
};

//  5.2.1.20  Riemann zeta function.
template <typename _Tp>
struct testcase_riemann_zeta
{
  _Tp f0;
  _Tp x;
  _Tp f;
};

//  5.2.1.21  Spherical Bessel functions.
template <typename _Tp>
struct testcase_sph_bessel
{
  _Tp f0;
  unsigned int n;
  _Tp x;
  _Tp f;
};

//  5.2.1.22  Spherical Legendre functions.
template <typename _Tp>
struct testcase_sph_legendre
{
  _Tp f0;
  unsigned int l;
  unsigned int m;
  _Tp theta;
  _Tp f;
};

//  5.2.1.23  Spherical Neumann functions.
template <typename _Tp>
struct testcase_sph_neumann
{
  _Tp f0;
  unsigned int n;
  _Tp x;
  _Tp f;
};

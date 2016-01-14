// Mathematical Special Functions for -*- C++ -*-

// Copyright (C) 2006-2015 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/specfun.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{cmath}
 */

#ifndef _GLIBCXX_BITS_SPECFUN_H
#define _GLIBCXX_BITS_SPECFUN_H 1

#pragma GCC visibility push(default)

#include <bits/c++config.h>

#if __STDCPP_WANT_MATH_SPEC_FUNCS__ == 0
# error include <cmath> and define __STDCPP_WANT_MATH_SPEC_FUNCS__
#endif

#define __STDCPP_MATH_SPEC_FUNCS__ 201003L

#include <bits/stl_algobase.h>
#include <limits>
#include <type_traits>

#include <tr1/gamma.tcc>
#include <tr1/bessel_function.tcc>
#include <tr1/beta_function.tcc>
#include <tr1/ell_integral.tcc>
#include <tr1/exp_integral.tcc>
#include <tr1/hypergeometric.tcc>
#include <tr1/legendre_function.tcc>
#include <tr1/modified_bessel_func.tcc>
#include <tr1/poly_hermite.tcc>
#include <tr1/poly_laguerre.tcc>
#include <tr1/riemann_zeta.tcc>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @defgroup mathsf Mathematical Special Functions
   * @ingroup numerics
   *
   * A collection of advanced mathematical special functions,
   * defined by ISO/IEC IS 29124.
   * @{
   */

  // Associated Laguerre polynomials

  inline float
  assoc_laguerref(unsigned int __n, unsigned int __m, float __x)
  { return __detail::__assoc_laguerre<float>(__n, __m, __x); }

  inline long double
  assoc_laguerrel(unsigned int __n, unsigned int __m, long double __x)
  { return __detail::__assoc_laguerre<long double>(__n, __m, __x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    assoc_laguerre(unsigned int __n, unsigned int __m, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__assoc_laguerre<__type>(__n, __m, __x);
    }

  // Associated Legendre functions

  inline float
  assoc_legendref(unsigned int __l, unsigned int __m, float __x)
  { return __detail::__assoc_legendre_p<float>(__l, __m, __x); }

  inline long double
  assoc_legendrel(unsigned int __l, unsigned int __m, long double __x)
  { return __detail::__assoc_legendre_p<long double>(__l, __m, __x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    assoc_legendre(unsigned int __l, unsigned int __m, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__assoc_legendre_p<__type>(__l, __m, __x);
    }

  // Beta functions

  inline float
  betaf(float __x, float __y)
  { return __detail::__beta<float>(__x, __y); }

  inline long double
  betal(long double __x, long double __y)
  { return __detail::__beta<long double>(__x, __y); }

  template<typename _Tpx, typename _Tpy>
    inline typename __gnu_cxx::__promote_2<_Tpx, _Tpy>::__type
    beta(_Tpx __x, _Tpy __y)
    {
      typedef typename __gnu_cxx::__promote_2<_Tpx, _Tpy>::__type __type;
      return __detail::__beta<__type>(__x, __y);
    }

  // Complete elliptic integrals of the first kind

  inline float
  comp_ellint_1f(float __k)
  { return __detail::__comp_ellint_1<float>(__k); }

  inline long double
  comp_ellint_1l(long double __k)
  { return __detail::__comp_ellint_1<long double>(__k); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    comp_ellint_1(_Tp __k)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__comp_ellint_1<__type>(__k);
    }

  // Complete elliptic integrals of the second kind

  inline float
  comp_ellint_2f(float __k)
  { return __detail::__comp_ellint_2<float>(__k); }

  inline long double
  comp_ellint_2l(long double __k)
  { return __detail::__comp_ellint_2<long double>(__k); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    comp_ellint_2(_Tp __k)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__comp_ellint_2<__type>(__k);
    }

  // Complete elliptic integrals of the third kind

  inline float
  comp_ellint_3f(float __k, float __nu)
  { return __detail::__comp_ellint_3<float>(__k, __nu); }

  inline long double
  comp_ellint_3l(long double __k, long double __nu)
  { return __detail::__comp_ellint_3<long double>(__k, __nu); }

  template<typename _Tp, typename _Tpn>
    inline typename __gnu_cxx::__promote_2<_Tp, _Tpn>::__type
    comp_ellint_3(_Tp __k, _Tpn __nu)
    {
      typedef typename __gnu_cxx::__promote_2<_Tp, _Tpn>::__type __type;
      return __detail::__comp_ellint_3<__type>(__k, __nu);
    }

  // Regular modified cylindrical Bessel functions

  inline float
  cyl_bessel_if(float __nu, float __x)
  { return __detail::__cyl_bessel_i<float>(__nu, __x); }

  inline long double
  cyl_bessel_il(long double __nu, long double __x)
  { return __detail::__cyl_bessel_i<long double>(__nu, __x); }

  template<typename _Tpnu, typename _Tp>
    inline typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type
    cyl_bessel_i(_Tpnu __nu, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type __type;
      return __detail::__cyl_bessel_i<__type>(__nu, __x);
    }

  // Cylindrical Bessel functions (of the first kind)

  inline float
  cyl_bessel_jf(float __nu, float __x)
  { return __detail::__cyl_bessel_j<float>(__nu, __x); }

  inline long double
  cyl_bessel_jl(long double __nu, long double __x)
  { return __detail::__cyl_bessel_j<long double>(__nu, __x); }

  template<typename _Tpnu, typename _Tp>
    inline typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type
    cyl_bessel_j(_Tpnu __nu, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type __type;
      return __detail::__cyl_bessel_j<__type>(__nu, __x);
    }

  // Irregular modified cylindrical Bessel functions

  inline float
  cyl_bessel_kf(float __nu, float __x)
  { return __detail::__cyl_bessel_k<float>(__nu, __x); }

  inline long double
  cyl_bessel_kl(long double __nu, long double __x)
  { return __detail::__cyl_bessel_k<long double>(__nu, __x); }

  template<typename _Tpnu, typename _Tp>
    inline typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type
    cyl_bessel_k(_Tpnu __nu, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type __type;
      return __detail::__cyl_bessel_k<__type>(__nu, __x);
    }

  // Cylindrical Neumann functions

  inline float
  cyl_neumannf(float __nu, float __x)
  { return __detail::__cyl_neumann_n<float>(__nu, __x); }

  inline long double
  cyl_neumannl(long double __nu, long double __x)
  { return __detail::__cyl_neumann_n<long double>(__nu, __x); }

  template<typename _Tpnu, typename _Tp>
    inline typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type
    cyl_neumann(_Tpnu __nu, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote_2<_Tpnu, _Tp>::__type __type;
      return __detail::__cyl_neumann_n<__type>(__nu, __x);
    }

  // Incomplete elliptic integrals of the first kind

  inline float
  ellint_1f(float __k, float __phi)
  { return __detail::__ellint_1<float>(__k, __phi); }

  inline long double
  ellint_1l(long double __k, long double __phi)
  { return __detail::__ellint_1<long double>(__k, __phi); }

  template<typename _Tp, typename _Tpp>
    inline typename __gnu_cxx::__promote_2<_Tp, _Tpp>::__type
    ellint_1(_Tp __k, _Tpp __phi)
    {
      typedef typename __gnu_cxx::__promote_2<_Tp, _Tpp>::__type __type;
      return __detail::__ellint_1<__type>(__k, __phi);
    }

  // Incomplete elliptic integrals of the second kind

  inline float
  ellint_2f(float __k, float __phi)
  { return __detail::__ellint_2<float>(__k, __phi); }

  inline long double
  ellint_2l(long double __k, long double __phi)
  { return __detail::__ellint_2<long double>(__k, __phi); }

  template<typename _Tp, typename _Tpp>
    inline typename __gnu_cxx::__promote_2<_Tp, _Tpp>::__type
    ellint_2(_Tp __k, _Tpp __phi)
    {
      typedef typename __gnu_cxx::__promote_2<_Tp, _Tpp>::__type __type;
      return __detail::__ellint_2<__type>(__k, __phi);
    }

  // Incomplete elliptic integrals of the third kind

  inline float
  ellint_3f(float __k, float __nu, float __phi)
  { return __detail::__ellint_3<float>(__k, __nu, __phi); }

  inline long double
  ellint_3l(long double __k, long double __nu, long double __phi)
  { return __detail::__ellint_3<long double>(__k, __nu, __phi); }

  template<typename _Tp, typename _Tpn, typename _Tpp>
    inline typename __gnu_cxx::__promote_3<_Tp, _Tpn, _Tpp>::__type
    ellint_3(_Tp __k, _Tpn __nu, _Tpp __phi)
    {
      typedef typename __gnu_cxx::__promote_3<_Tp, _Tpn, _Tpp>::__type __type;
      return __detail::__ellint_3<__type>(__k, __nu, __phi);
    }

  // Exponential integrals

  inline float
  expintf(float __x)
  { return __detail::__expint<float>(__x); }

  inline long double
  expintl(long double __x)
  { return __detail::__expint<long double>(__x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    expint(_Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__expint<__type>(__x);
    }

  // Hermite polynomials

  inline float
  hermitef(unsigned int __n, float __x)
  { return __detail::__poly_hermite<float>(__n, __x); }

  inline long double
  hermitel(unsigned int __n, long double __x)
  { return __detail::__poly_hermite<long double>(__n, __x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    hermite(unsigned int __n, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__poly_hermite<__type>(__n, __x);
    }

  // Laguerre polynomials

  inline float
  laguerref(unsigned int __n, float __x)
  { return __detail::__laguerre<float>(__n, __x); }

  inline long double
  laguerrel(unsigned int __n, long double __x)
  { return __detail::__laguerre<long double>(__n, __x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    laguerre(unsigned int __n, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__laguerre<__type>(__n, __x);
    }

  // Legendre polynomials

  inline float
  legendref(unsigned int __n, float __x)
  { return __detail::__poly_legendre_p<float>(__n, __x); }

  inline long double
  legendrel(unsigned int __n, long double __x)
  { return __detail::__poly_legendre_p<long double>(__n, __x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    legendre(unsigned int __n, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__poly_legendre_p<__type>(__n, __x);
    }

  // Riemann zeta functions

  inline float
  riemann_zetaf(float __s)
  { return __detail::__riemann_zeta<float>(__s); }

  inline long double
  riemann_zetal(long double __s)
  { return __detail::__riemann_zeta<long double>(__s); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    riemann_zeta(_Tp __s)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__riemann_zeta<__type>(__s);
    }

  // Spherical Bessel functions

  inline float
  sph_besself(unsigned int __n, float __x)
  { return __detail::__sph_bessel<float>(__n, __x); }

  inline long double
  sph_bessell(unsigned int __n, long double __x)
  { return __detail::__sph_bessel<long double>(__n, __x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    sph_bessel(unsigned int __n, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__sph_bessel<__type>(__n, __x);
    }

  // Spherical associated Legendre functions

  inline float
  sph_legendref(unsigned int __l, unsigned int __m, float __theta)
  { return __detail::__sph_legendre<float>(__l, __m, __theta); }

  inline long double
  sph_legendrel(unsigned int __l, unsigned int __m, long double __theta)
  { return __detail::__sph_legendre<long double>(__l, __m, __theta); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    sph_legendre(unsigned int __l, unsigned int __m, _Tp __theta)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__sph_legendre<__type>(__l, __m, __theta);
    }

  // Spherical Neumann functions

  inline float
  sph_neumannf(unsigned int __n, float __x)
  { return __detail::__sph_neumann<float>(__n, __x); }

  inline long double
  sph_neumannl(unsigned int __n, long double __x)
  { return __detail::__sph_neumann<long double>(__n, __x); }

  template<typename _Tp>
    inline typename __gnu_cxx::__promote<_Tp>::__type
    sph_neumann(unsigned int __n, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote<_Tp>::__type __type;
      return __detail::__sph_neumann<__type>(__n, __x);
    }

  // @} group mathsf

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{

  // Confluent hypergeometric functions

  inline float
  conf_hypergf(float __a, float __c, float __x)
  { return std::__detail::__conf_hyperg<float>(__a, __c, __x); }

  inline long double
  conf_hypergl(long double __a, long double __c, long double __x)
  { return std::__detail::__conf_hyperg<long double>(__a, __c, __x); }

  template<typename _Tpa, typename _Tpc, typename _Tp>
    inline typename __gnu_cxx::__promote_3<_Tpa, _Tpc, _Tp>::__type
    conf_hyperg(_Tpa __a, _Tpc __c, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote_3<_Tpa, _Tpc, _Tp>::__type __type;
      return std::__detail::__conf_hyperg<__type>(__a, __c, __x);
    }

  // Hypergeometric functions

  inline float
  hypergf(float __a, float __b, float __c, float __x)
  { return std::__detail::__hyperg<float>(__a, __b, __c, __x); }

  inline long double
  hypergl(long double __a, long double __b, long double __c, long double __x)
  { return std::__detail::__hyperg<long double>(__a, __b, __c, __x); }

  template<typename _Tpa, typename _Tpb, typename _Tpc, typename _Tp>
    inline typename __gnu_cxx::__promote_4<_Tpa, _Tpb, _Tpc, _Tp>::__type
    hyperg(_Tpa __a, _Tpb __b, _Tpc __c, _Tp __x)
    {
      typedef typename __gnu_cxx::__promote_4<_Tpa, _Tpb, _Tpc, _Tp>
		::__type __type;
      return std::__detail::__hyperg<__type>(__a, __b, __c, __x);
    }

} // namespace __gnu_cxx

#pragma GCC visibility pop

#endif // _GLIBCXX_BITS_SPECFUN_H

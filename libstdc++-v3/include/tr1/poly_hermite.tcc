// Special functions -*- C++ -*-

// Copyright (C) 2006, 2007, 2008
// Free Software Foundation, Inc.
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
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file tr1/poly_hermite.tcc
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

//
// ISO C++ 14882 TR1: 5.2  Special functions
//

// Written by Edward Smith-Rowland based on:
//   (1) Handbook of Mathematical Functions,
//       Ed. Milton Abramowitz and Irene A. Stegun,
//       Dover Publications, Section 22 pp. 773-802

#ifndef _GLIBCXX_TR1_POLY_HERMITE_TCC
#define _GLIBCXX_TR1_POLY_HERMITE_TCC 1

namespace std
{
namespace tr1
{

  // [5.2] Special functions

  // Implementation-space details.
  namespace __detail
  {

    /**
     *   @brief This routine returns the Hermite polynomial
     *          of order n: \f$ H_n(x) \f$ by recursion on n.
     * 
     *   The Hermite polynomial is defined by:
     *   @f[
     *     H_n(x) = (-1)^n e^{x^2} \frac{d^n}{dx^n} e^{-x^2}
     *   @f]
     *
     *   @param __n The order of the Hermite polynomial.
     *   @param __x The argument of the Hermite polynomial.
     *   @return The value of the Hermite polynomial of order n
     *           and argument x.
     */
    template<typename _Tp>
    _Tp
    __poly_hermite_recursion(const unsigned int __n, const _Tp __x)
    {
      //  Compute H_0.
      _Tp __H_0 = 1;
      if (__n == 0)
        return __H_0;

      //  Compute H_1.
      _Tp __H_1 = 2 * __x;
      if (__n == 1)
        return __H_1;

      //  Compute H_n.
      _Tp __H_n, __H_nm1, __H_nm2;
      unsigned int __i;
      for  (__H_nm2 = __H_0, __H_nm1 = __H_1, __i = 2; __i <= __n; ++__i)
        {
          __H_n = 2 * (__x * __H_nm1 + (__i - 1) * __H_nm2);
          __H_nm2 = __H_nm1;
          __H_nm1 = __H_n;
        }

      return __H_n;
    }


    /**
     *   @brief This routine returns the Hermite polynomial
     *          of order n: \f$ H_n(x) \f$.
     * 
     *   The Hermite polynomial is defined by:
     *   @f[
     *     H_n(x) = (-1)^n e^{x^2} \frac{d^n}{dx^n} e^{-x^2}
     *   @f]
     *
     *   @param __n The order of the Hermite polynomial.
     *   @param __x The argument of the Hermite polynomial.
     *   @return The value of the Hermite polynomial of order n
     *           and argument x.
     */
    template<typename _Tp>
    inline _Tp
    __poly_hermite(const unsigned int __n, const _Tp __x)
    {
      if (__isnan(__x))
        return std::numeric_limits<_Tp>::quiet_NaN();
      else
        return __poly_hermite_recursion(__n, __x);
    }

  } // namespace std::tr1::__detail
}
}

#endif // _GLIBCXX_TR1_POLY_HERMITE_TCC

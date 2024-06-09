// Compatibility symbols for previous versions -*- C++ -*-

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

/** @file bits/compatibility.h
 *  This is an internal header file, included by other library sources.
 *  You should not attempt to use it directly.
 */

// Switch for symbol version macro.
#ifndef _GLIBCXX_APPLY_SYMVER
#error must define _GLIBCXX_APPLY_SYMVER before including __FILE__
#endif

/* gcc-3.4.4
_ZNSt19istreambuf_iteratorIcSt11char_traitsIcEEppEv
_ZNSt19istreambuf_iteratorIwSt11char_traitsIwEEppEv
 */
namespace
{
_GLIBCXX_APPLY_SYMVER(_ZNSt21istreambuf_iteratorXXIcSt11char_traitsIcEEppEv,
		      _ZNSt19istreambuf_iteratorIcSt11char_traitsIcEEppEv)

#ifdef _GLIBCXX_USE_WCHAR_T
_GLIBCXX_APPLY_SYMVER(_ZNSt21istreambuf_iteratorXXIwSt11char_traitsIwEEppEv,
		      _ZNSt19istreambuf_iteratorIwSt11char_traitsIwEEppEv)
#endif
} // anonymous namespace

/* gcc-4.0.0
_ZNSs4_Rep26_M_set_length_and_sharableEj
_ZNSs7_M_copyEPcPKcj
_ZNSs7_M_moveEPcPKcj
_ZNSs9_M_assignEPcjc
_ZNKSs11_M_disjunctEPKc
_ZNKSs15_M_check_lengthEjjPKc
_ZNSbIwSt11char_traitsIwESaIwEE4_Rep26_M_set_length_and_sharableEj
_ZNSbIwSt11char_traitsIwESaIwEE7_M_copyEPwPKwj
_ZNSbIwSt11char_traitsIwESaIwEE7_M_moveEPwPKwj
_ZNSbIwSt11char_traitsIwESaIwEE9_M_assignEPwjw
_ZNKSbIwSt11char_traitsIwESaIwEE11_M_disjunctEPKw
_ZNKSbIwSt11char_traitsIwESaIwEE15_M_check_lengthEjjPKc

_ZNKSt13basic_fstreamIcSt11char_traitsIcEE7is_openEv
_ZNKSt13basic_fstreamIwSt11char_traitsIwEE7is_openEv
_ZNKSt14basic_ifstreamIcSt11char_traitsIcEE7is_openEv
_ZNKSt14basic_ifstreamIwSt11char_traitsIwEE7is_openEv
_ZNKSt14basic_ofstreamIcSt11char_traitsIcEE7is_openEv
_ZNKSt14basic_ofstreamIwSt11char_traitsIwEE7is_openEv

_ZNSi6ignoreEi
_ZNSi6ignoreEv
_ZNSt13basic_istreamIwSt11char_traitsIwEE6ignoreEi
_ZNSt13basic_istreamIwSt11char_traitsIwEE6ignoreEv

_ZNSt11char_traitsIcE2eqERKcS2_
_ZNSt11char_traitsIwE2eqERKwS2_
 */
namespace
{
_GLIBCXX_APPLY_SYMVER(_ZNSt11char_traitsIcE4eqXXERKcS2_,
		      _ZNSt11char_traitsIcE2eqERKcS2_)

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNSs9_M_copyXXEPcPKcj,
		      _ZNSs7_M_copyEPcPKcj)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSs9_M_copyXXEPcPKcm,
		      _ZNSs7_M_copyEPcPKcm)
#endif

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNSs9_M_moveXXEPcPKcj,
		      _ZNSs7_M_moveEPcPKcj)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSs9_M_moveXXEPcPKcm,
		      _ZNSs7_M_moveEPcPKcm)
#endif

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNSs11_M_assignXXEPcjc,
		      _ZNSs9_M_assignEPcjc)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSs11_M_assignXXEPcmc,
		      _ZNSs9_M_assignEPcmc)
#endif

_GLIBCXX_APPLY_SYMVER(_ZNKSs13_M_disjunctXXEPKc,
		      _ZNKSs11_M_disjunctEPKc)

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNKSs17_M_check_lengthXXEjjPKc,
		      _ZNKSs15_M_check_lengthEjjPKc)
#else
_GLIBCXX_APPLY_SYMVER(_ZNKSs17_M_check_lengthXXEmmPKc,
		      _ZNKSs15_M_check_lengthEmmPKc)
#endif

#ifdef _GLIBCXX_SIZE_T_IS_UINT
  _GLIBCXX_APPLY_SYMVER(_ZNSs4_Rep28_M_set_length_and_sharableXXEj,
			_ZNSs4_Rep26_M_set_length_and_sharableEj)
#else
  _GLIBCXX_APPLY_SYMVER(_ZNSs4_Rep28_M_set_length_and_sharableXXEm,
			_ZNSs4_Rep26_M_set_length_and_sharableEm)
#endif

_GLIBCXX_APPLY_SYMVER(_ZNSi8ignoreXXEv, _ZNSi6ignoreEv)

#ifdef _GLIBCXX_PTRDIFF_T_IS_INT
_GLIBCXX_APPLY_SYMVER(_ZNSi8ignoreXXEi, _ZNSi6ignoreEi)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSi8ignoreXXEl, _ZNSi6ignoreEl)
#endif

_GLIBCXX_APPLY_SYMVER(_ZNKSt15basic_fstreamXXIcSt11char_traitsIcEE7is_openEv,
		      _ZNKSt13basic_fstreamIcSt11char_traitsIcEE7is_openEv)

_GLIBCXX_APPLY_SYMVER(_ZNKSt16basic_ifstreamXXIcSt11char_traitsIcEE7is_openEv,
		      _ZNKSt14basic_ifstreamIcSt11char_traitsIcEE7is_openEv)

_GLIBCXX_APPLY_SYMVER(_ZNKSt16basic_ofstreamXXIcSt11char_traitsIcEE7is_openEv,
		      _ZNKSt14basic_ofstreamIcSt11char_traitsIcEE7is_openEv)

  // Support for wchar_t.
#ifdef _GLIBCXX_USE_WCHAR_T
_GLIBCXX_APPLY_SYMVER(_ZNSt11char_traitsIwE4eqXXERKwS2_,
		      _ZNSt11char_traitsIwE2eqERKwS2_)

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE9_M_copyXXEPwPKwj,
		      _ZNSbIwSt11char_traitsIwESaIwEE7_M_copyEPwPKwj)
#else
  _GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE9_M_copyXXEPwPKwm,
			_ZNSbIwSt11char_traitsIwESaIwEE7_M_copyEPwPKwm)
#endif

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE9_M_moveXXEPwPKwj,
		      _ZNSbIwSt11char_traitsIwESaIwEE7_M_moveEPwPKwj)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE9_M_moveXXEPwPKwm,
		      _ZNSbIwSt11char_traitsIwESaIwEE7_M_moveEPwPKwm)
#endif

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE11_M_assignXXEPwjw,
		      _ZNSbIwSt11char_traitsIwESaIwEE9_M_assignEPwjw)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE11_M_assignXXEPwmw,
		      _ZNSbIwSt11char_traitsIwESaIwEE9_M_assignEPwmw)
#endif

_GLIBCXX_APPLY_SYMVER(_ZNKSbIwSt11char_traitsIwESaIwEE13_M_disjunctXXEPKw,
		      _ZNKSbIwSt11char_traitsIwESaIwEE11_M_disjunctEPKw)

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNKSbIwSt11char_traitsIwESaIwEE17_M_check_lengthXXEjjPKc,
		      _ZNKSbIwSt11char_traitsIwESaIwEE15_M_check_lengthEjjPKc)
#else
_GLIBCXX_APPLY_SYMVER(_ZNKSbIwSt11char_traitsIwESaIwEE17_M_check_lengthXXEmmPKc,
		      _ZNKSbIwSt11char_traitsIwESaIwEE15_M_check_lengthEmmPKc)
#endif

#ifdef _GLIBCXX_SIZE_T_IS_UINT
_GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE4_Rep28_M_set_length_and_sharableXXEj,
		      _ZNSbIwSt11char_traitsIwESaIwEE4_Rep26_M_set_length_and_sharableEj)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSbIwSt11char_traitsIwESaIwEE4_Rep28_M_set_length_and_sharableXXEm,
		      _ZNSbIwSt11char_traitsIwESaIwEE4_Rep26_M_set_length_and_sharableEm)
#endif

_GLIBCXX_APPLY_SYMVER(_ZNSt13basic_istreamIwSt11char_traitsIwEE8ignoreXXEv,
		      _ZNSt13basic_istreamIwSt11char_traitsIwEE6ignoreEv)

#ifdef _GLIBCXX_PTRDIFF_T_IS_INT
_GLIBCXX_APPLY_SYMVER(_ZNSt13basic_istreamIwSt11char_traitsIwEE8ignoreXXEi,
		      _ZNSt13basic_istreamIwSt11char_traitsIwEE6ignoreEi)
#else
_GLIBCXX_APPLY_SYMVER(_ZNSt13basic_istreamIwSt11char_traitsIwEE8ignoreXXEl,
		      _ZNSt13basic_istreamIwSt11char_traitsIwEE6ignoreEl)
#endif

_GLIBCXX_APPLY_SYMVER(_ZNKSt15basic_fstreamXXIwSt11char_traitsIwEE7is_openEv,
		      _ZNKSt13basic_fstreamIwSt11char_traitsIwEE7is_openEv)

_GLIBCXX_APPLY_SYMVER(_ZNKSt16basic_ifstreamXXIwSt11char_traitsIwEE7is_openEv,
		      _ZNKSt14basic_ifstreamIwSt11char_traitsIwEE7is_openEv)

_GLIBCXX_APPLY_SYMVER(_ZNKSt16basic_ofstreamXXIwSt11char_traitsIwEE7is_openEv,
		      _ZNKSt14basic_ofstreamIwSt11char_traitsIwEE7is_openEv)
#endif
  } // anonymous namespace


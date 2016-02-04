// std::tr1::hash definitions, long double bits -*- C++ -*-

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

namespace std _GLIBCXX_VISIBILITY(default)
{
  namespace tr1 
  {
    // For long double, careful with random padding bits (e.g., on x86,
    // 10 bytes -> 12 bytes) and resort to frexp.
    template<>
      size_t
      hash<long double>::operator()(long double __val) const
      {
	// 0 and -0 both hash to zero.
	if (__val == 0.0L)
	  return 0;

	int __exponent;
	__val = __builtin_frexpl(__val, &__exponent);
	__val = __val < 0.0l ? -(__val + 0.5l) : __val;

	const long double __mult = __SIZE_MAX__ + 1.0l;
	__val *= __mult;

	// Try to use all the bits of the mantissa (really necessary only
	// on 32-bit targets, at least for 80-bit floating point formats).
	const size_t __hibits = (size_t)__val;
	__val = (__val - (long double)__hibits) * __mult;

	const size_t __coeff = __SIZE_MAX__ / __LDBL_MAX_EXP__;

	return __hibits + (size_t)__val + __coeff * __exponent;
      }
  }
}

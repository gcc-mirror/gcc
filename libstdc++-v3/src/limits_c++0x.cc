// std::limits definitions -*- C++ -*-

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

#include <limits>

#ifndef __GXX_EXPERIMENTAL_CXX0X__
# error "limits_c++0x.cc must be compiled with -std=gnu++0x"
#endif

namespace std
{
  // char16_t
  const bool numeric_limits<char16_t>::is_specialized;
  const int  numeric_limits<char16_t>::digits;
  const int  numeric_limits<char16_t>::digits10;
  const bool numeric_limits<char16_t>::is_signed;
  const bool numeric_limits<char16_t>::is_integer;
  const bool numeric_limits<char16_t>::is_exact;
  const int  numeric_limits<char16_t>::radix;
  const int  numeric_limits<char16_t>::min_exponent;
  const int  numeric_limits<char16_t>::min_exponent10;
  const int  numeric_limits<char16_t>::max_exponent;
  const int  numeric_limits<char16_t>::max_exponent10;
  const bool numeric_limits<char16_t>::has_infinity;
  const bool numeric_limits<char16_t>::has_quiet_NaN;
  const bool numeric_limits<char16_t>::has_signaling_NaN;
  const float_denorm_style numeric_limits<char16_t>::has_denorm;
  const bool numeric_limits<char16_t>::has_denorm_loss;
  const bool numeric_limits<char16_t>::is_iec559;
  const bool numeric_limits<char16_t>::is_bounded;
  const bool numeric_limits<char16_t>::is_modulo;
  const bool numeric_limits<char16_t>::traps;
  const bool numeric_limits<char16_t>::tinyness_before;
  const float_round_style numeric_limits<char16_t>::round_style;

  // char32_t
  const bool numeric_limits<char32_t>::is_specialized;
  const int  numeric_limits<char32_t>::digits;
  const int  numeric_limits<char32_t>::digits10;
  const bool numeric_limits<char32_t>::is_signed;
  const bool numeric_limits<char32_t>::is_integer;
  const bool numeric_limits<char32_t>::is_exact;
  const int  numeric_limits<char32_t>::radix;
  const int  numeric_limits<char32_t>::min_exponent;
  const int  numeric_limits<char32_t>::min_exponent10;
  const int  numeric_limits<char32_t>::max_exponent;
  const int  numeric_limits<char32_t>::max_exponent10;
  const bool numeric_limits<char32_t>::has_infinity;
  const bool numeric_limits<char32_t>::has_quiet_NaN;
  const bool numeric_limits<char32_t>::has_signaling_NaN;
  const float_denorm_style numeric_limits<char32_t>::has_denorm;
  const bool numeric_limits<char32_t>::has_denorm_loss;
  const bool numeric_limits<char32_t>::is_iec559;
  const bool numeric_limits<char32_t>::is_bounded;
  const bool numeric_limits<char32_t>::is_modulo;
  const bool numeric_limits<char32_t>::traps;
  const bool numeric_limits<char32_t>::tinyness_before;
  const float_round_style numeric_limits<char32_t>::round_style;
}

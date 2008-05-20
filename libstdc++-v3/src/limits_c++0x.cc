// std::limits definitions -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <limits>

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

// Definition of std::monostate for <variant> and <utility> -*- C++ -*-

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

/** @file bits/monostate.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{utility}
 */

#ifndef _GLIBCXX_MONOSTATE_H
#define _GLIBCXX_MONOSTATE_H 1

#include <bits/version.h>

#ifdef __glibcxx_variant // C++ >= 17

#include <bits/functional_hash.h>
#if __cplusplus >= 202002L
# include <compare>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  struct monostate { };

  constexpr bool operator==(monostate, monostate) noexcept { return true; }
#ifdef __cpp_lib_three_way_comparison
  constexpr strong_ordering
  operator<=>(monostate, monostate) noexcept { return strong_ordering::equal; }
#else
  constexpr bool operator!=(monostate, monostate) noexcept { return false; }
  constexpr bool operator<(monostate, monostate) noexcept { return false; }
  constexpr bool operator>(monostate, monostate) noexcept { return false; }
  constexpr bool operator<=(monostate, monostate) noexcept { return true; }
  constexpr bool operator>=(monostate, monostate) noexcept { return true; }
#endif

  template<>
    struct hash<monostate>
    {
#if __cplusplus < 202002L
      using result_type [[__deprecated__]] = size_t;
      using argument_type [[__deprecated__]] = monostate;
#endif

      size_t
      operator()(const monostate&) const noexcept
      {
	constexpr size_t __magic_monostate_hash = -7777;
	return __magic_monostate_hash;
      }
    };

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __glibcxx_variant
#endif /* _GLIBCXX_MONOSTATE_H */

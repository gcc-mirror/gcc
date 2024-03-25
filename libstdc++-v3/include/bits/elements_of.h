// Tag type for yielding ranges rather than values in <generator>  -*- C++ -*-

// Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_BITS_ELEMENTS_OF
#define _GLIBCXX_BITS_ELEMENTS_OF

#pragma GCC system_header

#include <bits/c++config.h>

#include <bits/version.h>

// C++ >= 23 && __glibcxx_coroutine
#if defined(__glibcxx_ranges) && defined(__glibcxx_generator)
#include <bits/ranges_base.h>
#include <bits/memoryfwd.h>

#if _GLIBCXX_HOSTED
# include <bits/allocator.h> // likely desirable if hosted.
#endif  // HOSTED

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace ranges
{

  /**
   * @ingroup ranges
   * @since C++23
   * @{
   */

  template<range _Range, typename _Alloc = allocator<byte>>
    struct elements_of
    {
      [[no_unique_address]] _Range range;
      [[no_unique_address]] _Alloc allocator = _Alloc();
    };

  template<typename _Range, typename _Alloc = allocator<byte>>
    elements_of(_Range&&, _Alloc = _Alloc())
      -> elements_of<_Range&&, _Alloc>;

  /// @}
}
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif  // __glibcxx_generator && __glibcxx_ranges
#endif  // _GLIBCXX_BITS_ELEMENTS_OF

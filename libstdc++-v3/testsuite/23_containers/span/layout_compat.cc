// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <span>
#include <cstddef>

#if __has_include(<sys/uio.h>)
#include <sys/uio.h>
#else
struct iovec { void* iov_base; std::size_t iov_len; };
#endif

#if __cpp_lib_is_pointer_interconvertible
using std::is_layout_compatible_v;
#else
// A poor substitute for is_layout_compatible_v
template<typename T, typename U>
  constexpr bool is_layout_compatible_v
    = std::is_standard_layout_v<T> && std::is_standard_layout_v<U>
      && sizeof(T) == sizeof(U) && alignof(T) == alignof(U);
#endif

void
test_pr95609()
{
  using rbuf = std::span<const std::byte>;
  using wbuf = std::span<std::byte>;

  static_assert(is_layout_compatible_v<rbuf, struct iovec>);
  static_assert(is_layout_compatible_v<wbuf, struct iovec>);
}

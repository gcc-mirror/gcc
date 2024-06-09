// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <span>
#include <cstddef>

#if __has_include(<sys/uio.h>)
#include <sys/uio.h>
#else
struct iovec { void* iov_base; std::size_t iov_len; };
#endif

// std::span cannot possibly be layout-compatible with struct iovec because
// iovec::iov_base is a void* and span<void> is ill-formed. Additionally,
// the libstdc++ std::span uses [[no_unique_address]] on the second member,
// so that it's not present for a span of static extent, and that affects
// layout-compatibility too.
// Use this to check the size and alignment are compatible.
template<typename T, typename U>
  constexpr bool same_size_and_alignment
    = std::is_standard_layout_v<T> && std::is_standard_layout_v<U>
      && sizeof(T) == sizeof(U) && alignof(T) == alignof(U);

void
test_pr95609()
{
  using rbuf = std::span<const std::byte>;
  static_assert(same_size_and_alignment<rbuf, struct iovec>);

  using wbuf = std::span<std::byte>;
  static_assert(same_size_and_alignment<wbuf, struct iovec>);
}

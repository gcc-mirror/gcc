// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=c++2a" }
// { dg-do run { target c++2a } }

#include <testsuite_hooks.h>

#include <span>
#include <type_traits>
#include <cstdint>
#include <vector>
#include <algorithm>
#include <cassert>

int
main()
{
  struct alignas(256) strawman
  {
    int x;
    int y;
    bool z;
    int w;
  };

  struct naked_span
  {
    char* p;
    std::size_t n;
  };

  struct strawman_span
  {
    strawman* p;
    std::size_t n;
  };

  static_assert(sizeof(std::span<char, 0>) <= sizeof(char*));
  static_assert(sizeof(std::span<const char, 0>) <= sizeof(const char*));
  static_assert(sizeof(std::span<strawman, 0>) <= sizeof(strawman*));
  static_assert(sizeof(std::span<strawman, 1>) <= sizeof(strawman*));
  static_assert(sizeof(std::span<char>) <= sizeof(naked_span));
  static_assert(sizeof(std::span<strawman>) <= sizeof(strawman_span));

  constexpr static std::array<int, 9> arr_data{ 0, 1, 2, 3, 4, 5, 6, 7, 8 };
  constexpr auto arr_data_span = std::span(arr_data);
  static_assert(arr_data_span.size() == 9);
  static_assert(arr_data_span.size_bytes() == 9 * sizeof(int));
  static_assert(*arr_data_span.begin() == 0);
  static_assert(*arr_data_span.data() == 0);
  static_assert(arr_data_span.front() == 0);
  static_assert(arr_data_span.back() == 8);
  static_assert(arr_data_span[0] == 0);
  static_assert(arr_data_span[1] == 1);
  static_assert(arr_data_span[2] == 2);
  static_assert(arr_data_span[3] == 3);
  static_assert(arr_data_span[4] == 4);
  static_assert(arr_data_span[5] == 5);
  static_assert(arr_data_span[6] == 6);
  static_assert(arr_data_span[7] == 7);
  static_assert(arr_data_span[8] == 8);
  static_assert(!arr_data_span.empty());
  static_assert(decltype(arr_data_span)::extent == 9);

  constexpr static int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
  constexpr auto data_span    = std::span(data);
  static_assert(data_span.size() == 9);
  static_assert(data_span.size_bytes() == 9 * sizeof(int));
  static_assert(*data_span.begin() == 0);
  static_assert(*data_span.data() == 0);
  static_assert(data_span.front() == 0);
  static_assert(data_span.back() == 8);
  static_assert(data_span[0] == 0);
  static_assert(data_span[1] == 1);
  static_assert(data_span[2] == 2);
  static_assert(data_span[3] == 3);
  static_assert(data_span[4] == 4);
  static_assert(data_span[5] == 5);
  static_assert(data_span[6] == 6);
  static_assert(data_span[7] == 7);
  static_assert(data_span[8] == 8);
  static_assert(!data_span.empty());
  static_assert(decltype(data_span)::extent == 9);

  constexpr auto data_span_first = data_span.first<3>();
  static_assert(
    std::is_same_v<std::remove_cv_t<decltype(data_span_first)>, std::span<const int, 3>>);
  static_assert(decltype(data_span_first)::extent == 3);
  static_assert(data_span_first.size() == 3);
  static_assert(data_span_first.front() == 0);
  static_assert(data_span_first.back() == 2);

  constexpr auto data_span_first_dyn = data_span.first(4);
  static_assert(
    std::is_same_v<std::remove_cv_t<decltype(data_span_first_dyn)>, std::span<const int>>);
  static_assert(decltype(data_span_first_dyn)::extent == std::dynamic_extent);
  static_assert(data_span_first_dyn.size() == 4);
  static_assert(data_span_first_dyn.front() == 0);
  static_assert(data_span_first_dyn.back() == 3);

  constexpr auto data_span_last = data_span.last<5>();
  static_assert(
    std::is_same_v<std::remove_cv_t<decltype(data_span_last)>, std::span<const int, 5>>);
  static_assert(decltype(data_span_last)::extent == 5);
  static_assert(data_span_last.size() == 5);
  static_assert(data_span_last.front() == 4);
  static_assert(data_span_last.back() == 8);

  constexpr auto data_span_last_dyn = data_span.last(6);
  static_assert(
    std::is_same_v<std::remove_cv_t<decltype(data_span_last_dyn)>, std::span<const int>>);
  static_assert(decltype(data_span_last_dyn)::extent == std::dynamic_extent);
  static_assert(data_span_last_dyn.size() == 6);
  static_assert(data_span_last_dyn.front() == 3);
  static_assert(data_span_last_dyn.back() == 8);

  constexpr auto data_span_subspan = data_span.subspan<1, 3>();
  static_assert(
    std::is_same_v<std::remove_cv_t<decltype(data_span_subspan)>, std::span<const int, 3>>);
  static_assert(decltype(data_span_subspan)::extent == 3);
  static_assert(data_span_subspan.size() == 3);
  static_assert(data_span_subspan.front() == 1);
  static_assert(data_span_subspan.back() == 3);

  constexpr auto data_span_subspan_offset = data_span.subspan<8>();
  static_assert(
    std::is_same_v<std::remove_cv_t<decltype(data_span_subspan_offset)>, std::span<const int, 1>>);
  static_assert(decltype(data_span_subspan_offset)::extent == 1);
  static_assert(data_span_subspan_offset.size() == 1);
  static_assert(data_span_subspan_offset.front() == 8);
  static_assert(data_span_subspan_offset.back() == 8);

  constexpr auto data_span_subspan_empty = data_span.subspan(9, 0);
  static_assert(
    std::is_same_v<std::remove_cv_t<decltype(data_span_subspan_empty)>, std::span<const int>>);
  static_assert(decltype(data_span_subspan_empty)::extent == std::dynamic_extent);
  static_assert(data_span_subspan_empty.size() == 0);

  constexpr auto data_span_subspan_empty_static = data_span.subspan<9>();
  static_assert(std::is_same_v<std::remove_cv_t<decltype(data_span_subspan_empty_static)>,
    std::span<const int, 0>>);
  static_assert(decltype(data_span_subspan_empty_static)::extent == 0);
  static_assert(data_span_subspan_empty.size() == 0);

  std::span<short> shorts{};
  bool really_empty0 = shorts.empty();
  bool really_empty1 = std::empty(shorts);
  bool really_empty2 = shorts.data() == nullptr;
  bool really_empty3 = shorts.begin() == shorts.end();
  bool really_empty =
    really_empty0 && really_empty1 && really_empty2 && really_empty3;
  (void)really_empty;
  VERIFY(really_empty);

  std::vector<std::int_least32_t> value{ 0 };
  std::span<int32_t> muh_span(value);
  VERIFY(muh_span.size() == 1);
  std::byte* original_bytes                  = reinterpret_cast<std::byte*>(value.data());
  original_bytes[0]                          = static_cast<std::byte>(1);
  original_bytes[1]                          = static_cast<std::byte>(2);
  original_bytes[2]                          = static_cast<std::byte>(3);
  original_bytes[3]                          = static_cast<std::byte>(4);
  std::span<const std::byte> muh_byte_span   = std::as_bytes(muh_span);
  std::span<std::byte> muh_mutable_byte_span = std::as_writable_bytes(muh_span);
  std::span<std::byte> muh_original_byte_span(original_bytes, original_bytes + 4);
  bool definitely_reinterpret_casted0 = std::equal(muh_byte_span.begin(), muh_byte_span.end(),
    muh_original_byte_span.begin(), muh_original_byte_span.end());
  bool definitely_reinterpret_casted1 = std::equal(muh_mutable_byte_span.begin(),
    muh_mutable_byte_span.end(), muh_original_byte_span.begin(), muh_original_byte_span.end());
  bool definitely_reinterpret_casted =
    definitely_reinterpret_casted0 && definitely_reinterpret_casted1;
  (void)definitely_reinterpret_casted;
  VERIFY(definitely_reinterpret_casted);

  std::span<std::byte> muh_original_byte_span_ptr_size(original_bytes, 4);
  bool definitely_equivalent =
    std::equal(muh_original_byte_span_ptr_size.begin(), muh_original_byte_span_ptr_size.end(),
      muh_original_byte_span.begin(), muh_original_byte_span.end());
  (void)definitely_equivalent;
  VERIFY(definitely_equivalent);

  return definitely_equivalent && definitely_reinterpret_casted && really_empty ? 0 : 1;
}

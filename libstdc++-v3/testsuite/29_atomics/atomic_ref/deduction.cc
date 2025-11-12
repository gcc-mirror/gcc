// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#include <atomic>

template <typename T>
void
test_impl(T v)
{
  std::atomic_ref a(v);
  static_assert(std::is_same_v<decltype(a), std::atomic_ref<T>>);
}

template <typename T>
void
test(T v)
{
  test_impl<T>(v);
  test_impl<const T>(v);
  if constexpr (std::atomic_ref<T>::is_always_lock_free)
  {
    test_impl<volatile T>(v);
    test_impl<const volatile T>(v);
  }
}

int main()
{
  test<int>(0);
  test<float>(1.0f);
  test<int*>(nullptr);
  struct X { } x;
  test<X>(x);
}

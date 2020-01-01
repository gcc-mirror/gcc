// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

// PR libstdc++/87749

#include <string>
#include <testsuite_hooks.h>

bool oom = false;

template<typename T>
struct alloc
{
  using value_type = T;

#if !_GLIBCXX_USE_CXX11_ABI
  using size_type = unsigned long;
  using difference_type = long;
  using reference = T&;
  using const_reference = T&;
  using pointer = T*;
  using const_pointer = const T*;
  template<typename U>
    struct rebind { using other = alloc<U>; };
#endif

  int not_empty = 0; // this makes is_always_equal false

  alloc() = default;
  template<typename U>
    alloc(const alloc<U>&) { }

  T* allocate(unsigned long n)
  {
    if (oom)
      throw std::bad_alloc();
    return std::allocator<T>().allocate(n);
  }

  void deallocate(T* p, unsigned long n)
  {
    std::allocator<T>().deallocate(p, n);
  }
};

template<typename T, typename U>
bool operator==(const alloc<T>&, const alloc<U>&) { return true; }

template<typename T, typename U>
bool operator!=(const alloc<T>&, const alloc<U>&) { return false; }

int main()
{
  using string = std::basic_string<char, std::char_traits<char>, alloc<char>>;

  string s = "PR libstdc++/87749 a string that is longer than a short string";
  const auto ptr = s.c_str();
  oom = true;
  string ss;
  ss = std::move(s); // allocators are equal, should not allocate new storage
  VERIFY( ss.c_str() == ptr );
}

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

// libstdc++/56166

#ifndef _GLIBCXX_USE_CXX11_ABI
# define _GLIBCXX_USE_CXX11_ABI 0
#endif
#include <ext/vstring.h>
#include <new>

static int fail_after = -1;

template<typename T>
  struct Allocator
  {
    using value_type = T;

    // Need these typedefs because COW string doesn't use allocator_traits.
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using difference_type = long;
    using size_type = unsigned long;
    template<typename U>
      struct rebind {
        using other = Allocator<U>;
      };

    Allocator() { }

    template<typename U>
      Allocator(const Allocator<U>&) { }

    T* allocate(size_type n)
    {
      if (fail_after >= 0) {
        if (fail_after-- == 0) {
          throw std::bad_alloc();
        }
      }
      return (T*)operator new (n * sizeof(T));
    }

    void deallocate(T* p, size_type)
    {
      operator delete (p);
    }
  };

template<typename T, typename U>
  bool operator==(const Allocator<T>&, const Allocator<U>&) { return true; }
template<typename T, typename U>
  bool operator!=(const Allocator<T>&, const Allocator<U>&) { return false; }


using string = __gnu_cxx::__versa_string<char, std::char_traits<char>,
                                         Allocator<char>,
                                         __gnu_cxx::__rc_string_base>;

string f()
{
  string s1("xxxxxx");
  string s2 = s1;
  s1.clear();
  return s2;
}

int main()
{
  for (int i = 0; i < 10; i++) {
    try {
      fail_after = i;
      f();
      break;
    } catch (const std::bad_alloc&) {
    }
  }
}

// The __versa_string destructor triggers a bogus -Wfree-nonheap-object
// due to pr54202.
// { dg-prune-output "\\\[-Wfree-nonheap-object" }

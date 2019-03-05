// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-options "-O2" }
// { dg-do compile { target c++11 } }

#include <cstdlib>
#include <vector>

template<class T>
struct Alloc : public std::allocator<T>
{
  template<class U>
    struct rebind { typedef Alloc<U> other; };

  Alloc() : std::allocator<T>() {}

  template<class U>
    Alloc(const Alloc<U>& other) : std::allocator<T>(other) {}

  T* allocate(std::size_t num, const void* = 0)
  {
    std::size_t size = num * sizeof(T);
    void *result = std::malloc(size);
    if(size>16 && (std::size_t(result) & 15)!=0) {
      std::free(result);
      return 0;
    }
    return static_cast<T*>( result );
  }

  void deallocate(T* p, std::size_t) { std::free(p); }
};

unsigned f(std::vector<int, Alloc<int> >& v)
{
  v.push_back(1);
  return v.size();
}

template<class T>
struct Alloc2 : public Alloc<T>
{
  template<class U>
    struct rebind { typedef Alloc2<U> other; };

  Alloc2() : Alloc<T>() {}

  template<class U>
    Alloc2(const Alloc2<U>& other) : Alloc<T>(other) {}

  std::size_t max_size() const { return std::size_t(-1) / sizeof(T); }
};

unsigned g(std::vector<int, Alloc2<int> >& v)
{
  v.push_back(1);
  return v.size();
}

// Copyright (C) 2011 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++0x" }

#include <memory>
#include <vector>
#include <testsuite_hooks.h>

int count = 0;

template <class T>
  struct allocator98 : std::allocator<T>
  {
    template <class U> struct rebind { typedef allocator98<U> other; };

    allocator98() { }

    template <class U> allocator98(const allocator98<U>&) { };

    void construct(T* p, const T& val)
    {
      ++count;
      std::allocator<T>::construct(p, val);
    }
  };

template <class T>
  struct allocator11 : std::allocator<T>
  {
    template <class U> struct rebind { typedef allocator11<U> other; };

    allocator11() { }

    template <class U> allocator11(const allocator11<U>&) { };

    template<typename... Args>
      void construct(T* p, Args&&... args)
      {
	++count;
	std::allocator<T>::construct(p, std::forward<Args>(args)...);
      }
  };

int main()
{
  std::vector< int, allocator98<int> > v98(1);
  VERIFY( count == 0 );

  std::vector< int, allocator11<int> > v11(1);
  VERIFY( count == 1 );
}

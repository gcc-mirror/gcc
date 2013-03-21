// -*- C++ -*-

// Copyright (C) 2013 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <map>

// { dg-do compile }
// { dg-options "-std=gnu++11" }

// libstdc++/56613
#include <map>

// A conforming C++03 allocator, should still work in C++11 mode.
template<typename T>
struct alloc
{
    typedef T value_type;
    typedef T* pointer;
    typedef const T* const_pointer;
    typedef T& reference;
    typedef const T& const_reference;
    typedef unsigned size_type;
    typedef int difference_type;

    template<typename U>
        struct rebind {
            typedef alloc<U> other;
        };

    alloc() { }
    template<typename U>
        alloc(const alloc<U>&) { }

    pointer allocate(size_type n, const void* = 0) { return
std::allocator<T>().allocate(n); }
    void deallocate(pointer p, size_type n) { std::allocator<T>().deallocate(p,
n); }

    size_type max_size() const { return -1; }

    void construct(pointer p, const T& t) { new ((void*) p) T(t); }
    void destroy(pointer p) { p->~T(); }

    pointer address(reference x) const throw() { return &x; }
    const_pointer address(const_reference x) const throw() { return &x; }
};

template<typename T, typename U>
bool operator==(alloc<T>, alloc<U>) { return true; }

template<typename T, typename U>
bool operator!=(alloc<T>, alloc<U>) { return false; }

int main()
{
  std::map<int, int, std::less<int>, alloc<int> > m;
  m[1];
}

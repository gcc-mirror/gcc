// { dg-do run { target c++11 } }
// { dg-require-effective-target cxx11_abi }

// 2019-05-27  Nina Dinka Ranns  <dinka.ranns@gmail.com>
//
// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

#include <string>
#include <testsuite_hooks.h>

using C = char;
using traits = std::char_traits<C>;
int constructCount = 0;

static void resetCounter()
{
 constructCount = 0;
}

template <class Tp>
struct TestAllocator
{
  typedef Tp value_type;
  using size_type = unsigned;

  TestAllocator() noexcept { constructCount++; }

  template <class T>
  TestAllocator(const TestAllocator<T>&) {}

  Tp *allocate(std::size_t n)
  { return std::allocator<Tp>().allocate(n); }

  void deallocate(Tp *p, std::size_t n)
  { std::allocator<Tp>().deallocate(p, n); }

};

template <class T, class U>
bool operator==(const TestAllocator<T>&, const TestAllocator<U>&)
{ return true; }
template <class T, class U>
bool operator!=(const TestAllocator<T>&, const TestAllocator<U>&)
{ return false; }

void test01()
{
  typedef TestAllocator<C> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;
  test_type v1{alloc_type()};
  std::string v2{"some_content"};

  resetCounter();
  v1.assign(v2.begin(),v2.end());
  VERIFY( constructCount == 0);

  v1.append(v2.begin(),v2.end());
  VERIFY( constructCount == 0);

  v1.insert(v1.begin(),v1.begin(),v1.end());
  VERIFY( constructCount == 0);

  v1.replace(v1.begin(),v1.end(),v1.begin(),v1.end());
  VERIFY( constructCount == 0);
}
int main()
{
  test01();
  return 0;
}

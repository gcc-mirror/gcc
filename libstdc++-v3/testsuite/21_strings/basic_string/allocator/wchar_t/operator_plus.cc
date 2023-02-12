// 2019-04-30  Nina Dinka Ranns  <dinka.ranns@gmail.com>
// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
// COW strings don't support C++11 allocator propagation:
// { dg-require-effective-target cxx11_abi }

#include <string>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>
#include <ext/throw_allocator.h>

using C = wchar_t;
using traits = std::char_traits<C>;

using __gnu_test::propagating_allocator;

void test01()
{
  typedef propagating_allocator<C, true> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;

  test_type v1(L"something",alloc_type(1));
  test_type v2(L"something",alloc_type(2));
  auto r1 = v1 + v2;
  VERIFY(r1.get_allocator().get_personality() == 1);

  auto r2 = v1 + std::move(v2);
  VERIFY(r2.get_allocator().get_personality() == 2);

  test_type v3(L"something", alloc_type(3));
  test_type v4(L"something", alloc_type(4));
  auto r3 = std::move(v3) + v4;
  VERIFY(r3.get_allocator().get_personality() == 3);

  auto r4 = std::move(v1) +std::move(v4);
  VERIFY(r4.get_allocator().get_personality() == 1);

  test_type v5(L"something", alloc_type(5));
  auto r5 = v5 + L"str";
  VERIFY(r5.get_allocator().get_personality() == 5);

  auto r6 = v5 + L'c';
  VERIFY(r6.get_allocator().get_personality() == 5);

  auto r7 = std::move(v5) + L"str";
  VERIFY(r7.get_allocator().get_personality() == 5);

  test_type v6(L"something", alloc_type(6));
  auto r8 = std::move(v6) + L'c';
  VERIFY(r8.get_allocator().get_personality() == 6);

  test_type v7(L"something", alloc_type(7));
  auto r9 = L"str" + v7;
  VERIFY(r9.get_allocator().get_personality() == 7);

  auto r10 = L'c' + v7;
  VERIFY(r10.get_allocator().get_personality() == 7);

  auto r11 = L"str" + std::move(v7);
  VERIFY(r11.get_allocator().get_personality() == 7);

  test_type v8(L"something", alloc_type(8));
  auto r12 = L'c' + std::move(v8);
  VERIFY(r12.get_allocator().get_personality() == 8);
}

void test02()
{
  typedef propagating_allocator<C, false> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;

  test_type v1(L"something",alloc_type(1));
  test_type v2(L"something",alloc_type(2));
  auto r1 = v1 + v2;
  VERIFY(r1.get_allocator().get_personality() != 1);

  auto r2 = v1 + std::move(v2);
  VERIFY(r2.get_allocator().get_personality() == 2);

  test_type v3(L"something", alloc_type(3));
  test_type v4(L"something", alloc_type(4));
  auto r3 = std::move(v3) + v4;
  VERIFY(r3.get_allocator().get_personality() == 3);

  auto r4 = std::move(v1) +std::move(v4);
  VERIFY(r4.get_allocator().get_personality() == 1);

  test_type v5(L"something", alloc_type(5));
  auto r5 = v5 + L"str";
  VERIFY(r5.get_allocator().get_personality() != 5);

  auto r6 = v5 + L'c';
  VERIFY(r6.get_allocator().get_personality() != 5);

  auto r7 = std::move(v5) + L"str";
  VERIFY(r7.get_allocator().get_personality() == 5);

  test_type v6(L"something", alloc_type(6));
  auto r8 = std::move(v6) + L'c';
  VERIFY(r8.get_allocator().get_personality() == 6);

  test_type v7(L"something", alloc_type(7));
  auto r9 = L"str" + v7;
  VERIFY(r9.get_allocator().get_personality() != 7);

  auto r10 = L'c' + v7;
  VERIFY(r10.get_allocator().get_personality() != 7);

  auto r11 = L"str" + std::move(v7);
  VERIFY(r11.get_allocator().get_personality() == 7);

  test_type v8(L"something", alloc_type(8));
  auto r12 = L'c' + std::move(v8);
  VERIFY(r12.get_allocator().get_personality() == 8);
}
void test03()
{
  typedef propagating_allocator<C, false> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;

  test_type v1(L"s",alloc_type(1));
  v1.resize(10);
  v1.shrink_to_fit();
  test_type v2(10000,L'x',alloc_type(2));
  v2.reserve(10010);

  auto r=std::move(v1)+std::move(v2);
  VERIFY(r.get_allocator().get_personality() == 1);
}
int main()
{
  test01();
  test02();
  test03();
  return 0;
}

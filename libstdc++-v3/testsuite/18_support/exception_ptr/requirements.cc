// { dg-do run { target c++11 } }

// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

#include <exception>
#include <testsuite_hooks.h>

// test NullablePointer requirements
void test01()
{
  std::exception_ptr p1;        // DefaultConstructible
  std::exception_ptr p2(p1);    // CopyConstructible
  p1 = p2;                      // CopyAssignable
  VERIFY( p1 == p2 );           // EqualityComparable
  VERIFY( !bool(p1) );          // contextually convertible to bool
  swap(p1, p2);                 // Swappable

  // Table 39 expressions
  std::exception_ptr p3 = nullptr;
  std::exception_ptr p4(nullptr);
  VERIFY( std::exception_ptr() == nullptr );
  p4 = nullptr;
  VERIFY( p4 == nullptr );
  VERIFY( nullptr == p4 );
  VERIFY( (p4 != nullptr) == !(p4 == nullptr) );
  VERIFY( (nullptr != p4) == !(p4 == nullptr) );

  std::exception_ptr p5{};      // value initialized ...
  VERIFY( p5 == nullptr );      // ... is equivalent to null
}

// additional exception_ptr requirements
void test02()
{
  std::exception_ptr p1;
  VERIFY( p1 == nullptr );
}

int main()
{
  test01();
  test02();
  return 0;
}

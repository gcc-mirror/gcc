// { dg-options "-std=gnu++0x" }

// 2010-09-27  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

struct MyAlloc { };

struct MyDerivedAlloc
: public MyAlloc { };

struct UA { };

struct UB { typedef int             allocator_type; };

struct UC { typedef MyAlloc         allocator_type; };

struct UD { typedef MyDerivedAlloc  allocator_type; };

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::uses_allocator;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_relationship<uses_allocator, UC, MyAlloc>(true)) );
  VERIFY( (test_relationship<uses_allocator, UC, MyDerivedAlloc>(true)));

  // Negative tests.
  VERIFY( (test_relationship<uses_allocator, UA, MyAlloc>(false)) );
  VERIFY( (test_relationship<uses_allocator, UB, MyAlloc>(false)) );
  VERIFY( (test_relationship<uses_allocator, UD, MyAlloc>(false)) );
}

int main()
{
  test01();
  return 0;
}

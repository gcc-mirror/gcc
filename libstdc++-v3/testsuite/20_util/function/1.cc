// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// 2005-01-15 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 20.7.15 polymorphic function object wrapper
#include <functional>
#include <testsuite_hooks.h>

using namespace __gnu_test;

// Operations on empty function<> objects
void test01()
{
  using std::function;
  using std::bad_function_call;

  // Default-construction
  function<int(float)> f1;
  VERIFY( ((bool)f1 == false) );
  VERIFY( !f1 );
  VERIFY( f1 == 0 );
  VERIFY( 0 == f1 );
  VERIFY( !(f1 != 0) );
  VERIFY( !(0 != f1) );

  // Copy-construction
  function<int(float)> f2(f1);
  VERIFY( !f2 );

  // Construct with NULL pointer
  function<int(float)> f3(0);
  VERIFY( !f3 );

  // Assignment
  f1 = f2;
  VERIFY( !f1);

  // Assignment to NULL pointer
  f1 = 0;
  VERIFY( !f1 );

  // Swap
  swap(f1, f2);
  VERIFY( !f1 );
  VERIFY( !f2 );

  // Invocation should throw bad_function_call
  bool thrown = false;
  try
    {
      f1(3.14159f);
      VERIFY( false );
    }
  catch (const bad_function_call&)
    {
      thrown = true;
    }
  VERIFY( thrown );

#if __cpp_rtti
  // target_type returns typeid(void)
  VERIFY( f1.target_type() == typeid(void) );
#endif

  // target() always returns a NULL pointer
  VERIFY( f1.target<int (*)(float)>() == 0);

  // Check const version
  const function<int(float)>& f1c = f1;
  VERIFY( f1c.target<int (*)(float)>() == 0 );
  VERIFY( !f1c );
}

int main()
{
  test01();
  return 0;
}

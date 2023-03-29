// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// 2005-01-15 Douglas Gregor <dgregor@cs.indiana.edu>
//
// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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
#include <testsuite_tr1.h>

using namespace __gnu_test;

// Put function objects into function<> wrappers
void test04()
{
  using std::function;

  do_truncate_float_t truncate_float;

  function<int(float)> f1(truncate_float);
  VERIFY( f1 );
  VERIFY( !!f1 );
  VERIFY( !(f1 == 0) );
  VERIFY( !(0 == f1) );
  VERIFY( f1 != 0 );
  VERIFY( 0 != f1 );

  // Copy-construction
  function<int(float)> f2(f1);
  VERIFY( f2 );

  // Invocation
  VERIFY( f1(3.1f) == 3 );
  VERIFY( f2(3.1f) == 3 );

  // Assignment to zero
  f1 = 0;
  VERIFY( !f1 );

  // Swap
  f1.swap(f2);
  VERIFY( f1 );
  VERIFY( !f2 );
  VERIFY( f1(3.1f) == 3 );

  // Assignment from a function pointer
  f2 = do_truncate_float_t();
  VERIFY( f2(3.1f) == 3 );

  // target_type and target() functions
  const function<int(float)>& f1c = f1;
#if __cpp_rtti
  VERIFY( typeid(do_truncate_float_t) == f1.target_type() );
#endif
  VERIFY( f2.target<do_truncate_float_t>() != 0 );
  VERIFY( f1c.target<do_truncate_float_t>() != 0 );
}

int main()
{
  test04();

  VERIFY( do_truncate_double_t::live_objects == 0 );
  VERIFY( do_truncate_float_t::live_objects == 0 );

  return 0;
}

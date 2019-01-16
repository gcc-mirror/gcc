// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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


#include <future>
#include <testsuite_hooks.h>

void test01()
{
  using std::launch;

  const launch none{};
  const launch both = launch::async|launch::deferred;
  const launch all = ~none;

  VERIFY( (none & both) == none );
  VERIFY( (none | both) == both );
  VERIFY( (none ^ both) == both );

  VERIFY( (none & all) == none );
  VERIFY( (none | all) == all );
  VERIFY( (none ^ all) == all );

  VERIFY( (both & all) == both );
  VERIFY( (both | all) == all );
  VERIFY( (both ^ all) == ~both );

  VERIFY( (none & launch::async) == none );
  VERIFY( (none & launch::deferred) == none );

  VERIFY( (none | launch::async) == launch::async );
  VERIFY( (none | launch::deferred) == launch::deferred );

  VERIFY( (none ^ launch::async) == launch::async );
  VERIFY( (none ^ launch::deferred) == launch::deferred );

  VERIFY( (none & none) == none );
  VERIFY( (none | none) == none );
  VERIFY( (none ^ none) == none );

  VERIFY( (both & both) == both );
  VERIFY( (both | both) == both );
  VERIFY( (both ^ both) == none );

  VERIFY( (all & all) == all );
  VERIFY( (all | all) == all );
  VERIFY( (all ^ all) == none );

  launch l = none;

  l &= none;
  VERIFY( l == none );
  l |= none;
  VERIFY( l == none );
  l ^= none;
  VERIFY( l == none );

  l &= both;
  VERIFY( l == none );
  l |= both;
  VERIFY( l == both );
  l ^= both;
  VERIFY( l == none );
}

int main()
{
  test01();
  return 0;
}

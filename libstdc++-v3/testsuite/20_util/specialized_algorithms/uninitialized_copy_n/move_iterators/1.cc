// { dg-options "-std=gnu++0x" }

// 2008-06-29  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#undef _GLIBCXX_CONCEPT_CHECKS
#define  _GLIBCXX_TESTSUITE_ALLOW_RVALREF_ALIASING

#include <algorithm>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::rvalstruct;
using std::uninitialized_copy_n;

typedef test_container<rvalstruct, input_iterator_wrapper> container_in;
typedef test_container<rvalstruct, forward_iterator_wrapper> container_out;

void test01()
{
  bool test __attribute__((unused)) = true;

  int inarray[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const int size = sizeof(inarray) / sizeof(int);

  rvalstruct in[size], out[size];
  std::copy(inarray, inarray + size, in);

  container_in incon(in, in + size);
  container_out outcon(out, out + size);

  uninitialized_copy_n(std::move_iterator<input_iterator_wrapper<rvalstruct> >(incon.begin()),
		       size, outcon.begin());
  VERIFY( std::equal(out, out + size, inarray) );
  for (int z = 0; z < size; ++z)
    VERIFY( out[z].valid );
  for (int z = 0; z < size; ++z)
    VERIFY( !in[z].valid );
}


int main()
{
  test01();
  return 0;
}

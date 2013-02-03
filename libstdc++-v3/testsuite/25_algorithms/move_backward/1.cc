// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007-2013 Free Software Foundation, Inc.
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

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::rvalstruct;
using std::move_backward;

typedef test_container<rvalstruct,
		       bidirectional_iterator_wrapper> container_in;
typedef test_container<rvalstruct,
		       bidirectional_iterator_wrapper> container_out;

void test01()
{
  bool test __attribute__((unused)) = true;

  int inarray[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const int size = sizeof(inarray) / sizeof(int);

  rvalstruct in[size], out[size];
  std::copy(inarray, inarray + size, in);
  std::fill(out, out + size, 0);

  container_in incon(in, in + size);
  container_out outcon(out, out + size);

  move_backward(incon.begin(), incon.end(), outcon.end());
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

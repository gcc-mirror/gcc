// 2004-07-07  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2024 Free Software Foundation, Inc.
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

// 27.7.1.3 basic_stringbuf overridden virtual functions.

#include <sstream>
#include <cstdlib>
#include <testsuite_hooks.h>

using namespace std;

string
data(unsigned len)
{
  string ret;
  for (unsigned i = 0; i < len; ++i)
    ret.push_back('a' + rand() % 26);
  return ret;
}

void
test01(unsigned iter)
{
  for (unsigned n = 1; n <= iter; n *= 10)
    {
      const string str = data(n);
      stringbuf sstr;
      for (unsigned i = 0; i < n; ++i)
	sstr.sputc(str[i]);
      VERIFY( str == sstr.str() );
    }
}

// This can take long on simulators, timing out the test.
// { dg-options "-DITERATIONS=10000" { target simulator } }
#ifndef ITERATIONS
#define ITERATIONS 10000000
#endif

int main()
{
  test01(ITERATIONS);
  return 0;
}

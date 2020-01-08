// 1999-11-15 Kevin Ediger  <kediger@licor.com>
// test the floating point inserters (facet num_put)

// Copyright (C) 1999-2020 Free Software Foundation, Inc.
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

#include <cmath> // for abs
#include <cfloat> // for DBL_EPSILON
#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

void
test05()
{
  using namespace std;

  double pi = 3.14159265358979323846;
  ostringstream ostr;
  ostr.precision(20);
  ostr << pi;
  string sval = ostr.str();
  istringstream istr (sval);
  double d;
  istr >> d;
  VERIFY( abs(pi-d)/pi < DBL_EPSILON );
}

int 
main()
{
  test05();
  return 0;
}

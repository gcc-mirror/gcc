// 1999-11-15 Kevin Ediger  <kediger@licor.com>
// test the floating point inserters (facet num_put)

// Copyright (C) 1999-2018 Free Software Foundation, Inc.
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

// { dg-do run { xfail lax_strtofp } }

#include <cmath> // for abs
#include <cfloat> // for DBL_EPSILON
#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

// libstdc++/9151
void
test06()
{
  using namespace std;

  int prec = numeric_limits<double>::digits10 + 2;
  double oval = numeric_limits<double>::min();

  stringstream ostr;
  ostr.precision(prec);
  ostr << oval;
  string sval = ostr.str();
  istringstream istr (sval);
  double ival;
  istr >> ival;
  VERIFY( abs(oval-ival)/oval < DBL_EPSILON ); 
}

int 
main()
{
  test06();
  return 0;
}

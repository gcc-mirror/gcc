// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

#include <cmath> // for abs
#include <cfloat> // for DBL_EPSILON
#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

void
test05()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  double pi = 3.14159265358979323846;
  wostringstream ostr;
  ostr.precision(20);
  ostr << pi;
  wstring sval = ostr.str();
  wistringstream istr(sval);
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

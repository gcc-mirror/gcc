// { dg-require-swprintf "" }

// Copyright (C) 2005, 2006, 2007, 2009 Free Software Foundation, Inc.
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

#include <cstdio> // for swprintf
#include <iostream>
#include <iomanip>
#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

void
test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // make sure we can output a very long float
  long double val = numeric_limits<long double>::max();
  int prec = numeric_limits<long double>::digits10;

  wostringstream os;
  os.precision(prec);
  os.setf(wios::scientific);
  os << val;

  wchar_t largebuf[512];
  swprintf(largebuf, 512, L"%.*Le", prec, val);
#ifdef TEST_NUMPUT_VERBOSE
  cout << "expect: " << largebuf << endl;
  cout << "result: " << os.str() << endl;
#endif
  VERIFY( os && os.str() == largebuf );

  // Make sure we can output a long float in fixed format
  // without seg-faulting (libstdc++/4402)
  double val2 = numeric_limits<double>::max();

  wostringstream os2;
  os2.precision(3);
  os2.setf(wios::fixed);
  os2 << val2;

  swprintf(largebuf, 512, L"%.*f", 3, val2);
#ifdef TEST_NUMPUT_VERBOSE
  cout << "expect: " << largebuf << endl;
  cout << "result: " << os2.str() << endl;
#endif
  VERIFY( os2 && os2.str() == largebuf );
}

int 
main()
{
  test02();
  return 0;
}

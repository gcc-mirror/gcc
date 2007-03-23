// 1999-11-15 Kevin Ediger  <kediger@licor.com>
// test the floating point inserters (facet num_put)

// Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
// Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <cstdio> // for sprintf
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

  ostringstream os;
  os.precision(prec);
  os.setf(ios::scientific);
  os << val;

  char largebuf[512];
  sprintf(largebuf, "%.*Le", prec, val);
#ifdef TEST_NUMPUT_VERBOSE
  cout << "expect: " << largebuf << endl;
  cout << "result: " << os.str() << endl;
#endif
  VERIFY( os && os.str() == largebuf );

  // Make sure we can output a long float in fixed format
  // without seg-faulting (libstdc++/4402)
  double val2 = numeric_limits<double>::max();

  ostringstream os2;
  os2.precision(3);
  os2.setf(ios::fixed);
  os2 << val2;

  sprintf(largebuf, "%.*f", 3, val2);
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

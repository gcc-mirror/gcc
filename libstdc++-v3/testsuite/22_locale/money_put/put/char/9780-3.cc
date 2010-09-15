// { dg-require-namedlocale "de_DE" }
// { dg-require-namedlocale "es_ES" }

// Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
// Free Software Foundation, Inc.
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

#include <sstream>
#include <locale>
#include <testsuite_hooks.h>

int main()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
  locale l1 = locale("de_DE");
  locale l2 = locale("es_ES");
  
  const money_put<char>& mp = use_facet<money_put<char> >(l1);  
  ostringstream oss;
  oss.imbue(l2);
  oss.setf(ios_base::showbase);

  long double ld = -1234567890;
  mp.put(oss.rdbuf(), true, oss, ' ', ld); // -EUR  12.345.678,90
  string res = oss.str();

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 7)
  VERIFY( res == "-12.345.678,90 EUR " );
#else
  VERIFY( res == "-EUR  12.345.678,90" );
#endif

  return 0;
}

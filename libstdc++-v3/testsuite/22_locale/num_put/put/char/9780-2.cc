// { dg-require-namedlocale "de_DE" }
// { dg-require-namedlocale "es_ES" }

// Copyright (C) 2004, 2005, 2009 Free Software Foundation, Inc.
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
  
  const num_put<char>& np = use_facet<num_put<char> >(l1);  
  ostringstream oss;
  oss.imbue(l2);

  long l = 1234567890;
  np.put(oss.rdbuf(), oss, ' ', l); // 1234567890
  string res = oss.str();
  
  VERIFY( res == "1234567890" );

  return 0;
}

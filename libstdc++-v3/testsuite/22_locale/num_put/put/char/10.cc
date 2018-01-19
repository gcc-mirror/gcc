// 2005-07-11  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  locale loc_c = locale::classic();

  const string empty;

  stringstream ss;
  ss.imbue(loc_c);
  const num_put<char>& np = use_facet<num_put<char> >(ss.getloc()); 

  long l = -1;
  unsigned long ul = 0;

  ss.setf(ios::hex, ios::basefield);
  np.put(ss.rdbuf(), ss, '+', l);
  VERIFY( ss.str() != "1" );
  ss >> ul;
  VERIFY( ul == static_cast<unsigned long>(l) );

#ifdef _GLIBCXX_USE_LONG_LONG  
  long long ll = -1LL;
  unsigned long long ull = 0ULL;

  ss.str(empty);
  ss.clear();
  np.put(ss.rdbuf(), ss, '+', ll);
  VERIFY( ss.str() != "1" );
  ss >> ull;
  VERIFY( ull == static_cast<unsigned long long>(ll) );
#endif
}

int main()
{
  test01();
  return 0;
}



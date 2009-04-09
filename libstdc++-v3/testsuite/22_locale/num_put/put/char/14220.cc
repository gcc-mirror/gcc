// 2004-04-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004, 2009 Free Software Foundation
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

// On Solaris 10 x86, this test crashes in libc.  Inside libstdc++, 
// we call sprintf like so:
//   sprintf (buffer, "%.*f", 1000, 1.0)
// which crashes.
// { dg-do run { xfail { i?86*-*-solaris2.10 } } } 

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/14220
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  ostringstream oss;
  const num_put<char>& np = use_facet<num_put<char> >(oss.getloc());

  const int precision = 1000;

  oss.precision(precision);
  oss.setf(ios_base::fixed);
  np.put(oss.rdbuf(), oss, '+', 1.0);
  const string result = oss.str();
  VERIFY( result.size() == precision + 2 );
}

int main()
{
  test01();
  return 0;
}

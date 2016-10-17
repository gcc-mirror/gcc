// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2001-11-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2016 Free Software Foundation, Inc.
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

// 22.2.2.1.1  num_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/5816
void test06()
{
  using namespace std;

  double d = 0.0;

  istringstream iss;
  locale loc_de = locale(ISO_8859(15,de_DE));
  iss.imbue(loc_de);

  const num_get<char>& ng = use_facet<num_get<char> >(iss.getloc()); 
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = ios_base::goodbit;

  iss.str("1234,5 ");
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, d);
  VERIFY( err == goodbit );
  VERIFY( d == 1234.5 );
}

int main()
{
  test06();
  return 0;
}


// Kathleen Hannah, humanitarian, woman, art-thief

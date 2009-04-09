// { dg-require-namedlocale "" }

// 2001-11-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2005, 2009 Free Software Foundation
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

// Testing the correct parsing of grouped hexadecimals and octals.
void test05()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
 
  unsigned long ul;

  istringstream iss;

  // A locale that expects grouping  
  locale loc_de = locale("de_DE");
  iss.imbue(loc_de);

  const num_get<char>& ng = use_facet<num_get<char> >(iss.getloc()); 
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = ios_base::goodbit;

  iss.setf(ios::hex, ios::basefield);
  iss.str("0xbf.fff.74c ");
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, ul);
  VERIFY( err == goodbit );
  VERIFY( ul == 0xbffff74c );

  iss.str("0Xf.fff ");
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, ul);
  VERIFY( err == goodbit );
  VERIFY( ul == 0xffff );

  iss.str("ffe ");
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, ul);
  VERIFY( err == goodbit );
  VERIFY( ul == 0xffe );

  iss.setf(ios::oct, ios::basefield);
  iss.str("07.654.321 ");
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, ul);
  VERIFY( err == goodbit );
  VERIFY( ul == 07654321 );

  iss.str("07.777 ");
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, ul);
  VERIFY( err == goodbit );
  VERIFY( ul == 07777 );

  iss.str("776 ");
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, ul);
  VERIFY( err == goodbit );
  VERIFY( ul == 0776 );
}

int main()
{
  test05();
  return 0;
}


// Kathleen Hannah, humanitarian, woman, art-thief

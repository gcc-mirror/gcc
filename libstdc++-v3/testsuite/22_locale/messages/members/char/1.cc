// { dg-require-namedlocale "de_DE" }

// 2001-07-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2009 Free Software Foundation
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

// 22.2.7.1.1 messages members

#include <locale>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef std::messages<char>::catalog catalog;
  typedef std::messages<char>::string_type string_type;

  bool test __attribute__((unused)) = true;
  // This is defined through CXXFLAGS in scripts/testsuite_flags[.in].
  const char* dir = LOCALEDIR;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale("de_DE");
  VERIFY( loc_c != loc_de );

  // cache the messages facets
  const messages<char>& mssg_de = use_facet<messages<char> >(loc_de); 

  // catalog open(const string&, const locale&) const;
  // string_type get(catalog, int, int, const string_type& ) const; 
  // void close(catalog) const;

  // Check German (de_DE) locale.
  catalog cat_de = mssg_de.open("libstdc++", loc_c, dir);
  string s01 = mssg_de.get(cat_de, 0, 0, "please");
  string s02 = mssg_de.get(cat_de, 0, 0, "thank you");
  VERIFY ( s01 == "bitte" );
  VERIFY ( s02 == "danke" );
  mssg_de.close(cat_de);
}

int main()
{
  test01();
  return 0;
}

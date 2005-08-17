// { dg-require-namedlocale "" }

// 2001-07-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation
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

// 22.2.7.1.1 messages members

#include <locale>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  typedef std::messages<char>::catalog catalog;
  typedef std::messages<char>::string_type string_type;

  bool test __attribute__((unused)) = true;
  // This is defined through CXXFLAGS in scripts/testsuite_flags[.in].
  const char* dir = LOCALEDIR;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_fr = locale("fr_FR");
  VERIFY( loc_c != loc_fr );

  // cache the messages facets
  const messages<char>& mssg_fr = use_facet<messages<char> >(loc_fr); 

  // catalog open(const string&, const locale&) const;
  // string_type get(catalog, int, int, const string_type& ) const; 
  // void close(catalog) const;

  // Check French (fr_FR) locale.
  catalog cat_fr = mssg_fr.open("libstdc++", loc_c, dir);
  string s01 = mssg_fr.get(cat_fr, 0, 0, "please");
  string s02 = mssg_fr.get(cat_fr, 0, 0, "thank you");
  VERIFY ( s01 == "s'il vous plaît" );
  VERIFY ( s02 == "merci" );
  mssg_fr.close(cat_fr);
}

int main()
{
  test02();
  return 0;
}

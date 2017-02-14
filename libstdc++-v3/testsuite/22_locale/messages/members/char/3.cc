// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "fr_FR.ISO8859-15" }

// 2001-07-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

void test03()
{
  using namespace std;
  typedef std::messages<char>::catalog catalog;
  typedef std::messages<char>::string_type string_type;

  // This is defined through CXXFLAGS in scripts/testsuite_flags[.in].
  const char* dir = LOCALEDIR;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_us = locale(ISO_8859(1,en_US));
  locale loc_fr = locale(ISO_8859(15,fr_FR));
  VERIFY( loc_c != loc_us );
  VERIFY( loc_us != loc_fr );

  // cache the messages facets
  const messages<char>& mssg_us = use_facet<messages<char> >(loc_us); 
  const messages<char>& mssg_fr = use_facet<messages<char> >(loc_fr); 

  // catalog open(const string&, const locale&) const;
  // string_type get(catalog, int, int, const string_type& ) const; 
  // void close(catalog) const;

  // Check US (en_US) locale.
  catalog cat_us = mssg_fr.open("libstdc++", loc_c, dir);
  string s01 = mssg_us.get(cat_us, 0, 0, "please");
  string s02 = mssg_us.get(cat_us, 0, 0, "thank you");
  VERIFY ( s01 == "please" );
  VERIFY ( s02 == "thank you" );
  mssg_us.close(cat_us);
}

int main()
{
  test03();
  return 0;
}

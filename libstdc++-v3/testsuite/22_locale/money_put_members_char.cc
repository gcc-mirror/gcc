// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001 Free Software Foundation
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 22.2.6.2.1 money_put members

#include <locale>
#include <testsuite_hooks.h>

// XXX This test is not working for non-glibc locale models.
// { dg-do run { xfail *-*-* } }

void test01()
{
  using namespace std;
  typedef money_base::part part;
  typedef money_base::pattern pattern;

  bool test = true;
  string str;

  // basic construction
  locale loc_c = locale::classic();
  str = loc_c.name();

  locale loc_us("en_US");
  str = loc_us.name();
  VERIFY( loc_c != loc_us );

  locale loc_fr("fr_FR");
  str = loc_fr.name();
  VERIFY( loc_c != loc_fr );

  locale loc_de("de_DE");
  str = loc_de.name();
  VERIFY( loc_c != loc_de );

  VERIFY( loc_us != loc_fr );
  VERIFY( loc_us != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the money_put facets
  const money_put<char>& monp_c = use_facet<money_put<char> >(loc_c); 
  const money_put<char>& monp_us = use_facet<money_put<char> >(loc_us); 
  const money_put<char>& monp_fr = use_facet<money_put<char> >(loc_fr); 
  const money_put<char>& monp_de = use_facet<money_put<char> >(loc_de); 

  // sanity check the data is correct.
  // VERIFY( dp1 != dp2 );
}

int main()
{
  test01();

  return 0;
}

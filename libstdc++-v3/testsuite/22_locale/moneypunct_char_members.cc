// 2001-08-23 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.6.3.1 moneypunct members

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

  // cache the moneypunct facets
  const moneypunct<char>& monp_c = use_facet<moneypunct<char> >(loc_c); 
  const moneypunct<char>& monp_us = use_facet<moneypunct<char> >(loc_us); 
  const moneypunct<char>& monp_fr = use_facet<moneypunct<char> >(loc_fr); 
  const moneypunct<char>& monp_de = use_facet<moneypunct<char> >(loc_de); 

  // sanity check the data is correct.
  char dp1 = monp_c.decimal_point();
  char th1 = monp_c.thousands_sep();
  string g1 = monp_c.grouping();
  string cs1 = monp_c.curr_symbol();
  string ps1 = monp_c.positive_sign();
  string ns1 = monp_c.negative_sign();
  int fd1 = monp_c.frac_digits();
  pattern pos1 = monp_c.pos_format();
  pattern neg1 = monp_c.neg_format();

  char dp2 = monp_de.decimal_point();
  char th2 = monp_de.thousands_sep();
  string g2 = monp_de.grouping();
  string cs2 = monp_de.curr_symbol();
  string ps2 = monp_de.positive_sign();
  string ns2 = monp_de.negative_sign();
  int fd2 = monp_de.frac_digits();
  pattern pos2 = monp_de.pos_format();
  pattern neg2 = monp_de.neg_format();

  VERIFY( dp1 != dp2 );
  VERIFY( th1 != th2 );
  VERIFY( g1 != g2 );
  VERIFY( cs1 != cs2 );
  //  VERIFY( ps1 != ps2 );
  VERIFY( ns1 != ns2 );
  VERIFY( fd1 != fd2 );
  VERIFY(static_cast<part>(pos1.field[0]) != static_cast<part>(pos2.field[0]));
  VERIFY(static_cast<part>(pos1.field[1]) != static_cast<part>(pos2.field[1]));
  VERIFY(static_cast<part>(pos1.field[2]) != static_cast<part>(pos2.field[2]));
  VERIFY(static_cast<part>(pos1.field[3]) != static_cast<part>(pos2.field[3]));

  VERIFY(static_cast<part>(neg1.field[0]) != static_cast<part>(neg2.field[0]));
  VERIFY(static_cast<part>(neg1.field[1]) != static_cast<part>(neg2.field[1]));
  VERIFY(static_cast<part>(neg1.field[2]) != static_cast<part>(neg2.field[2]));
  VERIFY(static_cast<part>(neg1.field[3]) != static_cast<part>(neg2.field[3]));
}

int main()
{
  test01();

  return 0;
}

// { dg-require-namedlocale "de_DE" }

// 2001-08-23 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.6.3.1 moneypunct members

#include <locale>
#include <string>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  typedef money_base::part part;
  typedef money_base::pattern pattern;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale("de_DE");

  // cache the moneypunct facets
  typedef moneypunct<wchar_t, true> __money_true;
  typedef moneypunct<wchar_t, false> __money_false;
  const __money_true& monp_c_t = use_facet<__money_true>(loc_c); 
  const __money_false& monp_c_f = use_facet<__money_false>(loc_c); 
  const __money_true& monp_de_t = use_facet<__money_true>(loc_de); 

  // quick sanity check for data.
  wchar_t q1 = monp_c_t.decimal_point();
  wchar_t q2 = monp_c_t.thousands_sep();
  wchar_t q3 = monp_c_f.decimal_point();
  wchar_t q4 = monp_c_f.thousands_sep();
  VERIFY( q1 != wchar_t() );
  VERIFY( q2 != wchar_t() );
  VERIFY( q3 != wchar_t() );
  VERIFY( q4 != wchar_t() );

  // sanity check the data is correct.
  wchar_t dp1 = monp_c_t.decimal_point();
  wchar_t th1 = monp_c_t.thousands_sep();
  string g1 = monp_c_t.grouping();
  wstring cs1 = monp_c_t.curr_symbol();
  wstring ps1 = monp_c_t.positive_sign();
  wstring ns1 = monp_c_t.negative_sign();
  int fd1 = monp_c_t.frac_digits();
  pattern pos1 = monp_c_t.pos_format();
  pattern neg1 = monp_c_t.neg_format();

  wchar_t dp2 = monp_de_t.decimal_point();
  wchar_t th2 = monp_de_t.thousands_sep();
  string g2 = monp_de_t.grouping();
  wstring cs2 = monp_de_t.curr_symbol();
  wstring ps2 = monp_de_t.positive_sign();
  wstring ns2 = monp_de_t.negative_sign();
  int fd2 = monp_de_t.frac_digits();
  pattern pos2 = monp_de_t.pos_format();
  pattern neg2 = monp_de_t.neg_format();

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
  test02();
  return 0;
}

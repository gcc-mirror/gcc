// 2001-09-12 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation
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

// 22.2.6.1.1 money_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// test double version
void test03()
{
  using namespace std;
  typedef money_base::part part;
  typedef money_base::pattern pattern;
  typedef istreambuf_iterator<char> iterator_type;

  bool test = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_hk("en_HK");
  locale loc_fr("fr_FR@euro");
  locale loc_de("de_DE@euro");
  VERIFY( loc_c != loc_de );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the moneypunct facets
  typedef moneypunct<char, true> __money_true;
  typedef moneypunct<char, false> __money_false;
  const __money_true& monpunct_c_t = use_facet<__money_true>(loc_c); 
  const __money_true& monpunct_de_t = use_facet<__money_true>(loc_de); 
  const __money_false& monpunct_c_f = use_facet<__money_false>(loc_c); 
  const __money_false& monpunct_de_f = use_facet<__money_false>(loc_de); 
  const __money_true& monpunct_hk_t = use_facet<__money_true>(loc_hk); 
  const __money_false& monpunct_hk_f = use_facet<__money_false>(loc_hk); 

  // sanity check the data is correct.
  const string empty;

  // total EPA budget FY 2002
  const long double  digits1 = 720000000000.0;

  // est. cost, national missile "defense", expressed as a loss in USD 2001
  const long double digits2 = -10000000000000.0;  

  // input less than frac_digits
  const long double digits4 = -1.0;
  
  iterator_type end;
  istringstream iss;
  iss.imbue(loc_de);
  // cache the money_get facet
  const money_get<char>& mon_get = use_facet<money_get<char> >(iss.getloc()); 

  iss.str("7.200.000.000,00 ");
  iterator_type is_it01(iss);
  long double result1;
  ios_base::iostate err01 = ios_base::goodbit;
  mon_get.get(is_it01, end, true, iss, err01, result1);
  VERIFY( result1 == digits1 );
  VERIFY( err01 == ios_base::eofbit );

  iss.str("7.200.000.000,00 ");
  iterator_type is_it02(iss);
  long double result2;
  ios_base::iostate err02 = ios_base::goodbit;
  mon_get.get(is_it02, end, false, iss, err02, result2);
  VERIFY( result2 == digits1 );
  VERIFY( err02 == ios_base::eofbit );
}

int main()
{
  test03();
  return 0;
}

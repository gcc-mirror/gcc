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

// test string version
void test02()
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
  const string digits1("720000000000");

  // est. cost, national missile "defense", expressed as a loss in USD 2001
  const string digits2("-10000000000000");  

  // not valid input
  const string digits3("-A"); 

  // input less than frac_digits
  const string digits4("-1");
  
  iterator_type end;
  istringstream iss;
  iss.imbue(loc_hk);
  // cache the money_get facet
  const money_get<char>& mon_get = use_facet<money_get<char> >(iss.getloc()); 

  // now try with showbase, to get currency symbol in format
  iss.setf(ios_base::showbase);

  iss.str("HK$7,200,000,000.00"); 
  iterator_type is_it09(iss);
  string result9;
  ios_base::iostate err09 = ios_base::goodbit;
  mon_get.get(is_it09, end, false, iss, err09, result9);
  VERIFY( result9 == digits1 );
  VERIFY( err09 == ios_base::eofbit );

  iss.str("(HKD 100,000,000,000.00)"); 
  iterator_type is_it10(iss);
  string result10;
  ios_base::iostate err10 = ios_base::goodbit;
  mon_get.get(is_it10, end, true, iss, err10, result10);
  VERIFY( result10 == digits2 );
  VERIFY( err10 == ios_base::goodbit );

  iss.str("(HKD .01)"); 
  iterator_type is_it11(iss);
  string result11;
  ios_base::iostate err11 = ios_base::goodbit;
  mon_get.get(is_it11, end, true, iss, err11, result11);
  VERIFY( result11 == digits4 );
  VERIFY( err11 == ios_base::goodbit );

  // for the "en_HK" locale the parsing of the very same input streams must
  // be successful without showbase too, since the symbol field appears in
  // the first positions in the format and the symbol, when present, must be
  // consumed.
  iss.unsetf(ios_base::showbase);

  iss.str("HK$7,200,000,000.00"); 
  iterator_type is_it12(iss);
  string result12;
  ios_base::iostate err12 = ios_base::goodbit;
  mon_get.get(is_it12, end, false, iss, err12, result12);
  VERIFY( result12 == digits1 );
  VERIFY( err12 == ios_base::eofbit );

  iss.str("(HKD 100,000,000,000.00)"); 
  iterator_type is_it13(iss);
  string result13;
  ios_base::iostate err13 = ios_base::goodbit;
  mon_get.get(is_it13, end, true, iss, err13, result13);
  VERIFY( result13 == digits2 );
  VERIFY( err13 == ios_base::goodbit );

  iss.str("(HKD .01)"); 
  iterator_type is_it14(iss);
  string result14;
  ios_base::iostate err14 = ios_base::goodbit;
  mon_get.get(is_it14, end, true, iss, err14, result14);
  VERIFY( result14 == digits4 );
  VERIFY( err14 == ios_base::goodbit );
}

int main()
{
  test02();
  return 0;
}

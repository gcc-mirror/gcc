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

void test05()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // Check money_get works with other iterators besides streambuf
  // input iterators.
  typedef wstring::const_iterator iter_type;
  typedef money_get<wchar_t, iter_type> mon_get_type;
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = goodbit;
  const locale loc_c = locale::classic();
  const wstring str = L"0.01Eleanor Roosevelt";

  wistringstream iss; 
  iss.imbue(locale(loc_c, new mon_get_type));

  // Iterator advanced, state, output.
  const mon_get_type& mg = use_facet<mon_get_type>(iss.getloc());

  // 01 wstring
  wstring res1;
  iter_type end1 = mg.get(str.begin(), str.end(), false, iss, err, res1);
  wstring rem1(end1, str.end());
  VERIFY( err == goodbit );
  VERIFY( res1 == L"1" );
  VERIFY( rem1 == L"Eleanor Roosevelt" );

  // 02 long double
  iss.clear();
  err = goodbit;
  long double res2;
  iter_type end2 = mg.get(str.begin(), str.end(), false, iss, err, res2);
  wstring rem2(end2, str.end());
  VERIFY( err == goodbit );
  VERIFY( res2 == 1 );
  VERIFY( rem2 == L"Eleanor Roosevelt" );
}

int main()
{
  test05();
  return 0;
}

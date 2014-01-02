// 2005-06-28  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

struct Punct: std::numpunct<wchar_t>
{
  std::string do_grouping() const { return "\1"; }
  wchar_t do_thousands_sep() const { return L'#'; }
};

// libstdc++/22131
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;
  
  bool test __attribute__((unused)) = true;

  wistringstream iss1, iss2;
  iss1.imbue(locale(iss1.getloc(), new Punct));
  const num_get<wchar_t>& ng1 = use_facet<num_get<wchar_t> >(iss1.getloc());

  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  long l = 0l;
  long l1 = 1l;
  long l2 = 2l;
  long l3 = 3l;
  double d = 0.0;
  double d1 = 1.0;
  double d2 = 2.0;

  iss1.str(L"00#0#1");
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == (ios_base::eofbit | ios_base::failbit) );
  VERIFY( l == l1 );

  iss1.str(L"000##2");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == ios_base::failbit );
  VERIFY( *end == L'#' );
  VERIFY( l == 0 );

  iss1.str(L"0#0#0#2");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == ios_base::eofbit );
  VERIFY( l == l2 );

  iss1.str(L"00#0#1");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, d);
  VERIFY( err == (ios_base::eofbit | ios_base::failbit) );
  VERIFY( d == d1 );

  iss1.str(L"000##2");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, d);
  VERIFY( err == ios_base::failbit );
  VERIFY( *end == L'#' );
  VERIFY( d == 0.0 );

  iss1.str(L"0#0#0#2");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, d);
  VERIFY( err == ios_base::eofbit );
  VERIFY( d == d2 );

  iss1.str(L"0#0");
  iss1.clear();
  iss1.unsetf(ios::basefield);
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == ios_base::failbit );
  VERIFY( *end == L'#' );
  VERIFY( l == 0 );

  iss1.str(L"00#0#3");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == ios_base::eofbit );
  VERIFY( l == l3 );

  iss1.str(L"00#02");
  iss1.clear();
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == (ios_base::eofbit | ios_base::failbit) );
  VERIFY( l == l2 );
}

int main()
{
  test01();
  return 0;
}

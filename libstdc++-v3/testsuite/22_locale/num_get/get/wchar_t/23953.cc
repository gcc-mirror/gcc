// 2005-09-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

using namespace std;

struct Punct1: numpunct<wchar_t>
{ string do_grouping() const { return string(1, char(-1)); } };

struct Punct2: numpunct<wchar_t>
{ string do_grouping() const { return string("\002") + char(-1); } };

struct Punct3: numpunct<wchar_t>
{ string do_grouping() const { return string("\001\002") + char(-1); } };

// libstdc++/23953
void test01()
{
  typedef istreambuf_iterator<wchar_t> iterator_type;
  
  wistringstream iss1, iss2, iss3;
  iss1.imbue(locale(iss1.getloc(), new Punct1));
  iss2.imbue(locale(iss2.getloc(), new Punct2));
  iss3.imbue(locale(iss3.getloc(), new Punct3));
  const num_get<wchar_t>& ng1 = use_facet<num_get<wchar_t> >(iss1.getloc());
  const num_get<wchar_t>& ng2 = use_facet<num_get<wchar_t> >(iss2.getloc());
  const num_get<wchar_t>& ng3 = use_facet<num_get<wchar_t> >(iss3.getloc());

  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  long l = 0l;
  long l1 = 12345l;
  long l2 = 12345678l;
  double d = 0.0;
  double d1 = 1234567.0;

  iss1.str(L"12345");
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == ios_base::eofbit );
  VERIFY( l == l1 );

  iss2.str(L"123456,78");
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, l);
  VERIFY( err == ios_base::eofbit );
  VERIFY( l == l2 );

  iss3.str(L"1234,56,7.0");
  err = ios_base::goodbit;
  end = ng3.get(iss3.rdbuf(), 0, iss3, err, d);
  VERIFY( err == ios_base::eofbit );
  VERIFY( d == d1 );
}

int main()
{
  test01();
  return 0;
}

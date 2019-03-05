// 2003-12-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

struct Punct1: std::numpunct<wchar_t>
{ std::string do_grouping() const { return "\003\002\001"; } };

struct Punct2: std::numpunct<wchar_t>
{ std::string do_grouping() const { return "\001\003"; } };

// libstdc++/13369
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  wistringstream iss1, iss2;
  iss1.imbue(locale(iss1.getloc(), new Punct1));
  iss2.imbue(locale(iss2.getloc(), new Punct2));
  const num_get<wchar_t>& ng1 = use_facet<num_get<wchar_t> >(iss1.getloc()); 
  const num_get<wchar_t>& ng2 = use_facet<num_get<wchar_t> >(iss2.getloc()); 

  ios_base::iostate err = ios_base::goodbit;
  iterator_type end;
  long l = 0l;
  long l1 = 12345678l;
  double d = 0.0;
  double d1 = 1234567.0;
  double d2 = 123456.0;

  iss1.str(L"1,2,3,45,678");
  err = ios_base::goodbit;
  end = ng1.get(iss1.rdbuf(), 0, iss1, err, l);
  VERIFY( err == ios_base::eofbit );
  VERIFY( l == l1 );

  iss2.str(L"123,456,7.0");
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, d);
  VERIFY( err == ios_base::eofbit );
  VERIFY( d == d1 );

  iss2.str(L"12,345,6.0");
  iss2.clear();
  err = ios_base::goodbit;
  end = ng2.get(iss2.rdbuf(), 0, iss2, err, d);
  VERIFY( err == ios_base::eofbit );
  VERIFY( d == d2 );
}

int main()
{
  test01();
  return 0;
}

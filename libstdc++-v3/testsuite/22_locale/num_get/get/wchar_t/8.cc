// 2003-12-15  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  bool test __attribute__((unused)) = true;

  bool b;

  // cache the num_get facet
  wistringstream iss;
  const num_get<wchar_t>& ng = use_facet<num_get<wchar_t> >(iss.getloc()); 
  const ios_base::iostate goodbit = ios_base::goodbit;
  const ios_base::iostate failbit = ios_base::failbit;
  ios_base::iostate err;
  iterator_type end;

  iss.setf(ios_base::boolalpha);
  iss.str(L"faLse");
  err = goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, b);
  VERIFY( *end == L'L' );
  VERIFY( err == failbit );

  iss.str(L"falsr");
  iss.clear();  
  err = goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, b);
  VERIFY( *end == L'r' );
  VERIFY( err == failbit );

  iss.str(L"trus");
  iss.clear();
  err = goodbit;
  end = ng.get(iss.rdbuf(), 0, iss, err, b);
  VERIFY( *end == L's' );
  VERIFY( err == failbit );
}

int main()
{
  test01();
  return 0;
}

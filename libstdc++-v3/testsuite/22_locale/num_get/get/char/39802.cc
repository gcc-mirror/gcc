// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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
#include <limits>
#include <testsuite_hooks.h>

// libstdc++/39802
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;

  stringstream ss;
  const num_get<char>& ng = use_facet<num_get<char> >(ss.getloc()); 
  ios_base::iostate err;
  iterator_type end;
  const string empty;

  unsigned long ul0 = 1;
  const unsigned long ul1 = numeric_limits<unsigned long>::max();

  ss << "-0";
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ul0);
  VERIFY( err == ios_base::eofbit );
  VERIFY( ul0 == 0 );

  ss.clear();
  ss.str(empty);
  ss << "-1";
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ul0);
  VERIFY( err == ios_base::eofbit );
  VERIFY( ul0 == ul1 );

  ss.clear();
  ss.str(empty);
  ss << '-' << ul1;
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ul0);
  VERIFY( err == ios_base::eofbit );
  VERIFY( ul0 == 1 );

  ss.clear();
  ss.str(empty);
  ss << '-' << ul1 << '0';
  err = ios_base::goodbit;
  end = ng.get(ss.rdbuf(), 0, ss, err, ul0);
  VERIFY( err == (ios_base::eofbit | ios_base::failbit) );
  VERIFY( ul0 == ul1 );
}

int main()
{
  test01();
  return 0;
}

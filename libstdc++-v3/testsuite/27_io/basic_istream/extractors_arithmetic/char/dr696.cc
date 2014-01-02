// 2009-07-15  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2009-2014 Free Software Foundation, Inc.
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

// 27.6.1.2.2 arithmetic extractors

#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

// DR 696.
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  short s1 = 0;
  ostringstream oss1;
  oss1 << numeric_limits<short>::max();
  istringstream iss1(oss1.str());
  iss1 >> s1;
  VERIFY( s1 == numeric_limits<short>::max() );
  VERIFY( !iss1.fail() && iss1.eof() );

  short s2 = 0;
  ostringstream oss2;
  oss2 << static_cast<long long>(numeric_limits<short>::max()) + 1;
  istringstream iss2(oss2.str());
  iss2 >> s2;
  VERIFY( s2 == numeric_limits<short>::max() );
  VERIFY( iss2.fail() && iss2.eof() );

  short s3 = 0;
  ostringstream oss3;
  oss3 << numeric_limits<short>::min();
  istringstream iss3(oss3.str());
  iss3 >> s3;
  VERIFY( s3 == numeric_limits<short>::min() );
  VERIFY( !iss3.fail() && iss3.eof() );

  short s4 = 0;
  ostringstream oss4;
  oss4 << static_cast<long long>(numeric_limits<short>::min()) - 1;
  istringstream iss4(oss4.str());
  iss4 >> s4;
  VERIFY( s4 == numeric_limits<short>::min() );
  VERIFY( iss4.fail() && iss4.eof() );

  int i1 = 0;
  ostringstream oss5;
  oss5 << numeric_limits<int>::max();
  istringstream iss5(oss5.str());
  iss5 >> i1;
  VERIFY( i1 == numeric_limits<int>::max() );
  VERIFY( !iss5.fail() && iss5.eof() );

  int i2 = 0;
  ostringstream oss6;
  oss6 << static_cast<long long>(numeric_limits<int>::max()) + 1;
  istringstream iss6(oss6.str());
  iss6 >> i2;
  VERIFY( i1 == numeric_limits<int>::max() );
  VERIFY( iss6.fail() && iss6.eof() );

  int i3 = 0;
  ostringstream oss7;
  oss7 << numeric_limits<int>::min();
  istringstream iss7(oss7.str());
  iss7 >> i3;
  VERIFY( i3 == numeric_limits<int>::min() );
  VERIFY( !iss7.fail() && iss7.eof() );

  int i4 = 0;
  ostringstream oss8;
  oss8 << static_cast<long long>(numeric_limits<int>::min()) - 1;
  istringstream iss8(oss8.str());
  iss8 >> i4;
  VERIFY( i4 == numeric_limits<int>::min() );
  VERIFY( iss8.fail() && iss8.eof() );
}

int main()
{
  test01();
  return 0;
}

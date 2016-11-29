// 2006-08-05  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2016 Free Software Foundation, Inc.
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

// 23.2.5 vector<bool> resize

#include <vector>
#include <testsuite_hooks.h>

const bool A1[] = {0};
const bool A2[] = {0, 0, 0};
const bool A3[] = {0, 0};
const bool A4[] = {0, 0, 1, 1, 1, 1, 1};
const bool A5[] = {0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
const bool A6[] = {0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		   1, 1, 1, 1};
const bool A7[] = {0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		   1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0};
const bool A8[] = {0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		   1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
const bool A9[] = {0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		   1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		   0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
const bool A10[] = {0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		    1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
		    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		    1, 1};
const bool A11[] = {0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0};

const unsigned N1 = sizeof(A1) / sizeof(bool);
const unsigned N2 = sizeof(A2) / sizeof(bool);
const unsigned N3 = sizeof(A3) / sizeof(bool);
const unsigned N4 = sizeof(A4) / sizeof(bool);
const unsigned N5 = sizeof(A5) / sizeof(bool);
const unsigned N6 = sizeof(A6) / sizeof(bool);
const unsigned N7 = sizeof(A7) / sizeof(bool);
const unsigned N8 = sizeof(A8) / sizeof(bool);
const unsigned N9 = sizeof(A9) / sizeof(bool);
const unsigned N10 = sizeof(A10) / sizeof(bool);
const unsigned N11 = sizeof(A11) / sizeof(bool);

void
test01()
{
  typedef std::vector<bool>  vec_type;

  vec_type v;

  v.resize(1);
  VERIFY( v.size() == 1 );
  VERIFY( std::equal(v.begin(), v.end(), A1) );

  v.resize(3);
  VERIFY( v.size() == 3 );
  VERIFY( std::equal(v.begin(), v.end(), A2) );

  v.resize(2);
  VERIFY( v.size() == 2 );
  VERIFY( std::equal(v.begin(), v.end(), A3) );

  v.resize(7, true);
  VERIFY( v.size() == 7 );
  VERIFY( std::equal(v.begin(), v.end(), A4) );

  v.resize(18, false);
  VERIFY( v.size() == 18 );
  VERIFY( std::equal(v.begin(), v.end(), A5) );

  v.resize(40, true);
  VERIFY( v.size() == 40 );
  VERIFY( std::equal(v.begin(), v.end(), A6) );

  v.resize(80, false);
  VERIFY( v.size() == 80 );
  VERIFY( std::equal(v.begin(), v.end(), A7) );

  v.resize(90, true);
  VERIFY( v.size() == 90 );
  VERIFY( std::equal(v.begin(), v.end(), A8) );

  v.resize(100, false);
  VERIFY( v.size() == 100 );
  VERIFY( std::equal(v.begin(), v.end(), A9) );

  v.resize(200, true);
  VERIFY( v.size() == 200 );
  VERIFY( std::equal(v.begin(), v.end(), A10) );

  v.resize(13, true);
  VERIFY( v.size() == 13 );
  VERIFY( std::equal(v.begin(), v.end(), A11) );

  v.resize(13, false);
  VERIFY( v.size() == 13 );
  VERIFY( std::equal(v.begin(), v.end(), A11) );
}

int main()
{
  test01();
  return 0;
}

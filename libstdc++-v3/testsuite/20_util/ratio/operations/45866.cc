// { dg-do run { target c++11 } }

// 2010-10-18  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <ratio>
#include <testsuite_hooks.h>

// libstdc++/45866
void test01()
{
  typedef std::ratio<1, 4>::type r_type1;
  typedef std::ratio<3, 2>::type r_type2;

  typedef std::ratio_add<r_type1, r_type2> ra_type;

  VERIFY( ra_type::num == ra_type::type::num );
  VERIFY( ra_type::den == ra_type::type::den );
  VERIFY( ra_type::num == 7 );
  VERIFY( ra_type::den == 4 );

  typedef std::ratio_subtract<r_type1, r_type2> rs_type;

  VERIFY( rs_type::num == rs_type::type::num );
  VERIFY( rs_type::den == rs_type::type::den );
  VERIFY( rs_type::num == -5 );
  VERIFY( rs_type::den == 4 );

  typedef std::ratio_multiply<r_type1, r_type2> rm_type;

  VERIFY( rm_type::num == rm_type::type::num );
  VERIFY( rm_type::den == rm_type::type::den );
  VERIFY( rm_type::num == 3 );
  VERIFY( rm_type::den == 8 );

  typedef std::ratio_divide<r_type1, r_type2> rd_type;

  VERIFY( rd_type::num == rd_type::type::num );
  VERIFY( rd_type::den == rd_type::type::den );
  VERIFY( rd_type::num == 1 );
  VERIFY( rd_type::den == 6 );
}

int main()
{
  test01();
  return 0;
}

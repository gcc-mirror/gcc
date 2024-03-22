// { dg-do run { target c++11 } }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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
#include <limits>
#include <testsuite_hooks.h>

// libstdc++/47913
void test01()
{
  using namespace std;

  const intmax_t m1 = (intmax_t)1 << (4 * sizeof(intmax_t) - 1);
  typedef ratio_add<ratio<1, (m1 - 1) * (m1 - 2)>,
                    ratio<1, (m1 - 3) * (m1 - 2)> > ra_type1;
  VERIFY( ra_type1::num == 2 );
  VERIFY( ra_type1::den == (m1 - 1) * (m1 - 3) );

  const intmax_t m2 = numeric_limits<intmax_t>::max();
  typedef ratio_add<ratio<m2, 2>,
                    ratio<-m2, 3> > ra_type2;
  VERIFY( ra_type2::num == m2 );
  VERIFY( ra_type2::den == 6 );

  typedef ratio_add<ratio<m2 / 7 * 5 - 1, 5>,
                    ratio<-m2 + 2, 7> > ra_type3;
  ra_type3();

  const intmax_t m3 = numeric_limits<intmax_t>::max() - 1;
  typedef ratio_add<ratio<-m3 / 7 * 5 - 1, 5>,
                    ratio<m3, 7> > ra_type4;
  ra_type4();

  const intmax_t m4 = numeric_limits<intmax_t>::max() / 2;
  typedef ratio_add<ratio<m4 - 5, 15>,
                    ratio<m4, 35> > ra_type5;
  VERIFY( ra_type5::num == (2 * m4 - 7) );
  VERIFY( ra_type5::den == 21 );
}

int main()
{
  test01();
  return 0;
}

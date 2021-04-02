// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <valarray>
#include <testsuite_hooks.h>

const std::valarray<int> v{
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
};

bool
all_of(const std::valarray<bool>& vals)
{
  for (bool b : vals)
    if (!b)
      return false;
  return true;
}

void
test01()
{
  // PR libstdc++/83860
  const std::valarray<int> va(v), vb(v), vc(v);
  auto sum = va + vb + vc;
  std::valarray<int> vsum = sum;
  VERIFY( all_of( vsum == (3 * v) ) );
}

void
test02()
{
  auto neg = -(-v);
  std::valarray<int> vneg = neg;
  VERIFY( all_of( vneg == v ) );
}

void
test03()
{
  const std::valarray<int> va(v), vb(v);
  auto diff = va + -vb;
  std::valarray<int> vdiff = diff;
  VERIFY( all_of( vdiff == (va - vb) ) );
}

void
test04()
{
  const std::valarray<int> va(v), vb(v);
  auto sum = -va + -vb;
  std::valarray<int> vsum = sum;
  VERIFY( all_of( vsum == (-2 * v) ) );
}

void
test05()
{
  const std::valarray<int> va(v), vb(v);
  auto sum = -(-va + -vb);
  std::valarray<int> vsum = sum;
  VERIFY( all_of( vsum == (2 * v) ) );
}

void
test06()
{
  auto prod = 3 * +v * 2;
  std::valarray<int> vprod = prod;
  VERIFY( all_of( vprod == (6 * v) ) );
}

void
test07()
{
  const std::valarray<int> va(v), vb(v);
  auto valfun = [](int i) { return i; };
  auto reffun = [](const int& i) { return i; };
  auto sum = (va.apply(valfun) + vb.apply(reffun));
  std::valarray<int> vsum = sum;
  VERIFY( all_of( vsum == (va + vb) ) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
}

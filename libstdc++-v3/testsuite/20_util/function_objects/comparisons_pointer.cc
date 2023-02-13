// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-do run }

#include <functional>
#include <sstream>
#include <testsuite_hooks.h>

int b[8];
int a[8];

void
test01()
{
  int* p = a + 8;
  std::greater<int*> gt;

  std::stringstream ss;
  ss << gt(p, b) << ' ' << gt(b, p) << ' ' << (!gt(p, b) && !gt(b, p));
  int sum = 0, n = 0;
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

#if __cplusplus >= 201402L
  static_assert( gt(a+1, a), "constexpr greater<int*>" );
  static_assert( !gt(a, a+1), "constexpr greater<int*>" );

  ss.str("");
  ss.clear();
  sum = 0;
  int* p2 = a + 8;
  std::greater<> gt2;
  ss << gt2(p2, b) << ' ' << gt2(b, p2) << ' ' << (!gt2(p2, b) && !gt2(b, p2));
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

  static_assert( gt2(a+1, a), "constexpr greater<>" );
  static_assert( !gt2(a, a+1), "constexpr greater<>" );
#endif
}

void
test02()
{
  int* p = a + 8;
  std::less<int*> lt;

  std::stringstream ss;
  ss << lt(p, b) << ' ' << lt(b, p) << ' ' << (!lt(p, b) && !lt(b, p));
  int sum = 0, n = 0;
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

#if __cplusplus >= 201402L
  static_assert( lt(a, a+1), "constexpr less<int*>" );
  static_assert( !lt(a+1, a), "constexpr less<int*>" );

  ss.str("");
  ss.clear();
  sum = 0;
  int* p2 = a + 8;
  std::less<> lt2;
  ss << lt2(p2, b) << ' ' << lt2(b, p2) << ' ' << (!lt2(p2, b) && !lt2(b, p2));
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

  static_assert( lt2(a, a+1), "constexpr less<>" );
  static_assert( !lt2(a+1, a), "constexpr less<>" );
#endif
}

void
test03()
{
  int* p = a + 8;
  std::greater_equal<int*> ge;

  std::stringstream ss;
  ss << !ge(p, b) << ' ' << !ge(b, p) << ' ' << (ge(p, b) && ge(b, p));
  int sum = 0, n = 0;
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

#if __cplusplus >= 201402L
  static_assert( !ge(a, a+1), "constexpr greater_equal<int*>" );
  static_assert( ge(a, a), "constexpr greater_equal<int*>" );
  static_assert( ge(a+1, a), "constexpr greater_equal<int*>" );

  ss.str("");
  ss.clear();
  sum = 0;
  int* p2 = a + 8;
  std::greater_equal<> ge2;
  ss << !ge2(p2, b) << ' ' << !ge2(b, p2) << ' ' << (ge2(p2, b) && ge2(b, p2));
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

  static_assert( !ge2(a, a+1), "constexpr greater_equal<>" );
  static_assert( ge2(a, a), "constexpr greater_equal<>" );
  static_assert( ge2(a+1, a), "constexpr greater_equal<>" );
#endif
}

void
test04()
{
  int* p = a + 8;
  std::less_equal<int*> le;

  std::stringstream ss;
  ss << !le(p, b) << ' ' << !le(b, p) << ' ' << (le(p, b) && le(b, p));
  int sum = 0, n = 0;
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

#if __cplusplus >= 201402L
  static_assert( !le(a+1, a), "constexpr less_equal<int*>" );
  static_assert( le(a, a), "constexpr less_equal<int*>" );
  static_assert( le(a, a+1), "constexpr less_equal<int*>" );

  ss.str("");
  ss.clear();
  sum = 0;
  int* p2 = a + 8;
  std::less_equal<> le2;
  ss << !le2(p2, b) << ' ' << !le2(b, p2) << ' ' << (le2(p2, b) && le2(b, p2));
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

  static_assert( !le2(a+1, a), "constexpr less_equal<>" );
  static_assert( le2(a, a), "constexpr less_equal<>" );
  static_assert( le2(a, a+1), "constexpr less_equal<>" );
#endif
}

struct X {
  operator const X*() const { return this; }
};

X x;
X y[4];

void
test05()
{
  std::less<const X*> lt;
  X* p = y + 4;
  std::stringstream ss;
  ss << lt(x, p) << ' ' << lt(p, x) << ' ' << (!lt(p, x) && !lt(x, p));
  int sum = 0;
  int n = 0;
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

#if __cplusplus >= 201402L
  static_assert( lt(y, y+1), "constexpr less<const X*>" );
  static_assert( !lt(y+1, y), "constexpr less<const X*>" );

  ss.str("");
  ss.clear();
  sum = 0;
  X* p2 = y + 4;
  std::less<> lt2;
  ss << lt2(x, p2) << ' ' << lt2(p2, x) << ' ' << (!lt2(x, p2) && !lt2(p2, x));
  while (ss >> n)
    sum += n;
  VERIFY( sum == 1 );

  static_assert( lt2(y, y+1), "constexpr less<>" );
  static_assert( !lt2(y+1, y), "constexpr less<>" );
#endif
}

struct Overloaded {
  bool operator>(int) { return true; }
  bool operator<(int) { return false; }
  bool operator>=(int) { return true; }
  bool operator<=(int) { return false; }
};
bool operator>(Overloaded, Overloaded) { return false; }
bool operator<(Overloaded, Overloaded) { return false; }
bool operator>=(Overloaded, Overloaded) { return true; }
bool operator<=(Overloaded, Overloaded) { return true; }

void
test06()
{
#if __cplusplus >= 201402L
  std::greater<void> gt;
  std::less<void> lt;
  std::greater_equal<void> ge;
  std::less_equal<void> le;

  Overloaded o;
  VERIFY( !gt(o, o) );
  VERIFY( !lt(o, o) );
  VERIFY( ge(o, o) );
  VERIFY( le(o, o) );

  VERIFY( gt(o, 1) );
  VERIFY( !lt(o, 1) );
  VERIFY( ge(o, 1) );
  VERIFY( !le(o, 1) );
#endif
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
}

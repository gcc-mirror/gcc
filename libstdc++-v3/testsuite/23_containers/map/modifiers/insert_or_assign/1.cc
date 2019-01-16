// { dg-options "-std=gnu++17" }
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PROFILE" } }

// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

#include <utility>
#include <map>
#include <testsuite_hooks.h>

struct Val
{
  bool moved_from_ctor = false;
  bool moved_from_assign = false;
  int val;
  Val(int val = 0) : val(val) {}
  Val(const Val& other) : val(other.val)
  {
  }
  Val(Val&& other) : val(other.val)
  {
    other.moved_from_ctor = true;
  }
  Val& operator=(Val&& other)
  {
    val = other.val;
    other.moved_from_assign = true;
    return *this;
  }
  Val& operator=(const Val& other)
  {
    val = other.val;
    return *this;
  }
};

bool operator<(const Val& a, const Val& b)
{
  return a.val < b.val;
}

void test01()
{
  typedef std::map<int, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(0, Val(5));
  VERIFY(res1.second);
  VERIFY(res1.first != m.end());
  VERIFY(m[0].val == 5);
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(0, std::move(v1));
  VERIFY(!res2.second);
  VERIFY(res2.first == res1.first);
  VERIFY(m[0].val == 6);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(v1.moved_from_assign);
  VERIFY(m.size() == 1);
  v1.moved_from_assign = false;
  auto res3 = m.insert_or_assign(1, std::move(v1));
  VERIFY(res3.first != res1.first && res3.first != m.end());
  VERIFY(res3.second);
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

void test02()
{
  typedef std::map<int, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(m.begin(), 0, Val(5));
  VERIFY(res1 != m.end());
  VERIFY(m[0].val == 5);
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(m.begin(), 0, std::move(v1));
  VERIFY(res2 == res1);
  VERIFY(m[0].val == 6);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(v1.moved_from_assign);
  VERIFY(m.size() == 1);
  v1.moved_from_assign = false;
  auto res3 = m.insert_or_assign(m.begin(), 1, std::move(v1));
  VERIFY(res3 != res1 && res3 != m.end());
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

void test03()
{
  typedef std::map<Val, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(0, Val(5));
  VERIFY(res1.second);
  VERIFY(res1.first != m.end());
  VERIFY(m[0].val == 5);
  Val k1{0};
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(std::move(k1), std::move(v1));
  VERIFY(!res2.second);
  VERIFY(res2.first == res1.first);
  VERIFY(m[0].val == 6);
  VERIFY(!k1.moved_from_ctor);
  VERIFY(!k1.moved_from_assign);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(v1.moved_from_assign);
  VERIFY(m.size() == 1);
  Val k2{1};
  v1.moved_from_assign = false;
  auto res3 = m.insert_or_assign(std::move(k2), std::move(v1));
  VERIFY(res3.first != res1.first && res3.first != m.end());
  VERIFY(res3.second);
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(k2.moved_from_ctor);
  VERIFY(!k2.moved_from_assign);
  VERIFY(v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

void test04()
{
  typedef std::map<Val, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(m.begin(), 0, Val(5));
  VERIFY(res1 != m.end());
  VERIFY(m[0].val == 5);
  Val k1{0};
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(m.begin(), std::move(k1), std::move(v1));
  VERIFY(res2 == res1);
  VERIFY(m[0].val == 6);
  VERIFY(!k1.moved_from_ctor);
  VERIFY(!k1.moved_from_assign);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(v1.moved_from_assign);
  VERIFY(m.size() == 1);
  Val k2{1};
  v1.moved_from_assign = false;
  auto res3 = m.insert_or_assign(m.begin(), std::move(k2), std::move(v1));
  VERIFY(res3 != res1 && res3 != m.end());
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(k2.moved_from_ctor);
  VERIFY(!k2.moved_from_assign);
  VERIFY(v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

void test05()
{
  typedef std::map<int, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(0, Val(5));
  VERIFY(res1.second);
  VERIFY(res1.first != m.end());
  VERIFY(m[0].val == 5);
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(0, v1);
  VERIFY(!res2.second);
  VERIFY(res2.first == res1.first);
  VERIFY(m[0].val == 6);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 1);
  auto res3 = m.insert_or_assign(1, v1);
  VERIFY(res3.first != res1.first && res3.first != m.end());
  VERIFY(res3.second);
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

void test06()
{
  typedef std::map<int, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(m.begin(), 0, Val(5));
  VERIFY(res1 != m.end());
  VERIFY(m[0].val == 5);
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(m.begin(), 0, v1);
  VERIFY(res2 == res1);
  VERIFY(m[0].val == 6);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 1);
  auto res3 = m.insert_or_assign(m.begin(), 1, v1);
  VERIFY(res3 != res1 && res3 != m.end());
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

void test07()
{
  typedef std::map<Val, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(0, Val(5));
  VERIFY(res1.second);
  VERIFY(res1.first != m.end());
  VERIFY(m[0].val == 5);
  Val k1{0};
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(k1, v1);
  VERIFY(!res2.second);
  VERIFY(res2.first == res1.first);
  VERIFY(m[0].val == 6);
  VERIFY(!k1.moved_from_ctor);
  VERIFY(!k1.moved_from_assign);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 1);
  Val k2{1};
  auto res3 = m.insert_or_assign(k2, v1);
  VERIFY(res3.first != res1.first && res3.first != m.end());
  VERIFY(res3.second);
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(!k2.moved_from_ctor);
  VERIFY(!k2.moved_from_assign);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

void test08()
{
  typedef std::map<Val, Val> Map;
  Map m;
  auto res1 = m.insert_or_assign(m.begin(), 0, Val(5));
  VERIFY(res1 != m.end());
  VERIFY(m[0].val == 5);
  Val k1{0};
  Val v1{6};
  VERIFY(m.size() == 1);
  auto res2 = m.insert_or_assign(m.begin(), k1, v1);
  VERIFY(res2 == res1);
  VERIFY(m[0].val == 6);
  VERIFY(!k1.moved_from_ctor);
  VERIFY(!k1.moved_from_assign);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 1);
  Val k2{1};
  auto res3 = m.insert_or_assign(m.begin(), k2, v1);
  VERIFY(res3 != res1 && res3 != m.end());
  VERIFY(m[0].val == 6);
  VERIFY(m[1].val == 6);
  VERIFY(!k2.moved_from_ctor);
  VERIFY(!k2.moved_from_assign);
  VERIFY(!v1.moved_from_ctor);
  VERIFY(!v1.moved_from_assign);
  VERIFY(m.size() == 2);
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  return 0;
}

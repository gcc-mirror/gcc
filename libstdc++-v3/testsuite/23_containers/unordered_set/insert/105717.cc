// { dg-do run { target c++11 } }

// Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <utility>

#include <testsuite_hooks.h>

struct Key
{
  explicit Key(const int* p) : value(p) { }
  ~Key() { value = nullptr; }

  bool operator==(const Key& k) const
  { return *value == *k.value; }

  const int* value;
};

struct hash
{
  std::size_t operator()(const Key& k) const noexcept
  { return *k.value; }
};

struct S
{
  static int _count;

  int value;
  operator Key() const
  {
    ++_count;
    return Key(&value);
  }
};

int S::_count = 0;

void test01()
{
    S a[1] = { {2} };
    std::unordered_set<Key, hash> s;
    std::unordered_multiset<Key, hash> ms;

    s.insert(a, a + 1);
    VERIFY( S::_count == 1 );

    ms.insert(a, a + 1);
    VERIFY( S::_count == 2 );
}

int main()
{
  test01();
  return 0;
}

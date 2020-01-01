// { dg-do run { target c++11 } }

// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <utility>

struct Key
{
  explicit Key(const int* p) : value(p) { }
  ~Key() { value = nullptr; }

  bool operator==(const Key& k) const { return *value == *k.value; }

  const int* value;
};

struct hash
{
  std::size_t operator()(const Key& k) const noexcept { return *k.value; }
};

struct S
{
  int value;
  operator std::pair<const Key, int>() const { return {Key(&value), value}; }
};

int main()
{
    S s[1] = { {2} };
    std::unordered_map<Key, int, hash> m(s, s+1);
    std::unordered_multimap<Key, int, hash> mm(s, s+1);
}

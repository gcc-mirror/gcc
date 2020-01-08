// { dg-do run { target c++11 } }

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#include <set>
#include <stdexcept>

struct Comparator
{
  Comparator() : valid(false) { }
  explicit Comparator(bool) : valid(true) { }

  bool operator()(int i, int j) const
  {
    if (!valid)
      throw std::logic_error("Comparator is invalid");
    return i < j;
  }

private:
  bool valid;
};

int main()
{
  using test_type = std::set<int, Comparator>;

  Comparator cmp{true};

  test_type good{cmp};

  test_type s1;
  s1 = good;             // copy-assign
  s1.insert(1);
  s1.insert(2);

  test_type s2;
  s2 = std::move(good);  // move-assign
  s2.insert(1);
  s2.insert(2);
}

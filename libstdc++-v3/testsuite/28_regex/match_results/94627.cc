// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
// { dg-timeout-factor 2 }

#include <regex>
#include <testsuite_hooks.h>

struct iterator
{
  using value_type = char;
  using difference_type = std::ptrdiff_t;
  using reference = char&;
  using pointer = char*;
  using iterator_category = std::bidirectional_iterator_tag;

  iterator() : ptr() { }
  explicit iterator(pointer p) : ptr(p) { }

  iterator& operator++() { if (bang) throw 1; ++ptr; return *this; }
  iterator operator++(int) { auto copy = *this; ++*this; return copy; }
  iterator& operator--() { if (bang) throw 1; --ptr; return *this; }
  iterator operator--(int) { auto copy = *this; --*this; return copy; }

  reference operator*() const noexcept { return *ptr; }
  pointer operator->() const noexcept { return ptr; }

  bool operator==(iterator rhs) const noexcept { return ptr == rhs.ptr; }
  bool operator!=(iterator rhs) const noexcept { return ptr != rhs.ptr; }

  static bool bang;

private:
  pointer ptr;
};

bool iterator::bang = false;

int main()
{
  char str[] = "abc";
  std::regex r(str);
  std::match_results<iterator> m;
  std::regex_match(iterator(str), iterator(str+3), m, r);
  iterator::bang = true;
  bool caught = false;
  try {
    (void) (m == m);
  } catch (int) {
    caught = true;
  }
  VERIFY( caught );
  caught = false;

  try {
    (void) (m != m);
  } catch (int) {
    caught = true;
  }
  VERIFY( caught );
}

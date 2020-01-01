// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_hooks.h>

bool values[100];

unsigned next_id()
{
  static unsigned counter = 0;
  VERIFY(counter < 100);
  return counter++;
}

struct value
{
  int val;
  const unsigned id;

  value(int i = 0) : val(i), id(next_id()) { values[id] = true; }
  value(const value& v) : val(v.val), id(next_id()) { values[id] = true; }
  value& operator=(const value& v) { val = v.val; return *this; }
  ~value() { values[id] = false; }
};

bool operator<(const value& lhs, const value& rhs)
{
  if (!values[lhs.id])
    throw lhs.id;
  if (!values[rhs.id])
    throw rhs.id;
  return lhs.val < rhs.val;
}

bool operator==(const value& lhs, const value& rhs)
{
  if (!values[lhs.id])
    throw lhs.id;
  if (!values[rhs.id])
    throw rhs.id;
  return lhs.val == rhs.val;
}

// A forward iterator that fails to meet the requirement that for any
// two dereferenceable forward iterators, a == b implies &*a == &*b
struct stashing_iterator
{
  typedef std::forward_iterator_tag iterator_category;
  typedef value value_type;
  typedef value_type const* pointer;
  typedef value_type const& reference;
  typedef std::ptrdiff_t difference_type;

  stashing_iterator() : ptr(), stashed() { }
  stashing_iterator(pointer p) : ptr(p), stashed() { stash(); }
  stashing_iterator(const stashing_iterator&) = default;
  stashing_iterator& operator=(const stashing_iterator&) = default;

  stashing_iterator& operator++()
  {
    ++ptr;
    stash();
    return *this;
  }

  stashing_iterator operator++(int)
  {
    stashing_iterator i = *this;
    ++*this;
    return i;
  }

  reference operator*() const { return stashed; }
  pointer operator->() const { return &**this; }

  bool operator==(const stashing_iterator& i) const { return ptr == i.ptr; }
  bool operator!=(const stashing_iterator& i) const { return !(*this == i); }

private:
  void stash()
  {
    if (ptr)
      stashed = *ptr;
  }

  pointer ptr;
  value_type stashed;
};

void
test01()
{
  value s[] = { 0, 1, 2, 3, 4, 5 };
  std::search(s, s+6, stashing_iterator(s), stashing_iterator(s+4));
}

int
main()
{
  test01();
}

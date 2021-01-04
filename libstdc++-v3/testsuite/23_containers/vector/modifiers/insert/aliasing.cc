// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <vector>
#include <memory>
#include <testsuite_hooks.h>

// See https://gcc.gnu.org/ml/libstdc++/2016-07/msg00008.html for background.

struct T
{
 T(int v = 0) : value(v) { }
 T(const T& t);
 T& operator=(const T& t);
 void make_child() { child = std::make_unique<T>(value + 10); }
 std::unique_ptr<T> child;
 int value;
};

T::T(const T& t) : value(t.value)
{
 if (t.child)
   child.reset(new T(*t.child));
}

T& T::operator=(const T& t)
{
 value = t.value;
 if (t.child)
 {
   if (child)
     *child = *t.child;
   else
     child.reset(new T(*t.child));
 }
 else
   child.reset();
 return *this;
}

void
test01()
{
 std::vector<T> v;
 v.reserve(3);
 v.push_back(T(1));
 v.back().make_child();
 v.push_back(T(2));
 v.back().make_child();

 VERIFY(v[1].child->value == 12);
 VERIFY(v[1].child->child == nullptr);

 v.insert(v.begin(), *v[1].child);

 VERIFY(v[0].value == 12);
 VERIFY(v[0].child == nullptr);
}

int main()
{
  test01();
}

// { dg-do run { target c++11 } }

// Copyright (C) 2010-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <list>
#include <testsuite_hooks.h>

template <class T>
  struct C
  {
    T t_;

    template <class U,
              class = typename std::enable_if
                    <
                        !std::is_lvalue_reference<U>::value
                    >::type>
      C(U&& u) : t_(std::forward<T>(std::move(u).get())) {}
  };

class A
{
  int data_;
public:
  explicit
  A(int data = 1)
  : data_(data) {}
  
  ~A() { data_ = -1; }

  void test() const
  {
    VERIFY( data_ == 3 );
  }
};

class Awrap
{
  const A& a_;
public:
  explicit Awrap(const A& a) : a_(a) { }
  const A& get() const { return a_; }
};

template <class C>
  void test(C c)
  {
    c.t_.test();
  }

// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2009/n2951.html
// Test A.
int main()
{
  std::list<C<const A&> > list;
  A a(3);
  C<const A&> c((Awrap(a)));
  list.push_back(c);
  test(c);
  test(list.front());
}

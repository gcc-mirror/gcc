// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

// { dg-error "static assertion failed" "" { target *-*-* } 87 }

#include <utility>

template <class T>
  struct C
  {
    T t_;

    C() {}

    explicit C(const T& t) : t_(t) { }

    template <class U,
              class = typename std::enable_if
                    <
                        std::is_convertible<U, T>::value
                    >::type>
      C(C<U>&& c) : t_(std::forward<T>(c.t_)) { }
  };

class B;

class A
{
  int data_;

  friend class B;
public:
  explicit
  A(int data = 1)
  : data_(data) { }

  ~A() { data_ = -1; }

  void test() const
  {
    __builtin_abort();
  }
};

class B
{
  int data_;
public:
  explicit
  B(int data = 1)
  : data_(data) { }

  B(const A& a) : data_(a.data_) { }

  B(A&& a) : data_(a.data_) { a.data_ = 100; }

  ~B() { data_ = -1; }

  void test() const
  {
    __builtin_abort();
  }
};

// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2009/n2951.html
// Test F.
int main()
{
  A a(3);
  C<A> ca(a);
  C<const B&> cb(std::move(ca));
  cb.t_.test();
}

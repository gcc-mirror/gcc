// { dg-options "-std=gnu++0x" }

// 2010-04-30  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010 Free Software Foundation, Inc.
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

// Tuple

#include <utility>
#include <tuple>
#include <testsuite_hooks.h>

struct type_zero
{
  type_zero() : n_(757) { }

  type_zero(const type_zero&) = delete;
  type_zero(type_zero&& other) : n_(other.n_) { }

  int get() const { return n_; }

private:
  int n_;
};

struct type_one
{
  type_one(int n) : n_(n) { }

  type_one(const type_one&) = delete;
  type_one(type_one&& other) : n_(other.n_) { }

  int get() const { return n_; }

private:
  int n_;
};

struct type_two
{
  type_two(int n1, int n2) : n1_(n1), n2_(n2) { }

  type_two(const type_two&) = delete;
  type_two(type_two&& other) : n1_(other.n1_), n2_(other.n2_) { }

  int get1() const { return n1_; }
  int get2() const { return n2_; }

private:
  int n1_, n2_;
};

void test01()
{
  bool test __attribute__((unused)) = true;

  std::pair<type_one, type_zero> pp0(std::piecewise_construct_t(),
				     std::forward_as_tuple(-3),
				     std::forward_as_tuple());
  VERIFY( pp0.first.get() == -3 );
  VERIFY( pp0.second.get() == 757 );

  std::pair<type_one, type_two> pp1(std::piecewise_construct_t(),
				    std::forward_as_tuple(6),
				    std::forward_as_tuple(5, 4));
  VERIFY( pp1.first.get() == 6 );
  VERIFY( pp1.second.get1() == 5 );
  VERIFY( pp1.second.get2() == 4 );

  std::pair<type_two, type_two> pp2(std::piecewise_construct_t(),
				    std::forward_as_tuple(2, 1),
				    std::forward_as_tuple(-1, -3));
  VERIFY( pp2.first.get1() == 2 );
  VERIFY( pp2.first.get2() == 1 );
  VERIFY( pp2.second.get1() == -1 );
  VERIFY( pp2.second.get2() == -3 );
}

int main()
{
  test01();
  return 0;
}

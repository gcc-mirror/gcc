// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2011 Free Software Foundation, Inc.
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

// 20.4.2.1 [tuple.cnstr] Allocator-extended constructors

#include <memory>
#include <tuple>

struct MyAlloc { };

struct Tag0 { };
struct Tag1 { };
struct Tag2 { };

// A non-copyable and non-movable type
struct Type
{
  typedef MyAlloc allocator_type;

  explicit Type(Tag0) { }
  Type(std::allocator_arg_t, MyAlloc, Tag1) { }
  Type(Tag2, MyAlloc) { }

  Type(const Type&) = delete;
  Type(Type&&) = delete;
  Type& operator=(const Type&) = delete;
  Type& operator=(Type&&) = delete;
};

void test01()
{
  using std::allocator_arg;
  using std::tuple;

  MyAlloc a;
  Tag0 tag0;
  Tag1 tag1;
  Tag2 tag2;

  // N.B. cannot use Tag0 with uses-allocator construction, because
  // uses_allocator<Type, MyAlloc> is true but no suitable cosntructor
  tuple<Type>		  t1(tag0);

  tuple<Type> 		  t2(allocator_arg, a, tag1);
  tuple<Type> 		  t3(allocator_arg, a, tag2);

  tuple<Type, Type> 	  t4(allocator_arg, a, tag1, tag2);

  tuple<Type, Type, Type> t5(allocator_arg, a, tag2, tag1, tag2);
}

int main()
{
  test01();
  return 0;
}

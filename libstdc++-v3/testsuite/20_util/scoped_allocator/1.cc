// { dg-do run { target c++11 } }

// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

#include <memory>
#include <scoped_allocator>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using __gnu_test::uneq_allocator;

struct Element
{
  typedef uneq_allocator<Element> allocator_type;

  allocator_type alloc;

  Element(const allocator_type& a = allocator_type()) : alloc(a) { }

  Element(std::allocator_arg_t, const allocator_type& a, int = 0)
  : alloc(a) { }

  Element(std::allocator_arg_t, const allocator_type& a, const Element&)
  : alloc(a) { }

  const allocator_type& get_allocator() const { return alloc; }
};

void test01()
{
  typedef std::scoped_allocator_adaptor<Element::allocator_type> alloc1_type;

  typedef std::vector<Element, alloc1_type> EltVec;

  alloc1_type a1(1);
  Element e;
  EltVec ev1(1, e, a1);
  VERIFY( ev1.get_allocator().get_personality() == 1 );
  VERIFY( ev1[0].get_allocator().get_personality() == 1 );
}

void test02()
{
  typedef std::scoped_allocator_adaptor<Element::allocator_type> alloc1_type;

  typedef std::vector<Element, alloc1_type> EltVec;

  typedef std::scoped_allocator_adaptor<Element::allocator_type,
                                        Element::allocator_type> alloc2_type;

  typedef std::allocator_traits<alloc2_type>::rebind_alloc<EltVec> alloc_type;

  typedef std::vector<EltVec, alloc_type> EltVecVec;

  alloc_type a(1, Element::allocator_type(2)); // outer=1, inner=2
  Element e;
  EltVec ev(1, e);
  EltVecVec evv(1, ev, a);

  VERIFY( evv.get_allocator().get_personality() == 1 );
  VERIFY( evv[0].get_allocator().get_personality() == 2 );
  VERIFY( evv[0][0].get_allocator().get_personality() == 2 );

  alloc_type a2(3, Element::allocator_type(4)); // outer=3, inner=4

  EltVecVec evv2(evv, a2); // copy with a different allocator

  VERIFY( evv2.get_allocator().get_personality() == 3 );
  VERIFY( evv2[0].get_allocator().get_personality() == 4 );
  VERIFY( evv2[0][0].get_allocator().get_personality() == 4 );

  EltVecVec evv3(std::move(evv), a2); // move with a different allocator

  VERIFY( evv3.get_allocator().get_personality() == 3 );
  VERIFY( evv3[0].get_allocator().get_personality() == 4 );
  VERIFY( evv3[0][0].get_allocator().get_personality() == 4 );
}

int main()
{
  test01();
  test02();
}

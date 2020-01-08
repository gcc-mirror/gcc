// { dg-do run { target c++11 } }
//
// Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

#include <tr2/type_traits>
#include <typeinfo>
#include <stdexcept>

struct A { };
struct B1 : virtual public A { };
struct B2 : virtual public A { };
struct C : public B1, public B2 { };

void test()
{
  // 1
  {
    typedef std::tr2::bases<A>::type tl;
    static_assert(tl::empty::value, "error");
  }

  // 2
  {
    typedef std::tr2::bases<B1>::type tl1;
    typedef std::tr2::bases<B2>::type tl2;

    // Sanity check w/ runtime.
    bool eq = typeid(tl1) == typeid(tl2);
    if (!eq)
      throw std::logic_error("typelist not equal");

    // Sanity check.
    static_assert(tl1::empty::value != std::true_type::value, "!empty");
    static_assert(tl2::empty::value != std::true_type::value, "!empty");

    typedef tl1::first::type		tl1_first;
    typedef tl1::rest::type		tl1_rest;
    typedef tl2::first::type		tl2_first;
    typedef tl2::rest::type		tl2_rest;

    eq = typeid(tl1_first) == typeid(tl2_first);
    if (!eq)
      throw std::logic_error("base not equal");

    static_assert(tl1_rest::empty::value == std::true_type::value, "empty");
    static_assert(tl2_rest::empty::value == std::true_type::value, "empty");
  }

  // 3
  {
    typedef std::tr2::bases<C>::type tl;

    // Sanity check.
    static_assert(tl::empty::value != std::true_type::value, "!empty");
  
    typedef tl::first::type		tl1_first;
    typedef tl::rest::type		tl2;
    typedef tl2::first::type		tl2_first;
    typedef tl2::rest::type		tl3;
    typedef tl3::first::type		tl3_first;
    typedef tl3::rest::type		tl4;

    bool eq = typeid(tl1_first) == typeid(tl2_first);
    if (eq)
      throw std::logic_error("bases are not equal");

    eq = typeid(tl2_first) == typeid(tl3_first);
    if (eq)
      throw std::logic_error("bases are not equal");

    static_assert(tl4::empty::value == std::true_type::value, "empty");
  }

}

int main()
{
  test();
  return 0;
}

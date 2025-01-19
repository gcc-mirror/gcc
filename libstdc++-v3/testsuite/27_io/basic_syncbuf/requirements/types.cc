// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <syncstream>

template<typename T>
  struct type_reqs
  {
    using test_type = T;
    using char_type = test_type::char_type;
    using int_type = test_type::int_type;
    using pos_type = test_type::pos_type;
    using off_Type = test_type::off_type;
    using traits_type = test_type::traits_type;
    using allocator_type = test_type::allocator_type;
    using streambuf_type = test_type::streambuf_type;
  };

void test01()
{
  // Check for required typedefs
  using test_type = type_reqs<std::osyncstream>;
  using wtest_type = type_reqs<std::wosyncstream>;
}

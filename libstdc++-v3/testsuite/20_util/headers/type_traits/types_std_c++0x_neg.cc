// { dg-do compile { target c++11 } }

// Copyright (C) 2007-2020 Free Software Foundation, Inc.
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

#include <type_traits>

namespace gnu
{
  // C++11 changes from TR1.
  using std::has_trivial_constructor; // { dg-error "has not been declared" }
  using std::has_trivial_default_constructor; // { dg-error "has not been declared" }
  using std::has_nothrow_constructor; // { dg-error "has not been declared" }
  using std::has_trivial_copy;	      // { dg-error "has not been declared" }
  using std::has_trivial_copy_constructor; // { dg-error "has not been declared" }
  using std::has_trivial_copy_assign; // { dg-error "has not been declared" }
  using std::has_nothrow_copy;	      // { dg-error "has not been declared" }
}

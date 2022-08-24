// Copyright (C) 2022 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <version>

#ifndef __cpp_lib_reference_from_temporary
# error "Feature test macro for reference_from_temporary is missing in <version>"
#elif __cpp_lib_reference_from_temporary < 202202L
# error "Feature test macro for reference_from_temporary has wrong value in <version>"
#endif

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do preprocess { target c++20 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_integer_comparison_functions
# error "Feature test macro for comparison functions is missing in <version>"
#elif __cpp_lib_integer_comparison_functions < 202002L
# error "Feature test macro for comparison functions has wrong value in <version>"
#endif

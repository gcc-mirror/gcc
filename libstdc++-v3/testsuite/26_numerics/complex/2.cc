// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_constexpr_complex
# error "Feature test macro for constexpr complex is missing in <version>"
#elif __cpp_lib_constexpr_complex < 201711L
# error "Feature test macro for constexpr complex has wrong value in <version>"
#endif

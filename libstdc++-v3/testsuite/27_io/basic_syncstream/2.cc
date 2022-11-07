// Copyright (C) 2020-2022 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
// { dg-require-effective-target cxx11_abi }
// { dg-require-effective-target hosted }

#include <version>

#ifndef __cpp_lib_syncbuf
# error "Feature-test macro for syncbuf missing in <version>"
#elif __cpp_lib_syncbuf != 201803L
# error "Feature-test macro for syncbuf has wrong value in <version>"
#endif

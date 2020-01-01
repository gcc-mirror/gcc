// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-options "-O1" }
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "__throw_length_error" } }
// { dg-final { scan-assembler-not "__throw_bad_alloc" } }

#include <bits/c++config.h>
#undef _GLIBCXX_EXTERN_TEMPLATE
#include <string>

void
test01(std::string& target, std::string&& source)
{
  // The move assignment operator should be simple enough that the compiler
  // can see that it never results in a length_error or bad_alloc exception
  // (which would be turned into std::terminate by the noexcept on the
  // assignment operator). This is only true when inlining though.
#ifndef __NO_INLINE__
  target = std::move(source);
#endif
}

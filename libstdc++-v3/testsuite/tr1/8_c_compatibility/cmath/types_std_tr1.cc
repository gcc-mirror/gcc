// { dg-do compile }

// 2006-03-10  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2019 Free Software Foundation, Inc.
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

// 8.16 Additions to header <cmath>

#include <tr1/cmath>

void test01()
{
#if _GLIBCXX_USE_C99_MATH_TR1

  typedef std::tr1::double_t  my_double_t;
  typedef std::tr1::float_t   my_float_t;

#endif
}

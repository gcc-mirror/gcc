// { dg-do compile }
// { dg-require-parallel-mode "" }
// { dg-options "-fopenmp" { target *-*-* } }

// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_ASSERTIONS
# define _GLIBCXX_ASSERTIONS 1
#endif
#include <parallel/algorithm>
#ifndef _GLIBCXX_PARALLEL_ASSERTIONS
# error "_GLIBCXX_ASSERTIONS does not enable _GLIBCXX_PARALLEL_ASSERTIONS"
#endif

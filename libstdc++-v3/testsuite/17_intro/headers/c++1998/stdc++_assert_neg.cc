// { dg-do compile }
// { dg-options "-D_GLIBCXX_NO_ASSERT" }
// NB: This is done to force any generated and possibly included PCH
// to be invalid, and also to remove cassert from the include set.

// 2005-05-24 bkoz

// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

// 17.4.1.2 Headers

// This file tests that assert is not included in any of the standard
// includes by accident.

#include <bits/stdc++.h>

void foo()
{
 assert(true);  // { dg-error "not declared" "" { target *-*-* } } 
}

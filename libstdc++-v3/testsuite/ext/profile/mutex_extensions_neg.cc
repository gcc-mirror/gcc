// { dg-options "-D_GLIBCXX_DEBUG -D_GLIBCXX_PROFILE" }
// { dg-do compile }

// -*- C++ -*-

// Otherwise we may get *multiple* errors.
#undef _GLIBCXX_PARALLEL

// Copyright (C) 2006, 2007, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#include <vector>

// { dg-error "multiple inlined namespaces" "" { target *-*-* } 262 }

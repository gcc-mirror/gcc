// { dg-options "-std=gnu++1y" }
// { dg-do compile }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

#include <type_traits>

using namespace std;

template<typename Trait, typename Result>
  using test = is_same<typename Trait::type, Result>;

static_assert( test<make_signed<const int>, make_signed_t<const int>>(),
               "make_signed_t<const int>" );

static_assert( test<make_signed<unsigned>, make_signed_t<unsigned>>(),
               "make_signed_t<unsigned>" );

static_assert( test<make_signed<char>, make_signed_t<char>>(),
               "make_signed_t<char>" );

// { dg-do compile { target c++14 } }

// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

static_assert( test<decay<bool>, decay_t<bool>>(), "decay<bool>" );
static_assert( test<decay<const int>, decay_t<const int>>(),
               "decay<const int>" );
static_assert( test<decay<int[4]>, decay_t<int[4]>>(), "decay<int[4]>" );
typedef void (fn_type) ();
static_assert( test<decay<fn_type>, decay_t<fn_type>>(), "decay<fn_type>" );
typedef void (cfn_type) () const;
static_assert( test<decay<cfn_type>, decay_t<cfn_type>>(), "decay<cfn_type>" );

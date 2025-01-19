// { dg-do compile }

// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

#include <clocale>

namespace gnu
{
#ifndef NULL
    #error "NULL_must_be_a_macro"
#endif

#ifndef LC_ALL
    #error "LC_ALL_must_be_a_macro"
#endif

#ifndef LC_COLLATE
    #error "LC_COLLATE_must_be_a_macro"
#endif

#ifndef LC_CTYPE
    #error "LC_CTYPE_must_be_a_macro"
#endif

#ifndef LC_MONETARY
    #error "LC_MONETARY_must_be_a_macro"
#endif

#ifndef LC_NUMERIC
    #error "LC_NUMERIC_must_be_a_macro"
#endif

#ifndef LC_TIME
    #error "LC_TIME_must_be_a_macro"
#endif
}

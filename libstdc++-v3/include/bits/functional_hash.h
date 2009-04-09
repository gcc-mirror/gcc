// functional_hash.h header -*- C++ -*-

// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/functional_hash.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _FUNCTIONAL_HASH_H
#define _FUNCTIONAL_HASH_H 1

#pragma GCC system_header

#ifndef __GXX_EXPERIMENTAL_CXX0X__
# include <c++0x_warning.h>
#endif

#if defined(_GLIBCXX_INCLUDE_AS_TR1)
#  error C++0x header cannot be included from TR1 header
#endif

#if defined(_GLIBCXX_INCLUDE_AS_CXX0X)
#  include <tr1_impl/functional_hash.h>
#else
#  define _GLIBCXX_INCLUDE_AS_CXX0X
#  define _GLIBCXX_BEGIN_NAMESPACE_TR1
#  define _GLIBCXX_END_NAMESPACE_TR1
#  define _GLIBCXX_TR1
#  include <tr1_impl/functional_hash.h>
#  undef _GLIBCXX_TR1
#  undef _GLIBCXX_END_NAMESPACE_TR1
#  undef _GLIBCXX_BEGIN_NAMESPACE_TR1
#  undef _GLIBCXX_INCLUDE_AS_CXX0X
#endif


namespace std
{
  struct error_code;

  template<>
    size_t
    hash<error_code>::operator()(error_code) const;
}

#endif // _FUNCTIONAL_HASH_H


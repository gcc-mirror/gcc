//  std::hash and std::tr1::hash definitions -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cstddef>
#include <string>

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <functional>
#  define _GLIBCXX_BEGIN_NAMESPACE_TR1 
#  define _GLIBCXX_END_NAMESPACE_TR1 
#else
#include <tr1/functional>
#  define _GLIBCXX_INCLUDE_AS_TR1
#  define _GLIBCXX_BEGIN_NAMESPACE_TR1 namespace tr1 {
#  define _GLIBCXX_END_NAMESPACE_TR1 }
#endif

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_TR1

  template<>
    size_t
    hash<string>::operator()(string __s) const
    { return _Fnv_hash<>::hash(__s.data(), __s.length()); }

  template<>
    size_t
    hash<const string&>::operator()(const string& __s) const
    { return _Fnv_hash<>::hash(__s.data(), __s.length()); }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    size_t
    hash<wstring>::operator()(wstring __s) const
    {
      const char* __p = reinterpret_cast<const char*>(__s.data());
      return _Fnv_hash<>::hash(__p, __s.length() * sizeof(wchar_t));
    }

  template<>
    size_t
    hash<const wstring&>::operator()(const wstring& __s) const
    {
      const char* __p = reinterpret_cast<const char*>(__s.data());
      return _Fnv_hash<>::hash(__p, __s.length() * sizeof(wchar_t));
    }
#endif

_GLIBCXX_END_NAMESPACE_TR1
}

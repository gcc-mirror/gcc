//  std::hash and std::tr1::hash definitions, string bits -*- C++ -*-

// Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_LONG_DOUBLE_COMPAT_IMPL
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

#endif

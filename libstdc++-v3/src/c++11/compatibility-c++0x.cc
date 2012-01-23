// Compatibility symbols for previous versions, C++0x bits -*- C++ -*-

// Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
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

#define _GLIBCXX_COMPATIBILITY_CXX0X
#include <string>
#include <system_error>

#ifndef __GXX_EXPERIMENTAL_CXX0X__
# error "compatibility-c++0x.cc must be compiled with -std=gnu++0x"
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
  // gcc-4.4.0
  // <mutex> exported std::lock_error
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
  class lock_error : public exception
  {
  public:
    virtual const char*
    _GLIBCXX_CONST what() const throw();
  };

  const char*
  lock_error::what() const throw()
  { return "std::lock_error"; }
#endif

  // We need these due to the symbols exported since GLIBCXX_3.4.10.
  // See libstdc++/41662 for details.

#ifndef _GLIBCXX_LONG_DOUBLE_COMPAT_IMPL
  template<>
    struct hash<string>
    {
      size_t operator()(string) const;
    };

  size_t
  hash<string>::operator()(string __s) const
  { return _Hash_impl::hash(__s.data(), __s.length()); }

  template<>
    struct hash<const string&>
    {
      size_t operator()(const string&) const;
    };

  size_t
  hash<const string&>::operator()(const string& __s) const
  { return _Hash_impl::hash(__s.data(), __s.length()); }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    struct hash<wstring>
    { 
      size_t operator()(wstring) const;
    };

  size_t
  hash<wstring>::operator()(wstring __s) const
  { return _Hash_impl::hash(__s.data(), __s.length() * sizeof(wchar_t)); }

  template<>
    struct hash<const wstring&>
    {
      size_t operator()(const wstring&) const;
    };

  size_t
  hash<const wstring&>::operator()(const wstring& __s) const
  { return _Hash_impl::hash(__s.data(), __s.length() * sizeof(wchar_t)); }
#endif
#endif

  template<>
    struct hash<error_code>
    {
      size_t operator()(error_code) const;
    };

  size_t
  hash<error_code>::operator()(error_code __e) const
  {
    const size_t __tmp = std::_Hash_impl::hash(__e._M_value);
    return std::_Hash_impl::__hash_combine(__e._M_cat, __tmp);
  }

  // gcc-4.7.0
  // <chrono> changes is_monotonic to is_steady.
  namespace chrono
  {
    struct system_clock
    {
      static constexpr bool is_monotonic = false;
    };
    constexpr bool system_clock::is_monotonic;
  } // namespace chrono
}


// Helper for accessing __cxx11::string from the ABI -*- C++ -*-

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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


#define _GLIBCXX_USE_CXX11_ABI 1
#define __sso_string __sso_stringxxx
#include <string>
#include <stdexcept>
#undef __sso_string

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wabi-tag"
  // Redefine __sso_string so that we can define and export its members
  // in terms of the SSO std::string.
  struct __sso_string
  {
    struct __str
    {
      const char* _M_p;
      size_t _M_string_length;
      char _M_local_buf[16];
    };

    union {
      __str _M_s;
      char _M_bytes[sizeof(_M_s)];
      std::string _M_str;
    };

    __sso_string();
    __sso_string(const std::string& s);
    __sso_string(const char*, size_t n);
    __sso_string(const __sso_string&) noexcept;
    __sso_string& operator=(const __sso_string&) noexcept;
    ~__sso_string();
    __sso_string(__sso_string&&) noexcept;
    __sso_string& operator=(__sso_string&&) noexcept;
  };
#pragma GCC diagnostic pop

  __sso_string::__sso_string() : _M_str() { }

#if _GLIBCXX_USE_CXX11_ABI
  static_assert(sizeof(__sso_string) == sizeof(std::string),
                "sizeof(std::string) has changed");
  static_assert(alignof(__sso_string) == alignof(std::string),
                "alignof(std::string) has changed");

  // This constructor is defined in src/c++11/cow-stdexcept.cc for COW strings
  __sso_string::__sso_string(const std::string& s) : _M_str(s) { }
#endif

  __sso_string::__sso_string(const char* s, size_t n) : _M_str(s, n) { }

  __sso_string::__sso_string(const __sso_string& s) noexcept
  : _M_str(s._M_str) { }

  __sso_string&
  __sso_string::operator=(const __sso_string& s) noexcept
  {
    _M_str = s._M_str;
    return *this;
  }

  __sso_string::~__sso_string() { _M_str.~basic_string(); }

  __sso_string::__sso_string(__sso_string&& s) noexcept
  : _M_str(std::move(s._M_str)) { }

  __sso_string&
  __sso_string::operator=(__sso_string&& s) noexcept
  {
    _M_str = std::move(s._M_str);
    return *this;
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

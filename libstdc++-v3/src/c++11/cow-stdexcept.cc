// Methods for Exception Support for -*- C++ -*-

// Copyright (C) 2014-2015 Free Software Foundation, Inc.
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

//
// ISO C++ 14882: 19.1  Exception classes
//

// All exception classes still use the classic COW std::string.
#define _GLIBCXX_USE_CXX11_ABI 0
#define _GLIBCXX_DEFINE_STDEXCEPT_COPY_OPS 1
#define __cow_string __cow_stringxxx
#include <stdexcept>
#include <system_error>
#undef __cow_string

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Copy constructors and assignment operators defined using COW std::string

  logic_error::logic_error(const logic_error& e) noexcept
  : _M_msg(e._M_msg) { }

  logic_error& logic_error::operator=(const logic_error& e) noexcept
  { _M_msg = e._M_msg; return *this; }

  runtime_error::runtime_error(const runtime_error& e) noexcept
  : _M_msg(e._M_msg) { }

  runtime_error&
  runtime_error::operator=(const runtime_error& e) noexcept
  { _M_msg = e._M_msg; return *this; }

  // New C++11 constructors:

  logic_error::logic_error(const char* __arg)
  : exception(), _M_msg(__arg) { }

  domain_error::domain_error(const char* __arg)
  : logic_error(__arg) { }

  invalid_argument::invalid_argument(const char* __arg)
  : logic_error(__arg) { }

  length_error::length_error(const char* __arg)
  : logic_error(__arg) { }

  out_of_range::out_of_range(const char* __arg)
  : logic_error(__arg) { }

  runtime_error::runtime_error(const char* __arg)
  : exception(), _M_msg(__arg) { }

  range_error::range_error(const char* __arg)
  : runtime_error(__arg) { }

  overflow_error::overflow_error(const char* __arg)
  : runtime_error(__arg) { }

  underflow_error::underflow_error(const char* __arg)
  : runtime_error(__arg) { }

#if _GLIBCXX_USE_DUAL_ABI
  // Converting constructor from COW std::string to SSO string.
  __sso_string::__sso_string(const string& s)
  : __sso_string(s.c_str(), s.length()) { }

  // Redefine __cow_string so that we can define and export its members
  // in terms of the COW std::string.
  struct __cow_string
  {
    union {
      const char* _M_p;
      char _M_bytes[sizeof(_M_p)];
      std::string _M_str;
    };

    __cow_string();
    __cow_string(const std::string& s);
    __cow_string(const char*, size_t n);
    __cow_string(const __cow_string&) noexcept;
    __cow_string& operator=(const __cow_string&) noexcept;
    ~__cow_string();
    __cow_string(__cow_string&&) noexcept;
    __cow_string& operator=(__cow_string&&) noexcept;
  };

  __cow_string::__cow_string() : _M_str() { }

  __cow_string::__cow_string(const std::string& s) : _M_str(s) { }

  __cow_string::__cow_string(const char* s, size_t n) : _M_str(s, n) { }

  __cow_string::__cow_string(const __cow_string& s) noexcept
  : _M_str(s._M_str) { }

  __cow_string&
  __cow_string::operator=(const __cow_string& s) noexcept
  {
    _M_str = s._M_str;
    return *this;
  }

  __cow_string::~__cow_string() { _M_str.~basic_string(); }

  __cow_string::__cow_string(__cow_string&& s) noexcept
  : _M_str(std::move(s._M_str)) { }

  __cow_string&
  __cow_string::operator=(__cow_string&& s) noexcept
  {
    _M_str = std::move(s._M_str);
    return *this;
  }

  static_assert(sizeof(__cow_string) == sizeof(std::string),
                "sizeof(std::string) has changed");
  static_assert(alignof(__cow_string) == alignof(std::string),
                "alignof(std::string) has changed");
#endif

  // Return error_category::message() as an SSO string
  __sso_string
  error_category::_M_message(int i) const
  {
    string msg = this->message(i);
    return {msg.c_str(), msg.length()};
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// <system_error> implementation file

// Copyright (C) 2007-2016 Free Software Foundation, Inc.
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
#include <cstring>
#include <system_error>
#include <bits/functexcept.h>
#include <limits>
#undef __sso_string

namespace
{
  using std::string; 
  
  struct generic_error_category : public std::error_category
  {
    virtual const char*
    name() const noexcept
    { return "generic"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    virtual string 
    message(int i) const
    {
      // XXX locale issues: how does one get or set loc.
      // _GLIBCXX_HAVE_STRERROR_L, strerror_l(i, cloc)
      return string(strerror(i));
    }
  };

  struct system_error_category : public std::error_category
  {
    virtual const char*
    name() const noexcept
    { return "system"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    virtual string
    message(int i) const
    {
      // XXX locale issues: how does one get or set loc.
      // _GLIBCXX_HAVE_STRERROR_L, strerror_l(i, cloc)
      return string(strerror(i));
    }
  };

  const generic_error_category generic_category_instance{};
  const system_error_category system_category_instance{};
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  __throw_system_error(int __i __attribute__((unused)))
  {
    _GLIBCXX_THROW_OR_ABORT(system_error(error_code(__i, generic_category())));
  }

  error_category::~error_category() noexcept = default;

  const error_category& 
  _V2::system_category() noexcept { return system_category_instance; }

  const error_category& 
  _V2::generic_category() noexcept { return generic_category_instance; }
  
  system_error::~system_error() noexcept = default;

  error_condition 
  error_category::default_error_condition(int __i) const noexcept
  { return error_condition(__i, *this); }

  bool 
  error_category::equivalent(int __i,
			     const error_condition& __cond) const noexcept
  { return default_error_condition(__i) == __cond; }

  bool 
  error_category::equivalent(const error_code& __code, int __i) const noexcept
  { return *this == __code.category() && __code.value() == __i; }

  error_condition 
  error_code::default_error_condition() const noexcept
  { return category().default_error_condition(value()); }

#if _GLIBCXX_USE_CXX11_ABI
  // Return error_category::message() as a COW string
  __cow_string
  error_category::_M_message(int i) const
  {
    string msg = this->message(i);
    return {msg.c_str(), msg.length()};
  }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

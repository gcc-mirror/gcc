// <system_error> implementation file

// Copyright (C) 2007, 2008
// Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cstring>
#include <system_error>
#include <bits/functexcept.h>
#include <limits>

namespace
{
  using std::string; 
  
  struct generic_error_category : public std::error_category
  {
    virtual const char*
    name() const 
    { return "generic"; }

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
    name() const
    { return "system"; }

    virtual string
    message(int i) const
    {
      // XXX locale issues: how does one get or set loc.
      // _GLIBCXX_HAVE_STRERROR_L, strerror_l(i, cloc)
      return string(strerror(i));
    }
  };

  const generic_error_category generic_category_instance;
  const system_error_category system_category_instance;
}

_GLIBCXX_BEGIN_NAMESPACE(std)

  const error_category& system_category = system_category_instance;
  const error_category& generic_category = generic_category_instance;
  
  system_error::~system_error() throw() { }

  error_condition 
  error_category::default_error_condition(int __i) const
  { return error_condition(__i, *this); }

  bool 
  error_category::equivalent(int __i, const error_condition& __cond) const
  { return default_error_condition(__i) == __cond; }

  bool 
  error_category::equivalent(const error_code& __code, int __i) const
  { return *this == __code.category() && __code.value() == __i; }

  error_condition 
  error_code::default_error_condition() const
  { return category().default_error_condition(value()); }

_GLIBCXX_END_NAMESPACE

// Methods for Exception Support for -*- C++ -*-

// Copyright (C) 1997, 1999, 2001, 2002, 2005, 2009, 2011
// Free Software Foundation, Inc.
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

#include <string>
#include <stdexcept>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  logic_error::logic_error(const string& __arg)
  : exception(), _M_msg(__arg) { }

  logic_error::~logic_error() throw() { }

  const char*
  logic_error::what() const throw()
  { return _M_msg.c_str(); }

  domain_error::domain_error(const string& __arg)
  : logic_error(__arg) { }

  domain_error::~domain_error() throw() { }

  invalid_argument::invalid_argument(const string& __arg)
  : logic_error(__arg) { }

  invalid_argument::~invalid_argument() throw() { }

  length_error::length_error(const string& __arg)
  : logic_error(__arg) { }

  length_error::~length_error() throw() { }

  out_of_range::out_of_range(const string& __arg)
  : logic_error(__arg) { }

  out_of_range::~out_of_range() throw() { }

  runtime_error::runtime_error(const string& __arg)
  : exception(), _M_msg(__arg) { }

  runtime_error::~runtime_error() throw() { }

  const char*
  runtime_error::what() const throw()
  { return _M_msg.c_str(); }

  range_error::range_error(const string& __arg)
  : runtime_error(__arg) { }

  range_error::~range_error() throw() { }

  overflow_error::overflow_error(const string& __arg)
  : runtime_error(__arg) { }

  overflow_error::~overflow_error() throw() { }

  underflow_error::underflow_error(const string& __arg)
  : runtime_error(__arg) { }

  underflow_error::~underflow_error() throw() { }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// regex -*- C++ -*-

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

#include <stdexcept>
#include <bits/regex_error.h>
#include <bits/functexcept.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  __throw_regex_error(regex_constants::error_type __ecode
		      __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(regex_error(__ecode)); }

  regex_error::regex_error(regex_constants::error_type __ecode)
  : std::runtime_error("regex_error"), _M_code(__ecode)
  { }

  regex_error::~regex_error() throw() { }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

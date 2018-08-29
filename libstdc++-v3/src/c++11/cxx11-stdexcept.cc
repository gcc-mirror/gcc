// Methods for Exception Support for -*- C++ -*-

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#define _GLIBCXX_USE_CXX11_ABI 1
#include <stdexcept>

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // These constructors take an abi-tagged std::string and use it to
  // initialize an untagged COW std::string in _M_msg.

  logic_error::logic_error(const string& __arg)
  : _M_msg(__arg) { }

  runtime_error::runtime_error(const string& __arg)
  : _M_msg(__arg) { }

  // These constructors take an abi-tagged std::string and pass it to the
  // base class constructors defined above.

  domain_error::domain_error(const string& __arg)
  : logic_error(__arg) { }

  invalid_argument::invalid_argument(const string& __arg)
  : logic_error(__arg) { }

  length_error::length_error(const string& __arg)
  : logic_error(__arg) { }

  out_of_range::out_of_range(const string& __arg)
  : logic_error(__arg) { }

  range_error::range_error(const string& __arg)
  : runtime_error(__arg) { }

  overflow_error::overflow_error(const string& __arg)
  : runtime_error(__arg) { }

  underflow_error::underflow_error(const string& __arg)
  : runtime_error(__arg) { }

  // Converting constructor from ABI-tagged std::string to COW string.
  __cow_string::__cow_string(const string& s)
  : __cow_string(s.c_str(), s.length()) { }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

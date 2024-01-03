// std::iostream_category() definition -*- C++ -*-

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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
// ISO C++ 14882:2011: 27.5.6.5  Error reporting [error.reporting]
//

#define _GLIBCXX_USE_CXX11_ABI 1
#include <ios>

#if __has_cpp_attribute(clang::require_constant_initialization)
#  define __constinit [[clang::require_constant_initialization]]
#endif

namespace
{
  struct io_error_category final : std::error_category
  {
    const char*
    name() const noexcept final
    { return "iostream"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    std::string
    message(int __ec) const final
    {
      std::string __msg;
      switch (std::io_errc(__ec))
      {
      case std::io_errc::stream:
          __msg = "iostream error";
          break;
      default:
          __msg = "Unknown error";
          break;
      }
      return __msg;
    }
  };

  struct constant_init
  {
    union {
      unsigned char unused;
      io_error_category cat;
    };
    constexpr constant_init() : cat() { }
    ~constant_init() { /* do nothing, union member is not destroyed */ }
  };

  __constinit constant_init io_category_instance{};
} // namespace

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  const error_category&
  iostream_category() noexcept
  { return io_category_instance.cat; }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

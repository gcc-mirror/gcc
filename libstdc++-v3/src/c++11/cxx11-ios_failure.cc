// Iostreams base classes -*- C++ -*-

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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
// ISO C++ 14882:2011: 27.5.3.1.1  Class ios_base::failure
//

#define _GLIBCXX_USE_CXX11_ABI 1
#include <ios>
#include <typeinfo>
#include <cxxabi.h>

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

namespace
{
  struct io_error_category : std::error_category
  {
    virtual const char*
    name() const noexcept
    { return "iostream"; }

    _GLIBCXX_DEFAULT_ABI_TAG
    virtual std::string message(int __ec) const
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

  const io_error_category&
  __io_category_instance() noexcept
  {
    static const io_error_category __ec{};
    return __ec;
  }

} // namespace

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  const error_category&
  iostream_category() noexcept
  { return __io_category_instance(); }

  ios_base::failure::failure(const string& __str)
  : system_error(io_errc::stream, __str) { }

  ios_base::failure::failure(const string& __str, const error_code& __ec)
  : system_error(__ec, __str) { }

  ios_base::failure::failure(const char* __str, const error_code& __ec)
  : system_error(__ec, __str) { }

  ios_base::failure::~failure()
  { }

  const char*
  ios_base::failure::what() const throw()
  { return runtime_error::what(); }

  // __throw_ios_failure() is defined in src/c++98/ios_failure.cc

#if __cpp_rtti
  // If RTTI is enabled the exception type thrown will use these functions to
  // construct/destroy a ios::failure[abi:cxx11] object in a buffer,
  // and to catch that object via a handler of the [abi:cxx11] type.
  void
  __construct_ios_failure(void* buf, const char* msg)
  { ::new(buf) ios_base::failure(msg); }

  void
  __destroy_ios_failure(void* buf)
  { static_cast<ios_base::failure*>(buf)->~failure(); }

  bool
  __is_ios_failure_handler(const __cxxabiv1::__class_type_info* type)
  { return *type == typeid(ios::failure); }

  // static assertions to ensure ios::failure fits in a buffer
  // with the same size and alignment as system_error:
  static_assert(sizeof(ios::failure) <= sizeof(system_error), "");
  static_assert(__alignof(ios::failure) <= __alignof(system_error), "");
#endif // __cpp_rtti

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

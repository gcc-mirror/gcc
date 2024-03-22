// Iostreams base classes -*- C++ -*-

// Copyright (C) 1997-2024 Free Software Foundation, Inc.
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
// ISO C++ 14882:1998: 27.4.2.1.1  Class ios_base::failure
//

#define _GLIBCXX_USE_CXX11_ABI 0
#include <ios>

#if _GLIBCXX_USE_DUAL_ABI && __cpp_rtti
#include <cxxabi.h>
#include <typeinfo>
#endif

#ifdef _GLIBCXX_USE_NLS
# include <libintl.h>
# define _(msgid)   gettext (msgid)
#else
# define _(msgid)   (msgid)
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  ios_base::failure::failure(const string& __str) throw()
  : _M_msg(__str) { }

  ios_base::failure::~failure() throw()
  { }

  const char*
  ios_base::failure::what() const throw()
  { return _M_msg.c_str(); }

#if _GLIBCXX_USE_DUAL_ABI
  // When the dual ABI is enabled __throw_ios_failure() is defined in
  // src/c++11/cxx11-ios_failure.cc
#if __cpp_rtti
  // If RTTI is enabled the exception type thrown will use these functions to
  // construct/destroy a gcc4-compatible ios::failure object in a buffer,
  // and to catch that object via a handler of the gcc4-compatible type.
  void
  __construct_ios_failure(void* buf, const char* msg)
  { ::new(buf) ios_base::failure(msg); }

  void
  __destroy_ios_failure(void* buf)
  { static_cast<ios_base::failure*>(buf)->~failure(); }

  bool
  __is_ios_failure_handler(const __cxxabiv1::__class_type_info* type)
  { return *type == typeid(ios::failure); }

  namespace {
  // C++98-style static assertions to ensure ios::failure fits in a buffer
  // with the same size and alignment as runtime_error:
  typedef char S[1 / (sizeof(ios::failure) <= sizeof(runtime_error))];
  typedef char A[1 / (__alignof(ios::failure) <= __alignof(runtime_error))];
  }
#endif // __cpp_rtti

#else // ! _GLIBCXX_USE_DUAL_ABI

  void
  __throw_ios_failure(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(ios::failure(_(__s))); }

  void
  __throw_ios_failure(const char* str, int)
  { __throw_ios_failure(str); }

#endif // _GLIBCXX_USE_DUAL_ABI

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

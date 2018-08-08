// Iostreams base classes -*- C++ -*-

// Copyright (C) 1997-2016 Free Software Foundation, Inc.
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

#if _GLIBCXX_USE_DUAL_ABI && __cpp_rtti
  // These functions are defined in src/c++11/cxx11-ios_failure.cc
  extern void __construct_ios_failure(void*, const char*);
  extern void __destroy_ios_failure(void*);
  extern bool __is_ios_failure_handler(const __cxxabiv1::__class_type_info*);

  // The type thrown to report errors during stream buffer operations.
  // In addition to the gcc4-compatible ios::failure base class it also has a
  // member of the ios::failure[abi:cxx11] type (in an opaque buffer).
  struct __iosfailure : std::ios::failure
  {
    __iosfailure(const char* s) : failure(s)
    { __construct_ios_failure(buf, failure::what()); }

    ~__iosfailure() throw()
    { __destroy_ios_failure(buf); }

    // Type that is layout-compatible with std::system_error
    struct __system_error : std::runtime_error
    {
      // Type that is layout-compatible with std::error_code
      struct error_code
      {
	error_code() { }
      private:
	int		_M_value;
	const void*	_M_cat;
      } _M_code;
    };

    // Use __system_error as a proxy for the ios::failure[abi:cxx11]
    // (which can't be declared here because _GLIBCXX_USE_CXX11_ABI == 0).
    // There are assertions in src/c++11/cxx11-ios_failure.cc to ensure the
    // size and alignment assumptions are valid.
    __attribute__((aligned(__alignof(__system_error))))
      unsigned char buf[sizeof(__system_error)];
  };

  // Custom type info for __ios_failure.
  class __iosfailure_type_info : __cxxabiv1::__si_class_type_info
  {
    ~__iosfailure_type_info();

    bool
    __do_upcast (const __class_type_info *dst_type,
		 void **obj_ptr) const;
  };

  __iosfailure_type_info::~__iosfailure_type_info() { }

  // This function gets called to see if an exception of type
  // __ios_failure can be upcast to the type in a catch handler.
  bool
  __iosfailure_type_info::__do_upcast(const __class_type_info *dst_type,
				      void **obj_ptr) const
  {
    // If the handler is for the ios::failure[abi:cxx11] type then
    // catch the object stored in __ios_failure::buf instead of
    // the __ios_failure exception object itself.
    if (__is_ios_failure_handler(dst_type))
      {
	*obj_ptr = static_cast<__iosfailure*>(*obj_ptr)->buf;
	return true;
      }
    // Otherwise proceeed as normal to see if the handler matches.
    return __class_type_info::__do_upcast(dst_type, obj_ptr);
  }
#else // _GLIBCXX_USE_DUAL_ABI && __cpp_rtti
  using __iosfailure = ios::failure;
#endif

  void
  __throw_ios_failure(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(__iosfailure(_(__s))); }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// Copyright (C) 2001, 2002, 2003, 2005, 2009 Free Software Foundation, Inc.
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

#include <bits/functexcept.h>
#include <cstdlib>
#include <exception>
#include <stdexcept>
#include <new>
#include <typeinfo>
#include <ios>
#include <system_error>
#include <future>
#include <functional>

#ifdef _GLIBCXX_USE_NLS
# include <libintl.h>
# define _(msgid)   gettext (msgid)
#else
# define _(msgid)   (msgid)
#endif

_GLIBCXX_BEGIN_NAMESPACE(std)

#if __EXCEPTIONS
  void
  __throw_bad_exception(void)
  { throw bad_exception(); }

  void
  __throw_bad_alloc(void)
  { throw bad_alloc(); }

  void
  __throw_bad_cast(void)
  { throw bad_cast(); }

  void
  __throw_bad_typeid(void)
  { throw bad_typeid(); }

  void
  __throw_logic_error(const char* __s)
  { throw logic_error(_(__s)); }

  void
  __throw_domain_error(const char* __s)
  { throw domain_error(_(__s)); }

  void
  __throw_invalid_argument(const char* __s)
  { throw invalid_argument(_(__s)); }

  void
  __throw_length_error(const char* __s)
  { throw length_error(_(__s)); }

  void
  __throw_out_of_range(const char* __s)
  { throw out_of_range(_(__s)); }

  void
  __throw_runtime_error(const char* __s)
  { throw runtime_error(_(__s)); }

  void
  __throw_range_error(const char* __s)
  { throw range_error(_(__s)); }

  void
  __throw_overflow_error(const char* __s)
  { throw overflow_error(_(__s)); }

  void
  __throw_underflow_error(const char* __s)
  { throw underflow_error(_(__s)); }

  void
  __throw_ios_failure(const char* __s)
  { throw ios_base::failure(_(__s)); }

  void
  __throw_system_error(int __i)
  { throw system_error(error_code(__i, generic_category())); }

  void
  __throw_future_error(int __i)
  { throw future_error(future_errc(__i)); }

  void
  __throw_bad_function_call()
  { throw bad_function_call(); }
#else
  void
  __throw_bad_exception(void)
  { std::abort(); }

  void
  __throw_bad_alloc(void)
  { std::abort(); }

  void
  __throw_bad_cast(void)
  { std::abort(); }

  void
  __throw_bad_typeid(void)
  { std::abort(); }

  void
  __throw_logic_error(const char*)
  { std::abort(); }

  void
  __throw_domain_error(const char*)
  { std::abort(); }

  void
  __throw_invalid_argument(const char*)
  { std::abort(); }

  void
  __throw_length_error(const char*)
  { std::abort(); }

  void
  __throw_out_of_range(const char*)
  { std::abort(); }

  void
  __throw_runtime_error(const char*)
  { std::abort(); }

  void
  __throw_range_error(const char*)
  { std::abort(); }

  void
  __throw_overflow_error(const char*)
  { std::abort(); }

  void
  __throw_underflow_error(const char*)
  { std::abort(); }

  void
  __throw_ios_failure(const char*)
  { std::abort(); }

  void
  __throw_system_error(int)
  { std::abort(); }

  void
  __throw_future_error(int)
  { std::abort(); }

  void
  __throw_bad_function_call()
  { std::abort(); }

#endif //__EXCEPTIONS

_GLIBCXX_END_NAMESPACE

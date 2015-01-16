// Copyright (C) 2001-2015 Free Software Foundation, Inc.
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

// We don't want to change the type thrown by __throw_ios_failure (yet?)
#define _GLIBCXX_USE_CXX11_ABI 0

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
#include <bits/regex_error.h>
#include <stdarg.h>

#ifdef _GLIBCXX_USE_NLS
# include <libintl.h>
# define _(msgid)   gettext (msgid)
#else
# define _(msgid)   (msgid)
#endif

namespace __gnu_cxx
{
  int __snprintf_lite(char *__buf, size_t __bufsize, const char *__fmt,
		      va_list __ap);
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  __throw_bad_exception()
  { _GLIBCXX_THROW_OR_ABORT(bad_exception()); }

  void
  __throw_bad_alloc()
  { _GLIBCXX_THROW_OR_ABORT(bad_alloc()); }

  void
  __throw_bad_cast()
  { _GLIBCXX_THROW_OR_ABORT(bad_cast()); }

  void
  __throw_bad_typeid()
  { _GLIBCXX_THROW_OR_ABORT(bad_typeid()); }

  void
  __throw_logic_error(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(logic_error(_(__s))); }

  void
  __throw_domain_error(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(domain_error(_(__s))); }

  void
  __throw_invalid_argument(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(invalid_argument(_(__s))); }

  void
  __throw_length_error(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(length_error(_(__s))); }

  void
  __throw_out_of_range(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(out_of_range(_(__s))); }

  void
  __throw_out_of_range_fmt(const char* __fmt, ...)
  {
    const size_t __len = __builtin_strlen(__fmt);
    // We expect at most 2 numbers, and 1 short string. The additional
    // 512 bytes should provide more than enough space for expansion.
    const size_t __alloca_size = __len + 512;
    char *const __s = static_cast<char*>(__builtin_alloca(__alloca_size));
    va_list __ap;

    va_start(__ap, __fmt);
    __gnu_cxx::__snprintf_lite(__s, __alloca_size, __fmt, __ap);
    _GLIBCXX_THROW_OR_ABORT(out_of_range(_(__s)));
    va_end(__ap);  // Not reached.
  }

  void
  __throw_runtime_error(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(runtime_error(_(__s))); }

  void
  __throw_range_error(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(range_error(_(__s))); }

  void
  __throw_overflow_error(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(overflow_error(_(__s))); }

  void
  __throw_underflow_error(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(underflow_error(_(__s))); }

  void
  __throw_ios_failure(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(ios_base::failure(_(__s))); }

  void
  __throw_system_error(int __i __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(system_error(error_code(__i,
						    generic_category()))); }

  void
  __throw_future_error(int __i __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(future_error(make_error_code(future_errc(__i)))); }

  void
  __throw_bad_function_call()
  { _GLIBCXX_THROW_OR_ABORT(bad_function_call()); }

  void
  __throw_regex_error(regex_constants::error_type __ecode
		      __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(regex_error(__ecode)); }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

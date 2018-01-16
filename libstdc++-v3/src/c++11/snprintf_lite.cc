// Debugging support -*- C++ -*-

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <stdarg.h>
#include <bits/functexcept.h>
#include <bits/locale_facets.h>

namespace std {
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  template<typename _CharT, typename _ValueT>
  int
  __int_to_char(_CharT* __bufend, _ValueT __v, const _CharT* __lit,
                ios_base::fmtflags __flags, bool __dec);
_GLIBCXX_END_NAMESPACE_VERSION
}

namespace __gnu_cxx {

  // Private helper to throw logic error if snprintf_lite runs out
  // of space (which is not expected to ever happen).
  // NUL-terminates __buf.
  void
  __throw_insufficient_space(const char *__buf, const char *__bufend)
    __attribute__((__noreturn__));

  void
  __throw_insufficient_space(const char *__buf, const char *__bufend)
  {
    // Include space for trailing NUL.
    const size_t __len = __bufend - __buf + 1;

    const char __err[] = "not enough space for format expansion "
      "(Please submit full bug report at https://gcc.gnu.org/bugs/):\n    ";
    const size_t __errlen = sizeof(__err) - 1;

    char *const __e
      = static_cast<char*>(__builtin_alloca(__errlen + __len));

    __builtin_memcpy(__e, __err, __errlen);
    __builtin_memcpy(__e + __errlen, __buf, __len - 1);
    __e[__errlen + __len - 1] = '\0';
    std::__throw_logic_error(__e);
  }


  // Private routine to append decimal representation of VAL to the given
  // BUFFER, but not more than BUFSIZE characters.
  // Does not NUL-terminate the output buffer.
  // Returns number of characters appended, or -1 if BUFSIZE is too small.
  int __concat_size_t(char *__buf, size_t __bufsize, size_t __val)
  {
    // Long enough for decimal representation.
    int __ilen = 3 * sizeof(__val);
    char *__cs = static_cast<char*>(__builtin_alloca(__ilen));
    char* __out = __cs + __ilen;
    do
      {
	*--__out = "0123456789"[__val % 10];
	__val /= 10;
      }
    while (__val != 0);
    size_t __len = __cs + __ilen - __out;
    if (__bufsize < __len)
      return -1;

    __builtin_memcpy(__buf, __cs + __ilen - __len, __len);
    return __len;
  }


  // Private routine to print into __buf arguments according to format,
  // not to exceed __bufsize.
  // Only '%%', '%s' and '%zu' format specifiers are understood.
  // Returns number of characters printed (excluding terminating NUL).
  // Always NUL-terminates __buf.
  // Throws logic_error on insufficient space.
  int __snprintf_lite(char *__buf, size_t __bufsize, const char *__fmt,
		      va_list __ap)
  {
    char *__d = __buf;
    const char *__s = __fmt;
    const char *const __limit = __d + __bufsize - 1;  // Leave space for NUL.

    while (__s[0] != '\0' && __d < __limit)
      {
	if (__s[0] == '%')
	  switch (__s[1])
	    {
	    default:  // Stray '%'. Just print it.
	      break;
	    case '%':  // '%%'
	      __s += 1;
	      break;
	    case 's':  // '%s'.
	      {
		const char *__v = va_arg(__ap, const char *);

		while (__v[0] != '\0' && __d < __limit)
		  *__d++ = *__v++;

		if (__v[0] != '\0')
		  // Not enough space for __fmt expansion.
		  __throw_insufficient_space(__buf, __d);

		__s += 2;  // Step over %s.
		continue;
	      }
	      break;
	    case 'z':
	      if (__s[2] == 'u')  // '%zu' -- expand next size_t arg.
		{
		  const int __len = __concat_size_t(__d, __limit - __d,
						    va_arg(__ap, size_t));
		  if (__len > 0)
		    __d += __len;
		  else
		    // Not enough space for __fmt expansion.
		    __throw_insufficient_space(__buf, __d);

		  __s += 3;  // Step over %zu
		  continue;
		}
	      // Stray '%zX'. Just print it.
	      break;
	    }
	*__d++ = *__s++;
      }

    if (__s[0] != '\0')
      // Not enough space for __fmt expansion.
      __throw_insufficient_space(__buf, __d);

    *__d = '\0';
    return __d - __buf;
  }

}  // __gnu_cxx

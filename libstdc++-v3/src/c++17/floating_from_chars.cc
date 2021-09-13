// std::from_chars implementation for floating-point types -*- C++ -*-

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
// ISO C++ 14882:2017
// 23.2.9  Primitive numeric input conversion [utility.from.chars]
//

// Prefer to use std::pmr::string if possible, which requires the cxx11 ABI.
#define _GLIBCXX_USE_CXX11_ABI 1

#include <charconv>
#include <string>
#include <memory_resource>
#include <cfenv>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <locale.h>
#include <bits/functexcept.h>
#if _GLIBCXX_HAVE_XLOCALE_H
# include <xlocale.h>
#endif

#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
#ifndef __LONG_DOUBLE_IBM128__
#error "floating_from_chars.cc must be compiled with -mabi=ibmlongdouble"
#endif
// strtold for __ieee128
extern "C" __ieee128 __strtoieee128(const char*, char**);
#endif

#if _GLIBCXX_HAVE_USELOCALE
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace
{
  // A memory resource with a static buffer that can be used for small
  // allocations. At most one allocation using the freestore can be done
  // if the static buffer is insufficient. The callers below only require
  // a single allocation, so there's no need for anything more complex.
  struct buffer_resource : pmr::memory_resource
  {
    ~buffer_resource() { if (m_ptr) operator delete(m_ptr, m_bytes); }

    void*
    do_allocate(size_t bytes, size_t alignment [[maybe_unused]]) override
    {
      // Allocate from the buffer if it will fit.
      if (m_bytes < sizeof(m_buf) && (m_bytes + bytes) <= sizeof(m_buf))
	return m_buf + std::__exchange(m_bytes, m_bytes + bytes);

      __glibcxx_assert(m_ptr == nullptr);
      __glibcxx_assert(alignment != 1);

      m_ptr = operator new(bytes);
      m_bytes = bytes;
      return m_ptr;
    }

    void
    do_deallocate(void*, size_t, size_t) noexcept override
    { /* like pmr::monotonic_buffer_resource, do nothing here */ }

    bool
    do_is_equal(const pmr::memory_resource& other) const noexcept override
    { return &other == this; }

    static constexpr int guaranteed_capacity() { return sizeof(m_buf); }

  private:
    char m_buf[512];
    size_t m_bytes = 0;
    void* m_ptr = nullptr;
  };

#if _GLIBCXX_USE_CXX11_ABI
  using buffered_string = std::pmr::string;
#else
  using buffered_string = std::string;
#endif

  inline bool valid_fmt(chars_format fmt)
  {
    return fmt != chars_format{}
      && ((fmt & chars_format::general) == fmt
	  || (fmt & chars_format::hex) == fmt);
  }

  constexpr char hex_digits[] = "abcdefABCDEF0123456789";
  constexpr auto dec_digits = hex_digits + 12;

  // Find initial portion of [first, last) containing a floating-point number.
  // The string `digits` is either `dec_digits` or `hex_digits`
  // and `exp` is 'e' or 'p' or '\0'.
  const char*
  find_end_of_float(const char* first, const char* last, const char* digits,
		    char exp)
  {
    while (first < last && strchr(digits, *first) != nullptr)
      ++first;
    if (first < last && *first == '.')
      {
	++first;
	while (first < last && strchr(digits, *first))
	  ++first;
      }
    if (first < last && exp != 0 && std::tolower((unsigned char)*first) == exp)
      {
	++first;
	if (first < last && (*first == '-' || *first == '+'))
	  ++first;
	while (first < last && strchr(dec_digits, *first) != nullptr)
	  ++first;
      }
    return first;
  }

  // Determine the prefix of [first, last) that matches the pattern
  // corresponding to `fmt`.
  // Returns a NTBS containing the pattern, using `buf` to allocate
  // additional storage if needed.
  // Returns a nullptr if a valid pattern is not present.
  const char*
  pattern(const char* const first, const char* last,
	  chars_format& fmt, buffered_string& buf)
  {
    // fmt has the value of one of the enumerators of chars_format.
    __glibcxx_assert(valid_fmt(fmt));

    string_view res;

    if (first == last || *first == '+') [[unlikely]]
	return nullptr;

    const int neg = (*first == '-');

    if (std::memchr("iInN", (unsigned char)first[neg], 4))
      {
	ptrdiff_t len = last - first;
	if (len < (3 + neg))
	  return nullptr;

	// possible infinity or NaN, let strtod decide
	if (first[neg] == 'i' || first[neg] == 'I')
	  {
	    // Need at most 9 chars for "-INFINITY", ignore anything after it.
	    len = std::min(len, ptrdiff_t(neg + 8));
	  }
	else if (len > (neg + 3) && first[neg + 3] == '(')
	  {
	    // Look for end of "NAN(n-char-sequence)"
	    if (void* p = std::memchr(const_cast<char*>(first)+4, ')', len-4))
	      len = static_cast<char*>(p) + 1 - first;
#ifndef __cpp_exceptions
	    if (len > buffer_resource::guaranteed_capacity())
	      {
		// The character sequence is too large for the buffer.
		// Allocation failure could terminate the process,
		// so just return an error via the fmt parameter.
		fmt = chars_format{};
		return nullptr;
	      }
#endif
	  }
	else // Only need 4 chars for "-NAN"
	  len = neg + 3;

	buf.assign(first, 0, len);
	// prevent make_result correcting for "0x"
	fmt = chars_format::general;
	return buf.c_str();
      }

    const char* digits;
    char* ptr;

    // Assign [first,last) to a std::string to get a NTBS that can be used
    // with strspn, strtod etc.
    // If the string would be longer than the fixed buffer inside the
    // buffer_resource type use find_end_of_float to try to reduce how
    // much memory is needed, to reduce the chance of std::bad_alloc.

    if (fmt == chars_format::hex)
      {
	digits = hex_digits;

	if ((last - first + 2) > buffer_resource::guaranteed_capacity())
	  {
	    last = find_end_of_float(first + neg, last, digits, 'p');
#ifndef __cpp_exceptions
	    if ((last - first + 2) > buffer_resource::guaranteed_capacity())
	      {
		// The character sequence is still too large for the buffer.
		// Allocation failure could terminate the process,
		// so just return an error via the fmt parameter.
		fmt = chars_format{};
		return nullptr;
	      }
#endif
	  }

	buf = "-0x" + !neg;
	buf.append(first + neg, last);
	ptr = buf.data() + neg + 2;
      }
    else
      {
	digits = dec_digits;

	if ((last - first) > buffer_resource::guaranteed_capacity())
	  {
	    last = find_end_of_float(first + neg, last, digits,
				     "e"[fmt == chars_format::fixed]);
#ifndef __cpp_exceptions
	    if ((last - first) > buffer_resource::guaranteed_capacity())
	      {
		// The character sequence is still too large for the buffer.
		// Allocation failure could terminate the process,
		// so just return an error via the fmt parameter.
		fmt = chars_format{};
		return nullptr;
	      }
#endif
	  }
	buf.assign(first, last);
	ptr = buf.data() + neg;
      }

    // "A non-empty sequence of decimal digits" or
    // "A non-empty sequence of hexadecimal digits"
    size_t len = std::strspn(ptr, digits);
    // "possibly containing a radix character,"
    if (ptr[len] == '.')
      {
	const size_t len2 = std::strspn(ptr + len + 1, digits);
	if (len + len2)
	  ptr += len + 1 + len2;
	else
	  return nullptr;
      }
    else if (len == 0) [[unlikely]]
      return nullptr;
    else
      ptr += len;

    if (fmt == chars_format::fixed)
      {
	// Truncate the string to stop strtod parsing past this point.
	*ptr = '\0';
      }
    else if (fmt == chars_format::scientific)
      {
	// Check for required exponent part which starts with 'e' or 'E'
	if (*ptr != 'e' && *ptr != 'E')
	  return nullptr;
	// then an optional plus or minus sign
	const int sign = (ptr[1] == '-' || ptr[1] == '+');
	// then a nonempty sequence of decimal digits
	if (!std::memchr(dec_digits, (unsigned char)ptr[1+sign], 10))
	  return nullptr;
      }
    else if (fmt == chars_format::general)
      {
	if (*ptr == 'x' || *ptr == 'X')
	  *ptr = '\0';
      }

    return buf.c_str();
  }

  // Convert the NTBS `str` to a floating-point value of type `T`.
  // If `str` cannot be converted, `value` is unchanged and `0` is returned.
  // Otherwise, let N be the number of characters consumed from `str`.
  // On success `value` is set to the converted value and N is returned.
  // If the converted value is out of range, `value` is unchanged and
  // -N is returned.
  template<typename T>
  ptrdiff_t
  from_chars_impl(const char* str, T& value, errc& ec) noexcept
  {
    if (locale_t loc = ::newlocale(LC_ALL_MASK, "C", (locale_t)0)) [[likely]]
      {
	locale_t orig = ::uselocale(loc);

#if _GLIBCXX_USE_C99_FENV_TR1 && defined(FE_TONEAREST)
	const int rounding = std::fegetround();
	if (rounding != FE_TONEAREST)
	  std::fesetround(FE_TONEAREST);
#endif

	const int save_errno = errno;
	errno = 0;
	char* endptr;
	T tmpval;
#if _GLIBCXX_USE_C99_STDLIB
	if constexpr (is_same_v<T, float>)
	  tmpval = std::strtof(str, &endptr);
	else if constexpr (is_same_v<T, double>)
	  tmpval = std::strtod(str, &endptr);
	else if constexpr (is_same_v<T, long double>)
	  tmpval = std::strtold(str, &endptr);
# ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
	else if constexpr (is_same_v<T, __ieee128>)
	  tmpval = __strtoieee128(str, &endptr);
# endif
#else
	tmpval = std::strtod(str, &endptr);
#endif
	const int conv_errno = std::__exchange(errno, save_errno);

#if _GLIBCXX_USE_C99_FENV_TR1 && defined(FE_TONEAREST)
	if (rounding != FE_TONEAREST)
	  std::fesetround(rounding);
#endif

	::uselocale(orig);
	::freelocale(loc);

	const ptrdiff_t n = endptr - str;
	if (conv_errno == ERANGE) [[unlikely]]
	  {
	    if (__builtin_isinf(tmpval)) // overflow
	      ec = errc::result_out_of_range;
	    else // underflow (LWG 3081 wants to set value = tmpval here)
	      ec = errc::result_out_of_range;
	  }
	else if (n)
	  {
	    value = tmpval;
	    ec = errc();
	  }
	return n;
      }
    else if (errno == ENOMEM)
      ec = errc::not_enough_memory;

    return 0;
  }

  inline from_chars_result
  make_result(const char* str, ptrdiff_t n, chars_format fmt, errc ec) noexcept
  {
    from_chars_result result = { str, ec };
    if (n != 0)
      {
	if (fmt == chars_format::hex)
	  n -= 2; // correct for the "0x" inserted into the pattern
	result.ptr += n;
      }
    else if (fmt == chars_format{}) [[unlikely]]
      {
	// FIXME: the standard does not allow this result.
	ec = errc::not_enough_memory;
      }
    return result;
  }

#if ! _GLIBCXX_USE_CXX11_ABI
  inline bool
  reserve_string(std::string& s) noexcept
  {
    __try
      {
	s.reserve(buffer_resource::guaranteed_capacity());
      }
    __catch (const std::bad_alloc&)
      {
	return false;
      }
    return true;
  }
#endif

} // namespace

// FIXME: This should be reimplemented so it doesn't use strtod and newlocale.
// That will avoid the need for any memory allocation, meaning that the
// non-conforming errc::not_enough_memory result cannot happen.

from_chars_result
from_chars(const char* first, const char* last, float& value,
	   chars_format fmt) noexcept
{
  errc ec = errc::invalid_argument;
#if _GLIBCXX_USE_CXX11_ABI
  buffer_resource mr;
  pmr::string buf(&mr);
#else
  string buf;
  if (!reserve_string(buf))
    return make_result(first, 0, {}, ec);
#endif
  size_t len = 0;
  __try
    {
      if (const char* pat = pattern(first, last, fmt, buf)) [[likely]]
	len = from_chars_impl(pat, value, ec);
    }
  __catch (const std::bad_alloc&)
    {
      fmt = chars_format{};
    }
  return make_result(first, len, fmt, ec);
}

from_chars_result
from_chars(const char* first, const char* last, double& value,
	   chars_format fmt) noexcept
{
  errc ec = errc::invalid_argument;
#if _GLIBCXX_USE_CXX11_ABI
  buffer_resource mr;
  pmr::string buf(&mr);
#else
  string buf;
  if (!reserve_string(buf))
    return make_result(first, 0, {}, ec);
#endif
  size_t len = 0;
  __try
    {
      if (const char* pat = pattern(first, last, fmt, buf)) [[likely]]
	len = from_chars_impl(pat, value, ec);
    }
  __catch (const std::bad_alloc&)
    {
      fmt = chars_format{};
    }
  return make_result(first, len, fmt, ec);
}

from_chars_result
from_chars(const char* first, const char* last, long double& value,
	   chars_format fmt) noexcept
{
  errc ec = errc::invalid_argument;
#if _GLIBCXX_USE_CXX11_ABI
  buffer_resource mr;
  pmr::string buf(&mr);
#else
  string buf;
  if (!reserve_string(buf))
    return make_result(first, 0, {}, ec);
#endif
  size_t len = 0;
  __try
    {
      if (const char* pat = pattern(first, last, fmt, buf)) [[likely]]
	len = from_chars_impl(pat, value, ec);
    }
  __catch (const std::bad_alloc&)
    {
      fmt = chars_format{};
    }
  return make_result(first, len, fmt, ec);
}

#ifdef _GLIBCXX_LONG_DOUBLE_COMPAT
// Make std::from_chars for 64-bit long double an alias for the overload
// for double.
extern "C" from_chars_result
_ZSt10from_charsPKcS0_ReSt12chars_format(const char* first, const char* last,
					 long double& value,
					 chars_format fmt) noexcept
__attribute__((alias ("_ZSt10from_charsPKcS0_RdSt12chars_format")));
#endif

#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
from_chars_result
from_chars(const char* first, const char* last, __ieee128& value,
	   chars_format fmt) noexcept
{
  buffer_resource mr;
  pmr::string buf(&mr);
  size_t len = 0;
  errc ec = errc::invalid_argument;
  __try
    {
      if (const char* pat = pattern(first, last, fmt, buf)) [[likely]]
	len = from_chars_impl(pat, value, ec);
    }
  __catch (const std::bad_alloc&)
    {
      fmt = chars_format{};
    }
  return make_result(first, len, fmt, ec);
}
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // _GLIBCXX_HAVE_USELOCALE

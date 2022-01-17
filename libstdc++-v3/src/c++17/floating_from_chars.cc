// std::from_chars implementation for floating-point types -*- C++ -*-

// Copyright (C) 2020-2022 Free Software Foundation, Inc.
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
#include <bit>
#include <string>
#include <memory_resource>
#include <cfenv>
#include <cfloat>
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

#if _GLIBCXX_FLOAT_IS_IEEE_BINARY32 && _GLIBCXX_DOUBLE_IS_IEEE_BINARY64
# define USE_LIB_FAST_FLOAT 1
#endif

#if USE_LIB_FAST_FLOAT
# define FASTFLOAT_DEBUG_ASSERT __glibcxx_assert
namespace
{
# include "fast_float/fast_float.h"
} // anon namespace
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

#if _GLIBCXX_FLOAT_IS_IEEE_BINARY32 && _GLIBCXX_DOUBLE_IS_IEEE_BINARY64
  // If the given ASCII character represents a hexit, return that hexit.
  // Otherwise return -1.
  int
  ascii_to_hexit(char ch)
  {
    if (ch >= '0' && ch <= '9')
      return ch - '0';
    if (ch >= 'a' && ch <= 'f')
      return ch - 'a' + 10;
    if (ch >= 'A' && ch <= 'F')
      return ch - 'A' + 10;
    return -1;
  }

  // Return true iff [FIRST,LAST) begins with PREFIX, ignoring case.
  bool
  starts_with_ci(const char* first, const char* last, string_view prefix)
  {
    __glibcxx_requires_valid_range(first, last);

    for (char ch : prefix)
      {
	__glibcxx_assert(ch >= 'a' && ch <= 'z');
	if (first == last || (*first != ch && *first != ch - 32))
	  return false;
	++first;
      }

    return true;
  }

  // An implementation of hexadecimal float parsing for binary32/64.
  template<typename T>
  from_chars_result
  __floating_from_chars_hex(const char* first, const char* last, T& value)
  {
    static_assert(is_same_v<T, float> || is_same_v<T, double>);

    using uint_t = conditional_t<is_same_v<T, float>, uint32_t, uint64_t>;
    constexpr int mantissa_bits = is_same_v<T, float> ? 23 : 52;
    constexpr int exponent_bits = is_same_v<T, float> ? 8 : 11;
    constexpr int exponent_bias = (1 << (exponent_bits - 1)) - 1;

    __glibcxx_requires_valid_range(first, last);
    if (first == last)
      return {first, errc::invalid_argument};

    // Consume the sign bit.
    const char* const orig_first = first;
    bool sign_bit = false;
    if (*first == '-')
      {
	sign_bit = true;
	++first;
      }

    // Handle "inf", "infinity", "NaN" and variants thereof.
    if (first != last)
      if (*first == 'i' || *first == 'I' || *first == 'n' || *first == 'N') [[unlikely]]
	{
	  if (starts_with_ci(first, last, "inf"sv))
	    {
	      first += strlen("inf");
	      if (starts_with_ci(first, last, "inity"sv))
		first += strlen("inity");

	      uint_t result = 0;
	      result |= sign_bit;
	      result <<= exponent_bits;
	      result |= (1ull << exponent_bits) - 1;
	      result <<= mantissa_bits;
	      memcpy(&value, &result, sizeof(result));

	      return {first, errc{}};
	    }
	  else if (starts_with_ci(first, last, "nan"))
	    {
	      first += strlen("nan");

	      if (first != last && *first == '(')
		{
		  // Tentatively consume the '(' as we look for an optional
		  // n-char-sequence followed by a ')'.
		  const char* const fallback_first = first;
		  for (;;)
		    {
		      ++first;
		      if (first == last)
			{
			  first = fallback_first;
			  break;
			}

		      char ch = *first;
		      if (ch == ')')
			{
			  ++first;
			  break;
			}
		      else if ((ch >= '0' && ch <= '9')
			       || (ch >= 'a' && ch <= 'z')
			       || (ch >= 'A' && ch <= 'Z')
			       || ch == '_')
			continue;
		      else
			{
			  first = fallback_first;
			  break;
			}
		    }
		}

	      // We make the implementation-defined decision of ignoring the
	      // sign bit and the n-char-sequence when assembling the NaN.
	      uint_t result = 0;
	      result <<= exponent_bits;
	      result |= (1ull << exponent_bits) - 1;
	      result <<= mantissa_bits;
	      result |= (1ull << (mantissa_bits - 1)) | 1;
	      memcpy(&value, &result, sizeof(result));

	      return {first, errc{}};
	    }
	}

    // Consume all insignificant leading zeros in the whole part of the
    // mantissa.
    bool seen_hexit = false;
    while (first != last && *first == '0')
      {
	seen_hexit = true;
	++first;
      }

    // Now consume the rest of the written mantissa, populating MANTISSA with
    // the first MANTISSA_BITS+k significant bits of the written mantissa, where
    // 1 <= k <= 4 is the bit width of the leading significant written hexit.
    //
    // Examples:
    //  After parsing "1.2f3", MANTISSA is 0x12f30000000000 (bit_width=52+1).
    //  After parsing ".0000f0e", MANTISSA is 0xf0e00000000000 (bit_width=52+4).
    //  After parsing ".1234567890abcd8", MANTISSA is 0x1234567890abcd (bit_width=52+1)
    //   and MIDPOINT_BIT is true (and NONZERO_TAIL is false).
    uint_t mantissa = 0;
    int mantissa_idx = mantissa_bits; // The current bit index into MANTISSA
				       // into which we'll write the next hexit.
    int exponent_adjustment = 0; // How much we'd have to adjust the written
				 // exponent in order to represent the mantissa
				 // in scientific form h.hhhhhhhhhhhhh.
    bool midpoint_bit = false; // Whether the MANTISSA_BITS+k+1 significant
			       // bit is set in the written mantissa.
    bool nonzero_tail = false; // Whether some bit thereafter is set in the
			       // written mantissa.
    bool seen_decimal_point = false;
    for (; first != last; ++first)
      {
	char ch = *first;
	if (ch == '.' && !seen_decimal_point)
	  {
	    seen_decimal_point = true;
	    continue;
	  }

	int hexit = ascii_to_hexit(ch);
	if (hexit == -1)
	  break;
	seen_hexit = true;

	if (!seen_decimal_point && mantissa != 0)
	  exponent_adjustment += 4;
	else if (seen_decimal_point && mantissa == 0)
	  {
	    exponent_adjustment -= 4;
	    if (hexit == 0x0)
	      continue;
	  }

	if (mantissa_idx >= 0)
	  mantissa |= uint_t(hexit) << mantissa_idx;
	else if (mantissa_idx >= -4)
	  {
	    if constexpr (is_same_v<T, float>)
	      {
		__glibcxx_assert(mantissa_idx == -1);
		mantissa |= hexit >> 1;
		midpoint_bit = (hexit & 0b0001) != 0;
	      }
	    else
	      {
		__glibcxx_assert(mantissa_idx == -4);
		midpoint_bit = (hexit & 0b1000) != 0;
		nonzero_tail = (hexit & 0b0111) != 0;
	      }
	  }
	else
	  nonzero_tail |= (hexit != 0x0);

	mantissa_idx -= 4;
      }
    if (mantissa != 0)
      __glibcxx_assert(__bit_width(mantissa) >= mantissa_bits + 1
		       && __bit_width(mantissa) <= mantissa_bits + 4);
    else
      __glibcxx_assert(!midpoint_bit && !nonzero_tail);

    if (!seen_hexit)
      // If we haven't seen any hexit at this point, the parse failed.
      return {orig_first, errc::invalid_argument};

    // Parse the written exponent.
    int written_exponent = 0;
    if (first != last && *first == 'p')
      {
	// Tentatively consume the 'p' and try to parse a decimal number.
	const char* const fallback_first = first;
	++first;
	if (first != last && *first == '+')
	  ++first;
	from_chars_result fcr = from_chars(first, last, written_exponent, 10);
	if (fcr.ptr == first)
	  // The parse failed, so undo consuming the 'p' and carry on as if the
	  // exponent was omitted (i.e. is 0).
	  first = fallback_first;
	else
	  {
	    first = fcr.ptr;
	    if (mantissa != 0 && fcr.ec == errc::result_out_of_range)
	      // Punt on very large exponents for now. FIXME
	      return {first, errc::result_out_of_range};
	  }
      }
    int biased_exponent = written_exponent + exponent_bias;
    if (exponent_adjustment != 0)
      // The mantissa wasn't written in scientific form.  Adjust the exponent
      // so that we may assume scientific form.
      //
      // Examples;
      //  For input "a.bcp5", EXPONENT_ADJUSTMENT would be 0 since this
      //   written mantissa is already in scientific form.
      //  For input "ab.cp5", EXPONENT_ADJUSTMENT would be 4 since the
      //   scientific form is "a.bcp9".
      //  For input 0.0abcp5", EXPONENT_ADJUSTMENT would be -8 since the
      //   scientific form is "a.bcp-3".
      biased_exponent += exponent_adjustment;

    // Shifts the mantissa to the right by AMOUNT while updating
    // BIASED_EXPONENT, MIDPOINT_BIT and NONZERO_TAIL accordingly.
    auto shift_mantissa = [&] (int amount) {
      __glibcxx_assert(amount >= 0);
      if (amount > mantissa_bits + 1)
	{
	  // Shifting the mantissa by an amount greater than its precision.
	  nonzero_tail |= midpoint_bit;
	  nonzero_tail |= mantissa != 0;
	  midpoint_bit = false;
	  mantissa = 0;
	  biased_exponent += amount;
	}
      else if (amount != 0)
	{
	  nonzero_tail |= midpoint_bit;
	  nonzero_tail |= (mantissa & ((1ull << (amount - 1)) - 1)) != 0;
	  midpoint_bit = (mantissa & (1ull << (amount - 1))) != 0;
	  mantissa >>= amount;
	  biased_exponent += amount;
	}
    };

    if (mantissa != 0)
      {
	// If the leading hexit is not '1', shift MANTISSA to make it so.
	// This normalizes input like "4.08p0" into "1.02p2".
	const int leading_hexit = mantissa >> mantissa_bits;
	const int leading_hexit_width = __bit_width(leading_hexit); // FIXME: optimize?
	__glibcxx_assert(leading_hexit_width >= 1 && leading_hexit_width <= 4);
	shift_mantissa(leading_hexit_width - 1);
	// After this adjustment, we can assume the leading hexit is '1'.
	__glibcxx_assert((mantissa >> mantissa_bits) == 0x1);
      }

    if (biased_exponent <= 0)
      {
	// This number is too small to be represented as a normal number, so
	// try for a subnormal number by shifting the mantissa sufficiently.
	// We need to shift by 1 more than -BIASED_EXPONENT because the leading
	// mantissa bit is omitted in the representation of a normal number but
	// not in a subnormal number.
	shift_mantissa(-biased_exponent + 1);
	__glibcxx_assert(!(mantissa & (1ull << mantissa_bits)));
	__glibcxx_assert(biased_exponent == 1);
	biased_exponent = 0;
      }

    // Perform round-to-nearest, tie-to-even rounding according to
    // MIDPOINT_BIT and NONZERO_TAIL.
    if (midpoint_bit && (nonzero_tail || (mantissa % 2) != 0))
      {
	// Rounding away from zero.
	++mantissa;
	midpoint_bit = false;
	nonzero_tail = false;

	// Deal with a couple of corner cases after rounding.
	if (mantissa == (1ull << mantissa_bits))
	  {
	    // We rounded the subnormal number 1.fffffffffffff...p-1023
	    // up to the normal number 1p-1022.
	    __glibcxx_assert(biased_exponent == 0);
	    ++biased_exponent;
	  }
	else if (mantissa & (1ull << (mantissa_bits + 1)))
	  {
	    // We rounded the normal number 1.fffffffffffff8pN (with maximal
	    // mantissa) up to to 1p(N+1).
	    mantissa >>= 1;
	    ++biased_exponent;
	  }
      }
    else
      {
	// Rounding toward zero.

	if (mantissa == 0 && (midpoint_bit || nonzero_tail))
	  {
	    // A nonzero number that rounds to zero is unrepresentable.
	    __glibcxx_assert(biased_exponent == 0);
	    return {first, errc::result_out_of_range};
	  }

	midpoint_bit = false;
	nonzero_tail = false;
      }

    if (mantissa != 0 && biased_exponent >= (1 << exponent_bits) - 1)
      // The exponent of this number is too large to be representable.
      return {first, errc::result_out_of_range};

    uint_t result = 0;
    if (mantissa == 0)
      {
	// Assemble a (possibly signed) zero.
	if (sign_bit)
	  result |= 1ull << (exponent_bits + mantissa_bits);
      }
    else
      {
	// Assemble a nonzero normal or subnormal value.
	result |= sign_bit;
	result <<= exponent_bits;
	result |= biased_exponent;
	result <<= mantissa_bits;
	result |= mantissa & ((1ull << mantissa_bits) - 1);
	// The implicit leading mantissa bit is set iff the number is normal.
	__glibcxx_assert(((mantissa & (1ull << mantissa_bits)) != 0)
			 == (biased_exponent != 0));
      }
    memcpy(&value, &result, sizeof(result));

    return {first, errc{}};
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
#if _GLIBCXX_FLOAT_IS_IEEE_BINARY32 && _GLIBCXX_DOUBLE_IS_IEEE_BINARY64
  if (fmt == chars_format::hex)
    return __floating_from_chars_hex(first, last, value);
  else
    {
      static_assert(USE_LIB_FAST_FLOAT);
      return fast_float::from_chars(first, last, value, fmt);
    }
#else
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
#endif
}

from_chars_result
from_chars(const char* first, const char* last, double& value,
	   chars_format fmt) noexcept
{
#if _GLIBCXX_FLOAT_IS_IEEE_BINARY32 && _GLIBCXX_DOUBLE_IS_IEEE_BINARY64
  if (fmt == chars_format::hex)
    return __floating_from_chars_hex(first, last, value);
  else
    {
      static_assert(USE_LIB_FAST_FLOAT);
      return fast_float::from_chars(first, last, value, fmt);
    }
#else
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
#endif
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

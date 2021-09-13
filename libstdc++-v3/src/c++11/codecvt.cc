// Locale support (codecvt) -*- C++ -*-

// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

#include <codecvt>
#include <cstring>		// std::memcpy, std::memcmp
#include <bits/stl_algobase.h>	// std::min

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // The standard doesn't define these operators, which is annoying.
  static underlying_type<codecvt_mode>::type
  to_integer(codecvt_mode m)
  { return static_cast<underlying_type<codecvt_mode>::type>(m); }

  static codecvt_mode& operator&=(codecvt_mode& m, codecvt_mode n)
  { return m = codecvt_mode(to_integer(m) & to_integer(n)); }

  static codecvt_mode& operator|=(codecvt_mode& m, codecvt_mode n)
  { return m = codecvt_mode(to_integer(m) | to_integer(n)); }

  static codecvt_mode operator~(codecvt_mode m)
  { return codecvt_mode(~to_integer(m)); }

namespace
{
  // Largest code point that fits in a single UTF-16 code unit.
  const char32_t max_single_utf16_unit = 0xFFFF;

  const char32_t max_code_point = 0x10FFFF;

  // The functions below rely on maxcode < incomplete_mb_character
  // (which is enforced by the codecvt_utf* classes on construction).
  const char32_t incomplete_mb_character = char32_t(-2);
  const char32_t invalid_mb_sequence = char32_t(-1);

  // Utility type for reading and writing code units of type Elem from
  // a range defined by a pair of pointers.
  template<typename Elem, bool Aligned = true>
    struct range
    {
      Elem* next;
      Elem* end;

      // Write a code unit.
      range& operator=(Elem e)
      {
	*next++ = e;
	return *this;
      }

      // Read the next code unit.
      Elem operator*() const { return *next; }

      // Read the Nth code unit.
      Elem operator[](size_t n) const { return next[n]; }

      // Move to the next code unit.
      range& operator++()
      {
	++next;
	return *this;
      }

      // Move to the Nth code unit.
      range& operator+=(size_t n)
      {
	next += n;
	return *this;
      }

      // The number of code units remaining.
      size_t size() const { return end - next; }

      // The number of bytes remaining.
      size_t nbytes() const { return (const char*)end - (const char*)next; }
    };

  // This specialization is used when accessing char16_t values through
  // pointers to char, which might not be correctly aligned for char16_t.
  template<typename Elem>
    struct range<Elem, false>
    {
      using value_type = typename remove_const<Elem>::type;

      using char_pointer = typename
	conditional<is_const<Elem>::value, const char*, char*>::type;

      char_pointer next;
      char_pointer end;

      // Write a code unit.
      range& operator=(Elem e)
      {
	memcpy(next, &e, sizeof(Elem));
	++*this;
	return *this;
      }

      // Read the next code unit.
      Elem operator*() const
      {
	value_type e;
	memcpy(&e, next, sizeof(Elem));
	return e;
      }

      // Read the Nth code unit.
      Elem operator[](size_t n) const
      {
	value_type e;
	memcpy(&e, next + n * sizeof(Elem), sizeof(Elem));
	return e;
      }

      // Move to the next code unit.
      range& operator++()
      {
	next += sizeof(Elem);
	return *this;
      }

      // Move to the Nth code unit.
      range& operator+=(size_t n)
      {
	next += n * sizeof(Elem);
	return *this;
      }

      // The number of code units remaining.
      size_t size() const { return nbytes() / sizeof(Elem); }

      // The number of bytes remaining.
      size_t nbytes() const { return end - next; }
    };

  // Multibyte sequences can have "header" consisting of Byte Order Mark
  const unsigned char utf8_bom[3] = { 0xEF, 0xBB, 0xBF };
  const unsigned char utf16_bom[2] = { 0xFE, 0xFF };
  const unsigned char utf16le_bom[2] = { 0xFF, 0xFE };

  // Write a BOM (space permitting).
  template<typename C, bool A, size_t N>
    bool
    write_bom(range<C, A>& to, const unsigned char (&bom)[N])
    {
      static_assert( (N / sizeof(C)) != 0, "" );
      static_assert( (N % sizeof(C)) == 0, "" );

      if (to.nbytes() < N)
	return false;
      memcpy(to.next, bom, N);
      to += (N / sizeof(C));
      return true;
    }

  // Try to read a BOM.
  template<typename C, bool A, size_t N>
    bool
    read_bom(range<C, A>& from, const unsigned char (&bom)[N])
    {
      static_assert( (N / sizeof(C)) != 0, "" );
      static_assert( (N % sizeof(C)) == 0, "" );

      if (from.nbytes() >= N && !memcmp(from.next, bom, N))
	{
	  from += (N / sizeof(C));
	  return true;
	}
      return false;
    }

  // If generate_header is set in mode write out UTF-8 BOM.
  template<typename C>
  bool
  write_utf8_bom(range<C>& to, codecvt_mode mode)
  {
    if (mode & generate_header)
      return write_bom(to, utf8_bom);
    return true;
  }

  // If generate_header is set in mode write out the UTF-16 BOM indicated
  // by whether little_endian is set in mode.
  template<bool Aligned>
  bool
  write_utf16_bom(range<char16_t, Aligned>& to, codecvt_mode mode)
  {
    if (mode & generate_header)
    {
      if (mode & little_endian)
	return write_bom(to, utf16le_bom);
      else
	return write_bom(to, utf16_bom);
    }
    return true;
  }

  // If consume_header is set in mode update from.next to after any BOM.
  template<typename C>
  void
  read_utf8_bom(range<const C>& from, codecvt_mode mode)
  {
    if (mode & consume_header)
      read_bom(from, utf8_bom);
  }

  // If consume_header is not set in mode, no effects.
  // Otherwise, if *from.next is a UTF-16 BOM increment from.next and then:
  // - if the UTF-16BE BOM was found unset little_endian in mode, or
  // - if the UTF-16LE BOM was found set little_endian in mode.
  template<bool Aligned>
  void
  read_utf16_bom(range<const char16_t, Aligned>& from, codecvt_mode& mode)
  {
    if (mode & consume_header)
      {
	if (read_bom(from, utf16_bom))
	  mode &= ~little_endian;
	else if (read_bom(from, utf16le_bom))
	  mode |= little_endian;
      }
  }

  // Read a codepoint from a UTF-8 multibyte sequence.
  // Updates from.next if the codepoint is not greater than maxcode.
  // Returns invalid_mb_sequence, incomplete_mb_character or the code point.
  template<typename C>
  char32_t
  read_utf8_code_point(range<const C>& from, unsigned long maxcode)
  {
    const size_t avail = from.size();
    if (avail == 0)
      return incomplete_mb_character;
    unsigned char c1 = from[0];
    // https://en.wikipedia.org/wiki/UTF-8#Sample_code
    if (c1 < 0x80)
    {
      ++from;
      return c1;
    }
    else if (c1 < 0xC2) // continuation or overlong 2-byte sequence
      return invalid_mb_sequence;
    else if (c1 < 0xE0) // 2-byte sequence
    {
      if (avail < 2)
	return incomplete_mb_character;
      unsigned char c2 = from[1];
      if ((c2 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      char32_t c = (c1 << 6) + c2 - 0x3080;
      if (c <= maxcode)
	from += 2;
      return c;
    }
    else if (c1 < 0xF0) // 3-byte sequence
    {
      if (avail < 3)
	return incomplete_mb_character;
      unsigned char c2 = from[1];
      if ((c2 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      if (c1 == 0xE0 && c2 < 0xA0) // overlong
	return invalid_mb_sequence;
      unsigned char c3 = from[2];
      if ((c3 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      char32_t c = (c1 << 12) + (c2 << 6) + c3 - 0xE2080;
      if (c <= maxcode)
	from += 3;
      return c;
    }
    else if (c1 < 0xF5) // 4-byte sequence
    {
      if (avail < 4)
	return incomplete_mb_character;
      unsigned char c2 = from[1];
      if ((c2 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      if (c1 == 0xF0 && c2 < 0x90) // overlong
	return invalid_mb_sequence;
      if (c1 == 0xF4 && c2 >= 0x90) // > U+10FFFF
      return invalid_mb_sequence;
      unsigned char c3 = from[2];
      if ((c3 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      unsigned char c4 = from[3];
      if ((c4 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      char32_t c = (c1 << 18) + (c2 << 12) + (c3 << 6) + c4 - 0x3C82080;
      if (c <= maxcode)
	from += 4;
      return c;
    }
    else // > U+10FFFF
      return invalid_mb_sequence;
  }

  template<typename C>
  bool
  write_utf8_code_point(range<C>& to, char32_t code_point)
  {
    if (code_point < 0x80)
      {
	if (to.size() < 1)
	  return false;
	to = code_point;
      }
    else if (code_point <= 0x7FF)
      {
	if (to.size() < 2)
	  return false;
	to = (code_point >> 6) + 0xC0;
	to = (code_point & 0x3F) + 0x80;
      }
    else if (code_point <= 0xFFFF)
      {
	if (to.size() < 3)
	  return false;
	to = (code_point >> 12) + 0xE0;
	to = ((code_point >> 6) & 0x3F) + 0x80;
	to = (code_point & 0x3F) + 0x80;
      }
    else if (code_point <= 0x10FFFF)
      {
	if (to.size() < 4)
	  return false;
	to = (code_point >> 18) + 0xF0;
	to = ((code_point >> 12) & 0x3F) + 0x80;
	to = ((code_point >> 6) & 0x3F) + 0x80;
	to = (code_point & 0x3F) + 0x80;
      }
    else
      return false;
    return true;
  }

  inline char16_t
  adjust_byte_order(char16_t c, codecvt_mode mode)
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    return (mode & little_endian) ? __builtin_bswap16(c) : c;
#else
    return (mode & little_endian) ? c : __builtin_bswap16(c);
#endif
  }

  // Return true if c is a high-surrogate (aka leading) code point.
  inline bool
  is_high_surrogate(char32_t c)
  {
    return c >= 0xD800 && c <= 0xDBFF;
  }

  // Return true if c is a low-surrogate (aka trailing) code point.
  inline bool
  is_low_surrogate(char32_t c)
  {
    return c >= 0xDC00 && c <= 0xDFFF;
  }

  inline char32_t
  surrogate_pair_to_code_point(char32_t high, char32_t low)
  {
    return (high << 10) + low - 0x35FDC00;
  }

  // Read a codepoint from a UTF-16 multibyte sequence.
  // The sequence's endianness is indicated by (mode & little_endian).
  // Updates from.next if the codepoint is not greater than maxcode.
  // Returns invalid_mb_sequence, incomplete_mb_character or the code point.
  template<bool Aligned>
    char32_t
    read_utf16_code_point(range<const char16_t, Aligned>& from,
			  unsigned long maxcode, codecvt_mode mode)
    {
      const size_t avail = from.size();
      if (avail == 0)
	return incomplete_mb_character;
      int inc = 1;
      char32_t c = adjust_byte_order(from[0], mode);
      if (is_high_surrogate(c))
	{
	  if (avail < 2)
	    return incomplete_mb_character;
	  const char16_t c2 = adjust_byte_order(from[1], mode);
	  if (is_low_surrogate(c2))
	    {
	      c = surrogate_pair_to_code_point(c, c2);
	      inc = 2;
	    }
	  else
	    return invalid_mb_sequence;
	}
      else if (is_low_surrogate(c))
	return invalid_mb_sequence;
      if (c <= maxcode)
	from += inc;
      return c;
    }

  template<typename C, bool A>
  bool
  write_utf16_code_point(range<C, A>& to, char32_t codepoint, codecvt_mode mode)
  {
    static_assert(sizeof(C) >= 2, "a code unit must be at least 16-bit");

    if (codepoint <= max_single_utf16_unit)
      {
	if (to.size() > 0)
	  {
	    to = adjust_byte_order(codepoint, mode);
	    return true;
	  }
      }
    else if (to.size() > 1)
      {
	// Algorithm from http://www.unicode.org/faq/utf_bom.html#utf16-4
	const char32_t LEAD_OFFSET = 0xD800 - (0x10000 >> 10);
	char16_t lead = LEAD_OFFSET + (codepoint >> 10);
	char16_t trail = 0xDC00 + (codepoint & 0x3FF);
	to = adjust_byte_order(lead, mode);
	to = adjust_byte_order(trail, mode);
	return true;
      }
    return false;
  }

  // utf8 -> ucs4
  template<typename C>
  codecvt_base::result
  ucs4_in(range<const C>& from, range<char32_t>& to,
          unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    read_utf8_bom(from, mode);
    while (from.size() && to.size())
      {
	const char32_t codepoint = read_utf8_code_point(from, maxcode);
	if (codepoint == incomplete_mb_character)
	  return codecvt_base::partial;
	if (codepoint > maxcode)
	  return codecvt_base::error;
	to = codepoint;
      }
    return from.size() ? codecvt_base::partial : codecvt_base::ok;
  }

  // ucs4 -> utf8
  template<typename C>
  codecvt_base::result
  ucs4_out(range<const char32_t>& from, range<C>& to,
           unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (!write_utf8_bom(to, mode))
      return codecvt_base::partial;
    while (from.size())
      {
	const char32_t c = from[0];
	if (c > maxcode)
	  return codecvt_base::error;
	if (!write_utf8_code_point(to, c))
	  return codecvt_base::partial;
	++from;
      }
    return codecvt_base::ok;
  }

  // utf16 -> ucs4
  codecvt_base::result
  ucs4_in(range<const char16_t, false>& from, range<char32_t>& to,
          unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    read_utf16_bom(from, mode);
    while (from.size() && to.size())
      {
	const char32_t codepoint = read_utf16_code_point(from, maxcode, mode);
	if (codepoint == incomplete_mb_character)
	  return codecvt_base::partial;
	if (codepoint > maxcode)
	  return codecvt_base::error;
	to = codepoint;
      }
    return from.size() ? codecvt_base::partial : codecvt_base::ok;
  }

  // ucs4 -> utf16
  codecvt_base::result
  ucs4_out(range<const char32_t>& from, range<char16_t, false>& to,
           unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (!write_utf16_bom(to, mode))
      return codecvt_base::partial;
    while (from.size())
      {
	const char32_t c = from[0];
	if (c > maxcode)
	  return codecvt_base::error;
	if (!write_utf16_code_point(to, c, mode))
	  return codecvt_base::partial;
	++from;
      }
    return codecvt_base::ok;
  }

  // Flag indicating whether to process UTF-16 or UCS2
  enum class surrogates { allowed, disallowed };

  // utf8 -> utf16 (or utf8 -> ucs2 if s == surrogates::disallowed)
  template<typename C8, typename C16>
  codecvt_base::result
  utf16_in(range<const C8>& from, range<C16>& to,
	   unsigned long maxcode = max_code_point, codecvt_mode mode = {},
	   surrogates s = surrogates::allowed)
  {
    read_utf8_bom(from, mode);
    while (from.size() && to.size())
      {
	auto orig = from;
	const char32_t codepoint = read_utf8_code_point(from, maxcode);
	if (codepoint == incomplete_mb_character)
	  {
	    if (s == surrogates::allowed)
	      return codecvt_base::partial;
	    else
	      return codecvt_base::error; // No surrogates in UCS2
	  }
	if (codepoint > maxcode)
	  return codecvt_base::error;
	if (!write_utf16_code_point(to, codepoint, mode))
	  {
	    from = orig; // rewind to previous position
	    return codecvt_base::partial;
	  }
      }
    return codecvt_base::ok;
  }

  // utf16 -> utf8 (or ucs2 -> utf8 if s == surrogates::disallowed)
  template<typename C16, typename C8>
  codecvt_base::result
  utf16_out(range<const C16>& from, range<C8>& to,
	    unsigned long maxcode = max_code_point, codecvt_mode mode = {},
	    surrogates s = surrogates::allowed)
  {
    if (!write_utf8_bom(to, mode))
      return codecvt_base::partial;
    while (from.size())
      {
	char32_t c = from[0];
	int inc = 1;
	if (is_high_surrogate(c))
	  {
	    if (s == surrogates::disallowed)
	      return codecvt_base::error; // No surrogates in UCS-2

	    if (from.size() < 2)
	      return codecvt_base::ok; // stop converting at this point

	    const char32_t c2 = from[1];
	    if (is_low_surrogate(c2))
	      {
		c = surrogate_pair_to_code_point(c, c2);
		inc = 2;
	      }
	    else
	      return codecvt_base::error;
	  }
	else if (is_low_surrogate(c))
	  return codecvt_base::error;
	if (c > maxcode)
	  return codecvt_base::error;
	if (!write_utf8_code_point(to, c))
	  return codecvt_base::partial;
	from += inc;
      }
    return codecvt_base::ok;
  }

  // return pos such that [begin,pos) is valid UTF-16 string no longer than max
  template<typename C>
  const C*
  utf16_span(const C* begin, const C* end, size_t max,
	     char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    range<const C> from{ begin, end };
    read_utf8_bom(from, mode);
    size_t count = 0;
    while (count+1 < max)
      {
	char32_t c = read_utf8_code_point(from, maxcode);
	if (c > maxcode)
	  return from.next;
	else if (c > max_single_utf16_unit)
	  ++count;
	++count;
      }
    if (count+1 == max) // take one more character if it fits in a single unit
      read_utf8_code_point(from, std::min(max_single_utf16_unit, maxcode));
    return from.next;
  }

  // utf8 -> ucs2
  template<typename C>
  codecvt_base::result
  ucs2_in(range<const C>& from, range<char16_t>& to,
	  char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    // UCS-2 only supports characters in the BMP, i.e. one UTF-16 code unit:
    maxcode = std::min(max_single_utf16_unit, maxcode);
    return utf16_in(from, to, maxcode, mode, surrogates::disallowed);
  }

  // ucs2 -> utf8
  template<typename C>
  codecvt_base::result
  ucs2_out(range<const char16_t>& from, range<C>& to,
	   char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    // UCS-2 only supports characters in the BMP, i.e. one UTF-16 code unit:
    maxcode = std::min(max_single_utf16_unit, maxcode);
    return utf16_out(from, to, maxcode, mode, surrogates::disallowed);
  }

  // ucs2 -> utf16
  codecvt_base::result
  ucs2_out(range<const char16_t>& from, range<char16_t, false>& to,
	   char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (!write_utf16_bom(to, mode))
      return codecvt_base::partial;
    while (from.size() && to.size())
      {
	char16_t c = from[0];
	if (is_high_surrogate(c))
	  return codecvt_base::error;
	if (c > maxcode)
	  return codecvt_base::error;
	to = adjust_byte_order(c, mode);
	++from;
      }
    return from.size() == 0 ? codecvt_base::ok : codecvt_base::partial;
  }

  // utf16 -> ucs2
  codecvt_base::result
  ucs2_in(range<const char16_t, false>& from, range<char16_t>& to,
	  char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    read_utf16_bom(from, mode);
    // UCS-2 only supports characters in the BMP, i.e. one UTF-16 code unit:
    maxcode = std::min(max_single_utf16_unit, maxcode);
    while (from.size() && to.size())
      {
	const char32_t c = read_utf16_code_point(from, maxcode, mode);
	if (c == incomplete_mb_character)
	  return codecvt_base::error; // UCS-2 only supports single units.
	if (c > maxcode)
	  return codecvt_base::error;
	to = c;
      }
    return from.size() == 0 ? codecvt_base::ok : codecvt_base::partial;
  }

  const char16_t*
  ucs2_span(range<const char16_t, false>& from, size_t max,
            char32_t maxcode, codecvt_mode mode)
  {
    read_utf16_bom(from, mode);
    // UCS-2 only supports characters in the BMP, i.e. one UTF-16 code unit:
    maxcode = std::min(max_single_utf16_unit, maxcode);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf16_code_point(from, maxcode, mode);
    return reinterpret_cast<const char16_t*>(from.next);
  }

  template<typename C>
  const C*
  ucs2_span(const C* begin, const C* end, size_t max,
            char32_t maxcode, codecvt_mode mode)
  {
    range<const C> from{ begin, end };
    read_utf8_bom(from, mode);
    // UCS-2 only supports characters in the BMP, i.e. one UTF-16 code unit:
    maxcode = std::min(max_single_utf16_unit, maxcode);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf8_code_point(from, maxcode);
    return from.next;
  }

  // return pos such that [begin,pos) is valid UCS-4 string no longer than max
  template<typename C>
  const C*
  ucs4_span(const C* begin, const C* end, size_t max,
            char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    range<const C> from{ begin, end };
    read_utf8_bom(from, mode);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf8_code_point(from, maxcode);
    return from.next;
  }

  // return pos such that [begin,pos) is valid UCS-4 string no longer than max
  const char16_t*
  ucs4_span(range<const char16_t, false>& from, size_t max,
            char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    read_utf16_bom(from, mode);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf16_code_point(from, maxcode, mode);
    return reinterpret_cast<const char16_t*>(from.next);
  }
}

// Define members of codecvt<char16_t, char, mbstate_t> specialization.
// Converts from UTF-8 to UTF-16.

locale::id codecvt<char16_t, char, mbstate_t>::id;

codecvt<char16_t, char, mbstate_t>::~codecvt() { }

codecvt_base::result
codecvt<char16_t, char, mbstate_t>::
do_out(state_type&,
       const intern_type* __from,
       const intern_type* __from_end, const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char16_t> from{ __from, __from_end };
  range<char> to{ __to, __to_end };
  auto res = utf16_out(from, to);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
codecvt<char16_t, char, mbstate_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv; // we don't use mbstate_t for the unicode facets
}

codecvt_base::result
codecvt<char16_t, char, mbstate_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
  range<char16_t> to{ __to, __to_end };
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  codecvt_mode mode = {};
#else
  codecvt_mode mode = little_endian;
#endif
  auto res = utf16_in(from, to, max_code_point, mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
codecvt<char16_t, char, mbstate_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
codecvt<char16_t, char, mbstate_t>::do_always_noconv() const throw()
{ return false; }

int
codecvt<char16_t, char, mbstate_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = utf16_span(__from, __end, __max);
  return __end - __from;
}

int
codecvt<char16_t, char, mbstate_t>::do_max_length() const throw()
{
  // A single character (one or two UTF-16 code units) requires
  // up to four UTF-8 code units.
  return 4;
}

// Define members of codecvt<char32_t, char, mbstate_t> specialization.
// Converts from UTF-8 to UTF-32 (aka UCS-4).

locale::id codecvt<char32_t, char, mbstate_t>::id;

codecvt<char32_t, char, mbstate_t>::~codecvt() { }

codecvt_base::result
codecvt<char32_t, char, mbstate_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char32_t> from{ __from, __from_end };
  range<char> to{ __to, __to_end };
  auto res = ucs4_out(from, to);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
codecvt<char32_t, char, mbstate_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
codecvt<char32_t, char, mbstate_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
  range<char32_t> to{ __to, __to_end };
  auto res = ucs4_in(from, to);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
codecvt<char32_t, char, mbstate_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
codecvt<char32_t, char, mbstate_t>::do_always_noconv() const throw()
{ return false; }

int
codecvt<char32_t, char, mbstate_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = ucs4_span(__from, __end, __max);
  return __end - __from;
}

int
codecvt<char32_t, char, mbstate_t>::do_max_length() const throw()
{
  // A single character (one UTF-32 code unit) requires
  // up to 4 UTF-8 code units.
  return 4;
}

#if defined(_GLIBCXX_USE_CHAR8_T)
// Define members of codecvt<char16_t, char8_t, mbstate_t> specialization.
// Converts from UTF-8 to UTF-16.

locale::id codecvt<char16_t, char8_t, mbstate_t>::id;

codecvt<char16_t, char8_t, mbstate_t>::~codecvt() { }

codecvt_base::result
codecvt<char16_t, char8_t, mbstate_t>::
do_out(state_type&,
       const intern_type* __from,
       const intern_type* __from_end, const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char16_t> from{ __from, __from_end };
  range<char8_t> to{ __to, __to_end };
  auto res = utf16_out(from, to);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
codecvt<char16_t, char8_t, mbstate_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv; // we don't use mbstate_t for the unicode facets
}

codecvt_base::result
codecvt<char16_t, char8_t, mbstate_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char8_t> from{ __from, __from_end };
  range<char16_t> to{ __to, __to_end };
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  codecvt_mode mode = {};
#else
  codecvt_mode mode = little_endian;
#endif
  auto res = utf16_in(from, to, max_code_point, mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
codecvt<char16_t, char8_t, mbstate_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
codecvt<char16_t, char8_t, mbstate_t>::do_always_noconv() const throw()
{ return false; }

int
codecvt<char16_t, char8_t, mbstate_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = utf16_span(__from, __end, __max);
  return __end - __from;
}

int
codecvt<char16_t, char8_t, mbstate_t>::do_max_length() const throw()
{
  // A single character (one or two UTF-16 code units) requires
  // up to four UTF-8 code units.
  return 4;
}

// Define members of codecvt<char32_t, char8_t, mbstate_t> specialization.
// Converts from UTF-8 to UTF-32 (aka UCS-4).

locale::id codecvt<char32_t, char8_t, mbstate_t>::id;

codecvt<char32_t, char8_t, mbstate_t>::~codecvt() { }

codecvt_base::result
codecvt<char32_t, char8_t, mbstate_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char32_t> from{ __from, __from_end };
  range<char8_t> to{ __to, __to_end };
  auto res = ucs4_out(from, to);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
codecvt<char32_t, char8_t, mbstate_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
codecvt<char32_t, char8_t, mbstate_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char8_t> from{ __from, __from_end };
  range<char32_t> to{ __to, __to_end };
  auto res = ucs4_in(from, to);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
codecvt<char32_t, char8_t, mbstate_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
codecvt<char32_t, char8_t, mbstate_t>::do_always_noconv() const throw()
{ return false; }

int
codecvt<char32_t, char8_t, mbstate_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = ucs4_span(__from, __end, __max);
  return __end - __from;
}

int
codecvt<char32_t, char8_t, mbstate_t>::do_max_length() const throw()
{
  // A single character (one UTF-32 code unit) requires
  // up to 4 UTF-8 code units.
  return 4;
}
#endif // _GLIBCXX_USE_CHAR8_T

// Define members of codecvt_utf8<char16_t> base class implementation.
// Converts from UTF-8 to UCS-2.

__codecvt_utf8_base<char16_t>::~__codecvt_utf8_base() { }

codecvt_base::result
__codecvt_utf8_base<char16_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char16_t> from{ __from, __from_end };
  range<char> to{ __to, __to_end };
  auto res = ucs2_out(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
__codecvt_utf8_base<char16_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf8_base<char16_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
  range<char16_t> to{ __to, __to_end };
  codecvt_mode mode = codecvt_mode(_M_mode & (consume_header|generate_header));
#if __BYTE_ORDER__ != __ORDER_BIG_ENDIAN__
  mode = codecvt_mode(mode | little_endian);
#endif
  auto res = ucs2_in(from, to, _M_maxcode, mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_base<char16_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
__codecvt_utf8_base<char16_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf8_base<char16_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = ucs2_span(__from, __end, __max, _M_maxcode, _M_mode);
  return __end - __from;
}

int
__codecvt_utf8_base<char16_t>::do_max_length() const throw()
{
  // A single UCS-2 character requires up to three UTF-8 code units.
  // (UCS-2 cannot represent characters that use four UTF-8 code units).
  int max = 3;
  if (_M_mode & consume_header)
    max += sizeof(utf8_bom);
  return max;
}

// Define members of codecvt_utf8<char32_t> base class implementation.
// Converts from UTF-8 to UTF-32 (aka UCS-4).

__codecvt_utf8_base<char32_t>::~__codecvt_utf8_base() { }

codecvt_base::result
__codecvt_utf8_base<char32_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char32_t> from{ __from, __from_end };
  range<char> to{ __to, __to_end };
  auto res = ucs4_out(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
__codecvt_utf8_base<char32_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf8_base<char32_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
  range<char32_t> to{ __to, __to_end };
  auto res = ucs4_in(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_base<char32_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
__codecvt_utf8_base<char32_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf8_base<char32_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = ucs4_span(__from, __end, __max, _M_maxcode, _M_mode);
  return __end - __from;
}

int
__codecvt_utf8_base<char32_t>::do_max_length() const throw()
{
  // A single UCS-4 character requires up to four UTF-8 code units.
  int max = 4;
  if (_M_mode & consume_header)
    max += sizeof(utf8_bom);
  return max;
}

#ifdef _GLIBCXX_USE_WCHAR_T

#if __SIZEOF_WCHAR_T__ == 2
static_assert(sizeof(wchar_t) == sizeof(char16_t), "");
#elif __SIZEOF_WCHAR_T__ == 4
static_assert(sizeof(wchar_t) == sizeof(char32_t), "");
#endif

// Define members of codecvt_utf8<wchar_t> base class implementation.
// Converts from UTF-8 to UCS-2 or UCS-4 depending on sizeof(wchar_t).

__codecvt_utf8_base<wchar_t>::~__codecvt_utf8_base() { }

codecvt_base::result
__codecvt_utf8_base<wchar_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<char> to{ __to, __to_end };
#if __SIZEOF_WCHAR_T__ == 2
  range<const char16_t> from{
    reinterpret_cast<const char16_t*>(__from),
    reinterpret_cast<const char16_t*>(__from_end)
  };
  auto res = ucs2_out(from, to, _M_maxcode, _M_mode);
#elif __SIZEOF_WCHAR_T__ == 4
  range<const char32_t> from{
    reinterpret_cast<const char32_t*>(__from),
    reinterpret_cast<const char32_t*>(__from_end)
  };
  auto res = ucs4_out(from, to, _M_maxcode, _M_mode);
#else
  return codecvt_base::error;
#endif
  __from_next = reinterpret_cast<const wchar_t*>(from.next);
  __to_next = to.next;
  return res;
}

codecvt_base::result
__codecvt_utf8_base<wchar_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf8_base<wchar_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
#if __SIZEOF_WCHAR_T__ == 2
  range<char16_t> to{
    reinterpret_cast<char16_t*>(__to),
    reinterpret_cast<char16_t*>(__to_end)
  };
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  codecvt_mode mode = {};
#else
  codecvt_mode mode = little_endian;
#endif
  auto res = ucs2_in(from, to, _M_maxcode, mode);
#elif __SIZEOF_WCHAR_T__ == 4
  range<char32_t> to{
    reinterpret_cast<char32_t*>(__to),
    reinterpret_cast<char32_t*>(__to_end)
  };
  auto res = ucs4_in(from, to, _M_maxcode, _M_mode);
#else
  return codecvt_base::error;
#endif
  __from_next = from.next;
  __to_next = reinterpret_cast<wchar_t*>(to.next);
  return res;
}

int
__codecvt_utf8_base<wchar_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
__codecvt_utf8_base<wchar_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf8_base<wchar_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
#if __SIZEOF_WCHAR_T__ == 2
  __end = ucs2_span(__from, __end, __max, _M_maxcode, _M_mode);
#elif __SIZEOF_WCHAR_T__ == 4
  __end = ucs4_span(__from, __end, __max, _M_maxcode, _M_mode);
#else
  __end = __from;
#endif
  return __end - __from;
}

int
__codecvt_utf8_base<wchar_t>::do_max_length() const throw()
{
#if __SIZEOF_WCHAR_T__ == 2
  int max = 3; // See __codecvt_utf8_base<char16_t>::do_max_length()
#else
  int max = 4; // See __codecvt_utf8_base<char32_t>::do_max_length()
#endif
  if (_M_mode & consume_header)
    max += sizeof(utf8_bom);
  return max;
}
#endif

// Define members of codecvt_utf16<char16_t> base class implementation.
// Converts from UTF-16 to UCS-2.

__codecvt_utf16_base<char16_t>::~__codecvt_utf16_base() { }

codecvt_base::result
__codecvt_utf16_base<char16_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char16_t> from{ __from, __from_end };
  range<char16_t, false> to{ __to, __to_end };
  auto res = ucs2_out(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = reinterpret_cast<char*>(to.next);
  return res;
}

codecvt_base::result
__codecvt_utf16_base<char16_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf16_base<char16_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char16_t, false> from{ __from, __from_end };
  range<char16_t> to{ __to, __to_end };
  auto res = ucs2_in(from, to, _M_maxcode, _M_mode);
  __from_next = reinterpret_cast<const char*>(from.next);
  __to_next = to.next;
  if (res == codecvt_base::ok && __from_next != __from_end)
    res = codecvt_base::error;
  return res;
}

int
__codecvt_utf16_base<char16_t>::do_encoding() const throw()
{ return 0; } // UTF-16 is not a fixed-width encoding

bool
__codecvt_utf16_base<char16_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf16_base<char16_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  range<const char16_t, false> from{ __from, __end };
  const char16_t* next = ucs2_span(from, __max, _M_maxcode, _M_mode);
  return reinterpret_cast<const char*>(next) - __from;
}

int
__codecvt_utf16_base<char16_t>::do_max_length() const throw()
{
  // A single UCS-2 character requires one UTF-16 code unit (so two chars).
  // (UCS-2 cannot represent characters that use multiple UTF-16 code units).
  int max = 2;
  if (_M_mode & consume_header)
    max += sizeof(utf16_bom);
  return max;
}

// Define members of codecvt_utf16<char32_t> base class implementation.
// Converts from UTF-16 to UTF-32 (aka UCS-4).

__codecvt_utf16_base<char32_t>::~__codecvt_utf16_base() { }

codecvt_base::result
__codecvt_utf16_base<char32_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char32_t> from{ __from, __from_end };
  range<char16_t, false> to{ __to, __to_end };
  auto res = ucs4_out(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = reinterpret_cast<char*>(to.next);
  return res;
}

codecvt_base::result
__codecvt_utf16_base<char32_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf16_base<char32_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char16_t, false> from{ __from, __from_end };
  range<char32_t> to{ __to, __to_end };
  auto res = ucs4_in(from, to, _M_maxcode, _M_mode);
  __from_next = reinterpret_cast<const char*>(from.next);
  __to_next = to.next;
  if (res == codecvt_base::ok && __from_next != __from_end)
    res = codecvt_base::error;
  return res;
}

int
__codecvt_utf16_base<char32_t>::do_encoding() const throw()
{ return 0; } // UTF-16 is not a fixed-width encoding

bool
__codecvt_utf16_base<char32_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf16_base<char32_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  range<const char16_t, false> from{ __from, __end };
  const char16_t* next = ucs4_span(from, __max, _M_maxcode, _M_mode);
  return reinterpret_cast<const char*>(next) - __from;
}

int
__codecvt_utf16_base<char32_t>::do_max_length() const throw()
{
  // A single UCS-4 character requires one or two UTF-16 code units
  // (so up to four chars).
  int max = 4;
  if (_M_mode & consume_header)
    max += sizeof(utf16_bom);
  return max;
}

#ifdef _GLIBCXX_USE_WCHAR_T
// Define members of codecvt_utf16<wchar_t> base class implementation.
// Converts from UTF-8 to UCS-2 or UCS-4 depending on sizeof(wchar_t).

__codecvt_utf16_base<wchar_t>::~__codecvt_utf16_base() { }

codecvt_base::result
__codecvt_utf16_base<wchar_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<char16_t, false> to{ __to, __to_end };
#if __SIZEOF_WCHAR_T__ == 2
  range<const char16_t> from{
    reinterpret_cast<const char16_t*>(__from),
    reinterpret_cast<const char16_t*>(__from_end),
  };
  auto res = ucs2_out(from, to, _M_maxcode, _M_mode);
#elif __SIZEOF_WCHAR_T__ == 4
  range<const char32_t> from{
    reinterpret_cast<const char32_t*>(__from),
    reinterpret_cast<const char32_t*>(__from_end),
  };
  auto res = ucs4_out(from, to, _M_maxcode, _M_mode);
#else
  return codecvt_base::error;
#endif
  __from_next = reinterpret_cast<const wchar_t*>(from.next);
  __to_next = reinterpret_cast<char*>(to.next);
  return res;
}

codecvt_base::result
__codecvt_utf16_base<wchar_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf16_base<wchar_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char16_t, false> from{ __from, __from_end };
#if __SIZEOF_WCHAR_T__ == 2
  range<char16_t> to{
    reinterpret_cast<char16_t*>(__to),
    reinterpret_cast<char16_t*>(__to_end),
  };
  auto res = ucs2_in(from, to, _M_maxcode, _M_mode);
#elif __SIZEOF_WCHAR_T__ == 4
  range<char32_t> to{
    reinterpret_cast<char32_t*>(__to),
    reinterpret_cast<char32_t*>(__to_end),
  };
  auto res = ucs4_in(from, to, _M_maxcode, _M_mode);
#else
  return codecvt_base::error;
#endif
  __from_next = reinterpret_cast<const char*>(from.next);
  __to_next = reinterpret_cast<wchar_t*>(to.next);
  if (res == codecvt_base::ok && __from_next != __from_end)
    res = codecvt_base::error;
  return res;
}

int
__codecvt_utf16_base<wchar_t>::do_encoding() const throw()
{ return 0; } // UTF-16 is not a fixed-width encoding

bool
__codecvt_utf16_base<wchar_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf16_base<wchar_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  range<const char16_t, false> from{ __from, __end };
#if __SIZEOF_WCHAR_T__ == 2
  const char16_t* next = ucs2_span(from, __max, _M_maxcode, _M_mode);
#elif __SIZEOF_WCHAR_T__ == 4
  const char16_t* next = ucs4_span(from, __max, _M_maxcode, _M_mode);
#endif
  return reinterpret_cast<const char*>(next) - __from;
}

int
__codecvt_utf16_base<wchar_t>::do_max_length() const throw()
{
#if __SIZEOF_WCHAR_T__ == 2
  int max = 2; // See __codecvt_utf16_base<char16_t>::do_max_length()
#else
  int max = 4; // See __codecvt_utf16_base<char32_t>::do_max_length()
#endif
  if (_M_mode & consume_header)
    max += sizeof(utf16_bom);
  return max;
}
#endif

// Define members of codecvt_utf8_utf16<char16_t> base class implementation.
// Converts from UTF-8 to UTF-16.

__codecvt_utf8_utf16_base<char16_t>::~__codecvt_utf8_utf16_base() { }

codecvt_base::result
__codecvt_utf8_utf16_base<char16_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char16_t> from{ __from, __from_end };
  range<char> to{ __to, __to_end };
  auto res = utf16_out(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
__codecvt_utf8_utf16_base<char16_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf8_utf16_base<char16_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
  range<char16_t> to{ __to, __to_end };
  codecvt_mode mode = codecvt_mode(_M_mode & (consume_header|generate_header));
#if __BYTE_ORDER__ != __ORDER_BIG_ENDIAN__
  mode = codecvt_mode(mode | little_endian);
#endif
  auto res = utf16_in(from, to, _M_maxcode, mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_utf16_base<char16_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
__codecvt_utf8_utf16_base<char16_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf8_utf16_base<char16_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = utf16_span(__from, __end, __max, _M_maxcode, _M_mode);
  return __end - __from;
}

int
__codecvt_utf8_utf16_base<char16_t>::do_max_length() const throw()
{
  // A single character can be 1 or 2 UTF-16 code units,
  // requiring up to 4 UTF-8 code units.
  int max = 4;
  if (_M_mode & consume_header)
    max += sizeof(utf8_bom);
  return max;
}

// Define members of codecvt_utf8_utf16<char32_t> base class implementation.
// Converts from UTF-8 to UTF-16.

__codecvt_utf8_utf16_base<char32_t>::~__codecvt_utf8_utf16_base() { }

codecvt_base::result
__codecvt_utf8_utf16_base<char32_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const char32_t> from{ __from, __from_end };
  range<char> to{ __to, __to_end };
  auto res = utf16_out(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
__codecvt_utf8_utf16_base<char32_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf8_utf16_base<char32_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
  range<char32_t> to{ __to, __to_end };
  codecvt_mode mode = codecvt_mode(_M_mode & (consume_header|generate_header));
#if __BYTE_ORDER__ != __ORDER_BIG_ENDIAN__
  mode = codecvt_mode(mode | little_endian);
#endif
  auto res = utf16_in(from, to, _M_maxcode, mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_utf16_base<char32_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
__codecvt_utf8_utf16_base<char32_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf8_utf16_base<char32_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = utf16_span(__from, __end, __max, _M_maxcode, _M_mode);
  return __end - __from;
}

int
__codecvt_utf8_utf16_base<char32_t>::do_max_length() const throw()
{
  // A single character can be 1 or 2 UTF-16 code units,
  // requiring up to 4 UTF-8 code units.
  int max = 4;
  if (_M_mode & consume_header)
    max += sizeof(utf8_bom);
  return max;
}

#ifdef _GLIBCXX_USE_WCHAR_T
// Define members of codecvt_utf8_utf16<wchar_t> base class implementation.
// Converts from UTF-8 to UTF-16.

__codecvt_utf8_utf16_base<wchar_t>::~__codecvt_utf8_utf16_base() { }

codecvt_base::result
__codecvt_utf8_utf16_base<wchar_t>::
do_out(state_type&, const intern_type* __from, const intern_type* __from_end,
       const intern_type*& __from_next,
       extern_type* __to, extern_type* __to_end,
       extern_type*& __to_next) const
{
  range<const wchar_t> from{ __from, __from_end };
  range<char> to{ __to, __to_end };
  auto res = utf16_out(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

codecvt_base::result
__codecvt_utf8_utf16_base<wchar_t>::
do_unshift(state_type&, extern_type* __to, extern_type*,
	   extern_type*& __to_next) const
{
  __to_next = __to;
  return noconv;
}

codecvt_base::result
__codecvt_utf8_utf16_base<wchar_t>::
do_in(state_type&, const extern_type* __from, const extern_type* __from_end,
      const extern_type*& __from_next,
      intern_type* __to, intern_type* __to_end,
      intern_type*& __to_next) const
{
  range<const char> from{ __from, __from_end };
  range<wchar_t> to{ __to, __to_end };
  codecvt_mode mode = codecvt_mode(_M_mode & (consume_header|generate_header));
#if __BYTE_ORDER__ != __ORDER_BIG_ENDIAN__
  mode = codecvt_mode(mode | little_endian);
#endif
  auto res = utf16_in(from, to, _M_maxcode, mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_utf16_base<wchar_t>::do_encoding() const throw()
{ return 0; } // UTF-8 is not a fixed-width encoding

bool
__codecvt_utf8_utf16_base<wchar_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf8_utf16_base<wchar_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  __end = utf16_span(__from, __end, __max, _M_maxcode, _M_mode);
  return __end - __from;
}

int
__codecvt_utf8_utf16_base<wchar_t>::do_max_length() const throw()
{
  // A single character can be 1 or 2 UTF-16 code units,
  // requiring up to 4 UTF-8 code units.
  int max = 4;
  if (_M_mode & consume_header)
    max += sizeof(utf8_bom);
  return max;
}
#endif

inline template class __codecvt_abstract_base<char16_t, char, mbstate_t>;
inline template class __codecvt_abstract_base<char32_t, char, mbstate_t>;
template class codecvt_byname<char16_t, char, mbstate_t>;
template class codecvt_byname<char32_t, char, mbstate_t>;

#if defined(_GLIBCXX_USE_CHAR8_T)
inline template class __codecvt_abstract_base<char16_t, char8_t, mbstate_t>;
inline template class __codecvt_abstract_base<char32_t, char8_t, mbstate_t>;
template class codecvt_byname<char16_t, char8_t, mbstate_t>;
template class codecvt_byname<char32_t, char8_t, mbstate_t>;
#endif

_GLIBCXX_END_NAMESPACE_VERSION
}

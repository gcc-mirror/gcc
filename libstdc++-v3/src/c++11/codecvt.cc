// Locale support (codecvt) -*- C++ -*-

// Copyright (C) 2015 Free Software Foundation, Inc.
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
#include <bits/stl_algobase.h>	// std::max

#ifdef _GLIBCXX_USE_C99_STDINT_TR1
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace
{
  // Largest code point that fits in a single UTF-16 code unit.
  const char32_t max_single_utf16_unit = 0xFFFF;

  const char32_t max_code_point = 0x10FFFF;

  // The functions below rely on maxcode < incomplete_mb_character
  // (which is enforced by the codecvt_utf* classes on construction).
  const char32_t incomplete_mb_character = char32_t(-2);
  const char32_t invalid_mb_sequence = char32_t(-1);

  template<typename Elem>
    struct range
    {
      Elem* next;
      Elem* end;

      Elem operator*() const { return *next; }

      range& operator++() { ++next; return *this; }

      size_t size() const { return end - next; }
    };

  // Multibyte sequences can have "header" consisting of Byte Order Mark
  const unsigned char utf8_bom[3] = { 0xEF, 0xBB, 0xBF };
  const unsigned char utf16_bom[4] = { 0xFE, 0xFF };
  const unsigned char utf16le_bom[4] = { 0xFF, 0xFE };

  template<size_t N>
    inline bool
    write_bom(range<char>& to, const unsigned char (&bom)[N])
    {
      if (to.size() < N)
	return false;
      memcpy(to.next, bom, N);
      to.next += N;
      return true;
    }

  // If generate_header is set in mode write out UTF-8 BOM.
  bool
  write_utf8_bom(range<char>& to, codecvt_mode mode)
  {
    if (mode & generate_header)
      return write_bom(to, utf8_bom);
    return true;
  }

  // If generate_header is set in mode write out the UTF-16 BOM indicated
  // by whether little_endian is set in mode.
  bool
  write_utf16_bom(range<char16_t>& to, codecvt_mode mode)
  {
    if (mode & generate_header)
    {
      if (!to.size())
	return false;
      auto* bom = (mode & little_endian) ? utf16le_bom : utf16_bom;
      std::memcpy(to.next, bom, 2);
      ++to.next;
    }
    return true;
  }

  template<size_t N>
    inline bool
    read_bom(range<const char>& from, const unsigned char (&bom)[N])
    {
      if (from.size() >= N && !memcmp(from.next, bom, N))
	{
	  from.next += N;
	  return true;
	}
      return false;
    }

  // If consume_header is set in mode update from.next to after any BOM.
  void
  read_utf8_bom(range<const char>& from, codecvt_mode mode)
  {
    if (mode & consume_header)
      read_bom(from, utf8_bom);
  }

  // If consume_header is set in mode update from.next to after any BOM.
  // Return little_endian iff the UTF-16LE BOM was present.
  codecvt_mode
  read_utf16_bom(range<const char16_t>& from, codecvt_mode mode)
  {
    if (mode & consume_header && from.size())
      {
	if (*from.next == 0xFEFF)
	  ++from.next;
	else if (*from.next == 0xFFFE)
	  {
	    ++from.next;
	    return little_endian;
	  }
      }
    return {};
  }

  // Read a codepoint from a UTF-8 multibyte sequence.
  // Updates from.next if the codepoint is not greater than maxcode.
  // Returns invalid_mb_sequence, incomplete_mb_character or the code point.
  char32_t
  read_utf8_code_point(range<const char>& from, unsigned long maxcode)
  {
    const size_t avail = from.size();
    if (avail == 0)
      return incomplete_mb_character;
    unsigned char c1 = from.next[0];
    // https://en.wikipedia.org/wiki/UTF-8#Sample_code
    if (c1 < 0x80)
    {
      ++from.next;
      return c1;
    }
    else if (c1 < 0xC2) // continuation or overlong 2-byte sequence
      return invalid_mb_sequence;
    else if (c1 < 0xE0) // 2-byte sequence
    {
      if (avail < 2)
	return incomplete_mb_character;
      unsigned char c2 = from.next[1];
      if ((c2 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      char32_t c = (c1 << 6) + c2 - 0x3080;
      if (c <= maxcode)
	from.next += 2;
      return c;
    }
    else if (c1 < 0xF0) // 3-byte sequence
    {
      if (avail < 3)
	return incomplete_mb_character;
      unsigned char c2 = from.next[1];
      if ((c2 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      if (c1 == 0xE0 && c2 < 0xA0) // overlong
	return invalid_mb_sequence;
      unsigned char c3 = from.next[2];
      if ((c3 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      char32_t c = (c1 << 12) + (c2 << 6) + c3 - 0xE2080;
      if (c <= maxcode)
	from.next += 3;
      return c;
    }
    else if (c1 < 0xF5) // 4-byte sequence
    {
      if (avail < 4)
	return incomplete_mb_character;
      unsigned char c2 = from.next[1];
      if ((c2 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      if (c1 == 0xF0 && c2 < 0x90) // overlong
	return invalid_mb_sequence;
      if (c1 == 0xF4 && c2 >= 0x90) // > U+10FFFF
      return invalid_mb_sequence;
      unsigned char c3 = from.next[2];
      if ((c3 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      unsigned char c4 = from.next[3];
      if ((c4 & 0xC0) != 0x80)
	return invalid_mb_sequence;
      char32_t c = (c1 << 18) + (c2 << 12) + (c3 << 6) + c4 - 0x3C82080;
      if (c <= maxcode)
	from.next += 4;
      return c;
    }
    else // > U+10FFFF
      return invalid_mb_sequence;
  }

  bool
  write_utf8_code_point(range<char>& to, char32_t code_point)
  {
    if (code_point < 0x80)
      {
	if (to.size() < 1)
	  return false;
	*to.next++ = code_point;
      }
    else if (code_point <= 0x7FF)
      {
	if (to.size() < 2)
	  return false;
	*to.next++ = (code_point >> 6) + 0xC0;
	*to.next++ = (code_point & 0x3F) + 0x80;
      }
    else if (code_point <= 0xFFFF)
      {
	if (to.size() < 3)
	  return false;
	*to.next++ = (code_point >> 12) + 0xE0;
	*to.next++ = ((code_point >> 6) & 0x3F) + 0x80;
	*to.next++ = (code_point & 0x3F) + 0x80;
      }
    else if (code_point <= 0x10FFFF)
      {
	if (to.size() < 4)
	  return false;
	*to.next++ = (code_point >> 18) + 0xF0;
	*to.next++ = ((code_point >> 12) & 0x3F) + 0x80;
	*to.next++ = ((code_point >> 6) & 0x3F) + 0x80;
	*to.next++ = (code_point & 0x3F) + 0x80;
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
  char32_t
  read_utf16_code_point(range<const char16_t>& from, unsigned long maxcode,
			codecvt_mode mode)
  {
    const size_t avail = from.size();
    if (avail == 0)
      return incomplete_mb_character;
    int inc = 1;
    char32_t c = adjust_byte_order(from.next[0], mode);
    if (is_high_surrogate(c))
      {
	if (avail < 2)
	  return incomplete_mb_character;
	const char16_t c2 = adjust_byte_order(from.next[1], mode);
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
      from.next += inc;
    return c;
  }

  template<typename C>
  bool
  write_utf16_code_point(range<C>& to, char32_t codepoint, codecvt_mode mode)
  {
    static_assert(sizeof(C) >= 2, "a code unit must be at least 16-bit");

    if (codepoint < max_single_utf16_unit)
      {
	if (to.size() > 0)
	  {
	    *to.next = adjust_byte_order(codepoint, mode);
	    ++to.next;
	    return true;
	  }
      }
    else if (to.size() > 1)
      {
	// Algorithm from http://www.unicode.org/faq/utf_bom.html#utf16-4
	const char32_t LEAD_OFFSET = 0xD800 - (0x10000 >> 10);
	char16_t lead = LEAD_OFFSET + (codepoint >> 10);
	char16_t trail = 0xDC00 + (codepoint & 0x3FF);
	to.next[0] = adjust_byte_order(lead, mode);
	to.next[1] = adjust_byte_order(trail, mode);
	to.next += 2;
	return true;
      }
    return false;
  }

  // utf8 -> ucs4
  codecvt_base::result
  ucs4_in(range<const char>& from, range<char32_t>& to,
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
	*to.next++ = codepoint;
      }
    return from.size() ? codecvt_base::partial : codecvt_base::ok;
  }

  // ucs4 -> utf8
  codecvt_base::result
  ucs4_out(range<const char32_t>& from, range<char>& to,
           unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (!write_utf8_bom(to, mode))
      return codecvt_base::partial;
    while (from.size())
      {
	const char32_t c = from.next[0];
	if (c > maxcode)
	  return codecvt_base::error;
	if (!write_utf8_code_point(to, c))
	  return codecvt_base::partial;
	++from.next;
      }
    return codecvt_base::ok;
  }

  // utf16 -> ucs4
  codecvt_base::result
  ucs4_in(range<const char16_t>& from, range<char32_t>& to,
          unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (read_utf16_bom(from, mode) == little_endian)
      mode = codecvt_mode(mode & little_endian);
    while (from.size() && to.size())
      {
	const char32_t codepoint = read_utf16_code_point(from, maxcode, mode);
	if (codepoint == incomplete_mb_character)
	  return codecvt_base::partial;
	if (codepoint > maxcode)
	  return codecvt_base::error;
	*to.next++ = codepoint;
      }
    return from.size() ? codecvt_base::partial : codecvt_base::ok;
  }

  // ucs4 -> utf16
  codecvt_base::result
  ucs4_out(range<const char32_t>& from, range<char16_t>& to,
           unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (!write_utf16_bom(to, mode))
      return codecvt_base::partial;
    while (from.size())
      {
	const char32_t c = from.next[0];
	if (c > maxcode)
	  return codecvt_base::error;
	if (!write_utf16_code_point(to, c, mode))
	  return codecvt_base::partial;
	++from.next;
      }
    return codecvt_base::ok;
  }

  // utf8 -> utf16
  template<typename C>
  codecvt_base::result
  utf16_in(range<const char>& from, range<C>& to,
           unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    read_utf8_bom(from, mode);
    while (from.size() && to.size())
      {
	const char* const first = from.next;
	const char32_t codepoint = read_utf8_code_point(from, maxcode);
	if (codepoint == incomplete_mb_character)
	  return codecvt_base::partial;
	if (codepoint > maxcode)
	  return codecvt_base::error;
	if (!write_utf16_code_point(to, codepoint, mode))
	  {
	    from.next = first;
	    return codecvt_base::partial;
	  }
      }
    return codecvt_base::ok;
  }

  // utf16 -> utf8
  template<typename C>
  codecvt_base::result
  utf16_out(range<const C>& from, range<char>& to,
            unsigned long maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (!write_utf8_bom(to, mode))
      return codecvt_base::partial;
    while (from.size())
      {
	char32_t c = from.next[0];
	int inc = 1;
	if (is_high_surrogate(c))
	  {
	    if (from.size() < 2)
	      return codecvt_base::ok; // stop converting at this point

	    const char32_t c2 = from.next[1];
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
	from.next += inc;
      }
    return codecvt_base::ok;
  }

  // return pos such that [begin,pos) is valid UTF-16 string no longer than max
  const char*
  utf16_span(const char* begin, const char* end, size_t max,
	     char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    range<const char> from{ begin, end };
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
      read_utf8_code_point(from, std::max(max_single_utf16_unit, maxcode));
    return from.next;
  }

  // utf8 -> ucs2
  codecvt_base::result
  ucs2_in(range<const char>& from, range<char16_t>& to,
	  char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    return utf16_in(from, to, std::max(max_single_utf16_unit, maxcode), mode);
  }

  // ucs2 -> utf8
  codecvt_base::result
  ucs2_out(range<const char16_t>& from, range<char>& to,
	   char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    return utf16_out(from, to, std::max(max_single_utf16_unit, maxcode), mode);
  }

  // ucs2 -> utf16
  codecvt_base::result
  ucs2_out(range<const char16_t>& from, range<char16_t>& to,
	   char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (!write_utf16_bom(to, mode))
      return codecvt_base::partial;
    while (from.size() && to.size())
      {
	char16_t c = from.next[0];
	if (is_high_surrogate(c))
	  return codecvt_base::error;
	if (c > maxcode)
	  return codecvt_base::error;
	*to.next++ = adjust_byte_order(c, mode);
	++from.next;
      }
    return from.size() == 0 ? codecvt_base::ok : codecvt_base::partial;
  }

  // utf16 -> ucs2
  codecvt_base::result
  ucs2_in(range<const char16_t>& from, range<char16_t>& to,
	  char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    if (read_utf16_bom(from, mode) == little_endian)
      mode = codecvt_mode(mode & little_endian);
    maxcode = std::max(max_single_utf16_unit, maxcode);
    while (from.size() && to.size())
      {
	const char32_t c = read_utf16_code_point(from, maxcode, mode);
	if (c == incomplete_mb_character)
	  return codecvt_base::partial;
	if (c > maxcode)
	  return codecvt_base::error;
	*to.next++ = c;
      }
    return from.size() == 0 ? codecvt_base::ok : codecvt_base::partial;
  }

  const char16_t*
  ucs2_span(const char16_t* begin, const char16_t* end, size_t max,
            char32_t maxcode, codecvt_mode mode)
  {
    range<const char16_t> from{ begin, end };
    if (read_utf16_bom(from, mode) == little_endian)
      mode = codecvt_mode(mode & little_endian);
    maxcode = std::max(max_single_utf16_unit, maxcode);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf16_code_point(from, maxcode, mode);
    return from.next;
  }

  const char*
  ucs2_span(const char* begin, const char* end, size_t max,
            char32_t maxcode, codecvt_mode mode)
  {
    range<const char> from{ begin, end };
    read_utf8_bom(from, mode);
    maxcode = std::max(max_single_utf16_unit, maxcode);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf8_code_point(from, maxcode);
    return from.next;
  }

  // return pos such that [begin,pos) is valid UCS-4 string no longer than max
  const char*
  ucs4_span(const char* begin, const char* end, size_t max,
            char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    range<const char> from{ begin, end };
    read_utf8_bom(from, mode);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf8_code_point(from, maxcode);
    return from.next;
  }

  // return pos such that [begin,pos) is valid UCS-4 string no longer than max
  const char16_t*
  ucs4_span(const char16_t* begin, const char16_t* end, size_t max,
            char32_t maxcode = max_code_point, codecvt_mode mode = {})
  {
    range<const char16_t> from{ begin, end };
    if (read_utf16_bom(from, mode) == little_endian)
      mode = codecvt_mode(mode & little_endian);
    char32_t c = 0;
    while (max-- && c <= maxcode)
      c = read_utf16_code_point(from, maxcode, mode);
    return from.next;
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
{ return 0; }

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
  // Any valid UTF-8 sequence of 3 bytes fits in a single 16-bit code unit,
  // whereas 4 byte sequences require two 16-bit code units.
  return 3;
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
{ return 0; }

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
{ return 4; }

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
  auto res = ucs2_in(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_base<char16_t>::do_encoding() const throw()
{ return 0; }

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
{ return 3; }

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
{ return 0; }

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
{ return 4; }

#ifdef _GLIBCXX_USE_WCHAR_T
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
  auto res = ucs2_in(from, to, _M_maxcode, _M_mode);
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
{ return 0; }

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
{ return 4; }
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
  range<char16_t> to{
    reinterpret_cast<char16_t*>(__to),
    reinterpret_cast<char16_t*>(__to_end)
  };
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
  range<const char16_t> from{
    reinterpret_cast<const char16_t*>(__from),
    reinterpret_cast<const char16_t*>(__from_end)
  };
  range<char16_t> to{ __to, __to_end };
  auto res = ucs2_in(from, to, _M_maxcode, _M_mode);
  __from_next = reinterpret_cast<const char*>(from.next);
  __to_next = to.next;
  return res;
}

int
__codecvt_utf16_base<char16_t>::do_encoding() const throw()
{ return 1; }

bool
__codecvt_utf16_base<char16_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf16_base<char16_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  auto next = reinterpret_cast<const char16_t*>(__from);
  next = ucs2_span(next, reinterpret_cast<const char16_t*>(__end), __max,
		   _M_maxcode, _M_mode);
  return reinterpret_cast<const char*>(next) - __from;
}

int
__codecvt_utf16_base<char16_t>::do_max_length() const throw()
{ return 3; }

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
  range<char16_t> to{
    reinterpret_cast<char16_t*>(__to),
    reinterpret_cast<char16_t*>(__to_end)
  };
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
  range<const char16_t> from{
    reinterpret_cast<const char16_t*>(__from),
    reinterpret_cast<const char16_t*>(__from_end)
  };
  range<char32_t> to{ __to, __to_end };
  auto res = ucs4_in(from, to, _M_maxcode, _M_mode);
  __from_next = reinterpret_cast<const char*>(from.next);
  __to_next = to.next;
  return res;
}

int
__codecvt_utf16_base<char32_t>::do_encoding() const throw()
{ return 0; }

bool
__codecvt_utf16_base<char32_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf16_base<char32_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  auto next = reinterpret_cast<const char16_t*>(__from);
  next = ucs4_span(next, reinterpret_cast<const char16_t*>(__end), __max,
		   _M_maxcode, _M_mode);
  return reinterpret_cast<const char*>(next) - __from;
}

int
__codecvt_utf16_base<char32_t>::do_max_length() const throw()
{ return 4; }

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
  range<const char> from{ __from, __from_end };
#if __SIZEOF_WCHAR_T__ == 2
  range<char16_t> to{
    reinterpret_cast<char16_t*>(__to),
    reinterpret_cast<char16_t*>(__to_end)
  };
  auto res = ucs2_in(from, to, _M_maxcode, _M_mode);
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
__codecvt_utf16_base<wchar_t>::do_encoding() const throw()
{ return 0; }

bool
__codecvt_utf16_base<wchar_t>::do_always_noconv() const throw()
{ return false; }

int
__codecvt_utf16_base<wchar_t>::
do_length(state_type&, const extern_type* __from,
	  const extern_type* __end, size_t __max) const
{
  auto next = reinterpret_cast<const char16_t*>(__from);
#if __SIZEOF_WCHAR_T__ == 2
  next = ucs2_span(next, reinterpret_cast<const char16_t*>(__end), __max,
		   _M_maxcode, _M_mode);
#elif __SIZEOF_WCHAR_T__ == 4
  next = ucs4_span(next, reinterpret_cast<const char16_t*>(__end), __max,
		   _M_maxcode, _M_mode);
#endif
  return reinterpret_cast<const char*>(next) - __from;
}

int
__codecvt_utf16_base<wchar_t>::do_max_length() const throw()
{ return 4; }
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
  codecvt_mode mode = codecvt_mode(_M_mode | (consume_header|generate_header));
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
{ return 0; }

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
  // Any valid UTF-8 sequence of 3 bytes fits in a single 16-bit code unit,
  // whereas 4 byte sequences require two 16-bit code units.
  return 3;
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
  auto res = utf16_in(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_utf16_base<char32_t>::do_encoding() const throw()
{ return 0; }

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
  // Any valid UTF-8 sequence of 3 bytes fits in a single 16-bit code unit,
  // whereas 4 byte sequences require two 16-bit code units.
  return 3;
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
  auto res = utf16_in(from, to, _M_maxcode, _M_mode);
  __from_next = from.next;
  __to_next = to.next;
  return res;
}

int
__codecvt_utf8_utf16_base<wchar_t>::do_encoding() const throw()
{ return 0; }

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
  // Any valid UTF-8 sequence of 3 bytes fits in a single 16-bit code unit,
  // whereas 4 byte sequences require two 16-bit code units.
  return 3;
}
#endif

inline template class __codecvt_abstract_base<char16_t, char, mbstate_t>;
inline template class __codecvt_abstract_base<char32_t, char, mbstate_t>;
template class codecvt_byname<char16_t, char, mbstate_t>;
template class codecvt_byname<char32_t, char, mbstate_t>;

_GLIBCXX_END_NAMESPACE_VERSION
}
#endif // _GLIBCXX_USE_C99_STDINT_TR1

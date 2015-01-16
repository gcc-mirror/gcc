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

#include <bits/locale_classes.h>
#include <bits/codecvt.h>
#include <bits/stl_algobase.h>	// std::max
#include <cstring>		// std::memcpy, std::memcmp

#ifdef _GLIBCXX_USE_C99_STDINT_TR1
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace
{
  // Largest code point that fits in a single UTF-16 code unit.
  const char32_t max_single_utf16_unit = 0xFFFF;
  const char32_t max_code_point = 0x10FFFF;

  template<typename Elem>
    struct range
    {
      Elem* next;
      Elem* end;

      Elem operator*() const { return *next; }

      range& operator++() { ++next; return *this; }

      size_t size() const { return end - next; }
    };

  char32_t
  read_utf8_code_point(range<const char>& from, unsigned long maxcode)
  {
    size_t avail = from.size();
    if (avail == 0)
      return -1;
    unsigned char c1 = from.next[0];
    // https://en.wikipedia.org/wiki/UTF-8#Sample_code
    if (c1 < 0x80)
    {
      ++from.next;
      return c1;
    }
    else if (c1 < 0xC2) // continuation or overlong 2-byte sequence
      return -1;
    else if (c1 < 0xE0) // 2-byte sequence
    {
      if (avail < 2)
	return -1;
      unsigned char c2 = from.next[1];
      if ((c2 & 0xC0) != 0x80)
	return -1;
      char32_t c = (c1 << 6) + c2 - 0x3080;
      if (c > maxcode)
	return -1;
      from.next += 2;
      return c;
    }
    else if (c1 < 0xF0) // 3-byte sequence
    {
      if (avail < 3)
	return -1;
      unsigned char c2 = from.next[1];
      if ((c2 & 0xC0) != 0x80)
	return -1;
      if (c1 == 0xE0 && c2 < 0xA0) // overlong
	return -1;
      unsigned char c3 = from.next[2];
      if ((c3 & 0xC0) != 0x80)
	return -1;
      char32_t c = (c1 << 12) + (c2 << 6) + c3 - 0xE2080;
      if (c > maxcode)
	return -1;
      from.next += 3;
      return c;
    }
    else if (c1 < 0xF5) // 4-byte sequence
    {
      if (avail < 4)
	return -1;
      unsigned char c2 = from.next[1];
      if ((c2 & 0xC0) != 0x80)
	return -1;
      if (c1 == 0xF0 && c2 < 0x90) // overlong
	return -1;
      if (c1 == 0xF4 && c2 >= 0x90) // > U+10FFFF
      return -1;
      unsigned char c3 = from.next[2];
      if ((c3 & 0xC0) != 0x80)
	return -1;
      unsigned char c4 = from.next[3];
      if ((c4 & 0xC0) != 0x80)
	return -1;
      char32_t c = (c1 << 18) + (c2 << 12) + (c3 << 6) + c4 - 0x3C82080;
      if (c > maxcode)
	return -1;
      from.next += 4;
      return c;
    }
    else // > U+10FFFF
      return -1;
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

  bool
  write_utf16_code_point(range<char16_t>& to, char32_t codepoint)
  {
    if (codepoint < max_single_utf16_unit)
      {
	if (to.size() > 0)
	  {
	    *to.next = codepoint;
	    ++to.next;
	    return true;
	  }
      }
    else if (to.size() > 1)
      {
	// Algorithm from http://www.unicode.org/faq/utf_bom.html#utf16-4
	const char32_t LEAD_OFFSET = 0xD800 - (0x10000 >> 10);
	const char32_t SURROGATE_OFFSET = 0x10000 - (0xD800 << 10) - 0xDC00;
	char16_t lead = LEAD_OFFSET + (codepoint >> 10);
	char16_t trail = 0xDC00 + (codepoint & 0x3FF);
	char32_t utf16bytes = (lead << 10) + trail + SURROGATE_OFFSET;

	to.next[0] = utf16bytes >> 16;
	to.next[1] = utf16bytes & 0xFFFF;
	to.next += 2;
	return true;
      }
    return false;
  }

  // utf8 -> ucs4
  codecvt_base::result
  ucs4_in(range<const char>& from, range<char32_t>& to,
          unsigned long maxcode = max_code_point)
  {
    while (from.size() && to.size())
      {
	const char32_t codepoint = read_utf8_code_point(from, maxcode);
	if (codepoint == char32_t(-1) || codepoint > maxcode)
	  return codecvt_base::error;
	*to.next++ = codepoint;
      }
    return from.size() ? codecvt_base::partial : codecvt_base::ok;
  }

  // ucs4 -> utf8
  codecvt_base::result
  ucs4_out(range<const char32_t>& from, range<char>& to,
           unsigned long maxcode = max_code_point)
  {
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

  // utf8 -> utf16
  codecvt_base::result
  utf16_in(range<const char>& from, range<char16_t>& to,
           unsigned long maxcode = max_code_point)
  {
    while (from.size() && to.size())
      {
	const char* first = from.next;
	if ((unsigned char)*first >= 0xF0 && to.size() < 2)
	  return codecvt_base::partial;
	const char32_t codepoint = read_utf8_code_point(from, maxcode);
	if (codepoint == char32_t(-1) || codepoint > maxcode)
	  return codecvt_base::error;
	if (!write_utf16_code_point(to, codepoint))
	  {
	    from.next = first;
	    return codecvt_base::partial;
	  }
      }
    return codecvt_base::ok;
  }

  // utf16 -> utf8
  codecvt_base::result
  utf16_out(range<const char16_t>& from, range<char>& to,
            unsigned long maxcode = max_code_point)
  {
    while (from.size())
      {
	char32_t c = from.next[0];
	int inc = 1;
	if (c >= 0xD800 && c < 0xDBFF) // start of surrogate pair
	  {
	    if (from.size() < 2)
	      return codecvt_base::ok; // stop converting at this point

	    const char32_t c2 = from.next[1];
	    if (c2 >= 0xDC00 && c2 <= 0xDFFF)
	      {
		inc = 2;
		c = (c << 10) + c2 - 0x35FDC00;
	      }
	    else
	      return codecvt_base::error;
	  }
	if (c > maxcode)
	  return codecvt_base::error;
	if (!write_utf8_code_point(to, c))
	  return codecvt_base::partial;
	from.next += inc;
      }
    return codecvt_base::ok;
  }

  // return pos such that [begin,pos) is valid UTF-16 string no longer than max
  int
  utf16_len(const char* begin, const char* end, size_t max,
            char32_t maxcode = max_code_point)
  {
    range<const char> from{ begin, end };
    size_t count = 0;
    while (count+1 < max)
      {
	char32_t c = read_utf8_code_point(from, maxcode);
	if (c == char32_t(-1))
	  break;
	else if (c > max_single_utf16_unit)
	  ++count;
	++count;
      }
    if (count+1 == max) // take one more character if it fits in a single unit
      read_utf8_code_point(from, std::max(max_single_utf16_unit, maxcode));
    return from.next - begin;
  }

  // return pos such that [begin,pos) is valid UCS-4 string no longer than max
  int
  ucs4_len(const char* begin, const char* end, size_t max,
            char32_t maxcode = max_code_point)
  {
    range<const char> from{ begin, end };
    size_t count = 0;
    while (count < max)
      {
	char32_t c = read_utf8_code_point(from, maxcode);
	if (c == char32_t(-1))
	  break;
	++count;
      }
    return from.next - begin;
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
  auto res = utf16_in(from, to);
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
  return utf16_len(__from, __end, __max);
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
  return ucs4_len(__from, __end, __max);
}

int
codecvt<char32_t, char, mbstate_t>::do_max_length() const throw()
{ return 4; }

inline template class __codecvt_abstract_base<char16_t, char, mbstate_t>;
inline template class __codecvt_abstract_base<char32_t, char, mbstate_t>;

_GLIBCXX_END_NAMESPACE_VERSION
}
#endif // _GLIBCXX_USE_C99_STDINT_TR1

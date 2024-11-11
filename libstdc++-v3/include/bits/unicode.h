// Unicode utilities -*- C++ -*-

// Copyright The GNU Toolchain Authors.
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

/** @file include/bits/unicode.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{format}
 */

#ifndef _GLIBCXX_UNICODE_H
#define _GLIBCXX_UNICODE_H 1

#if __cplusplus >= 202002L
#include <array>
#include <bit>      // bit_width
#include <charconv> // __detail::__from_chars_alnum_to_val_table
#include <string_view>
#include <cstdint>
#include <bits/stl_algo.h>
#include <bits/stl_iterator.h>
#include <bits/ranges_base.h> // iterator_t, sentinel_t, input_range, etc.
#include <bits/ranges_util.h> // view_interface

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace __unicode
{
  // A Unicode code point that is not a high or low surrogate.
  constexpr bool
  __is_scalar_value(char32_t __c)
  {
    if (__c < 0xD800) [[likely]]
      return true;
    return 0xDFFF < __c && __c <= 0x10FFFF;
  }

  // A code point that can be encoded in a single code unit of type _CharT.
  template<typename _CharT>
    constexpr bool
    __is_single_code_unit(char32_t __c)
    {
      if constexpr (__gnu_cxx::__int_traits<_CharT>::__max <= 0xFF)
	return __c < 0x7F; // ASCII character
      else
	return __c < __gnu_cxx::__int_traits<_CharT>::__max
		       && __is_scalar_value(__c);
    }

  // Based on https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/p2728r6.html#add-the-transcoding-iterator-template

  struct _Repl
  {
    constexpr char32_t
    operator()() const noexcept
    { return 0xFFFD; }
  };

  struct _Null_sentinel_t
  {
    template<input_iterator _It>
      requires default_initializable<iter_value_t<_It>>
	&& equality_comparable_with<iter_reference_t<_It>, iter_value_t<_It>>
      friend constexpr auto
      operator==(_It __it, _Null_sentinel_t)
      { return *__it == iter_value_t<_It>{}; }
  };

  template<typename _FromFmt, typename _ToFmt,
	   input_iterator _Iter, sentinel_for<_Iter> _Sent = _Iter,
	   typename _ErrorHandler = _Repl>
    requires convertible_to<iter_value_t<_Iter>, _FromFmt>
    class _Utf_iterator
    {
      static_assert(forward_iterator<_Iter> || noexcept(_ErrorHandler()()));

    public:
      using value_type = _ToFmt;
      using difference_type = iter_difference_t<_Iter>;
      using reference = value_type;
      using iterator_concept
	= std::__detail::__clamp_iter_cat<__iter_category_t<_Iter>,
					  bidirectional_iterator_tag>;

      constexpr _Utf_iterator() = default;

      constexpr
      _Utf_iterator(_Iter __first, _Iter __it, _Sent __last)
      requires bidirectional_iterator<_Iter>
      : _M_first_and_curr{__first, __it}, _M_last(__last)
      {
	if (_M_curr() != _M_last)
	  _M_read();
	else
	  _M_buf = {};
      }

      constexpr
      _Utf_iterator(_Iter __it, _Sent __last)
      requires (!bidirectional_iterator<_Iter>)
      : _M_first_and_curr{__it}, _M_last(__last)
      {
	if (_M_curr() != _M_last)
	  _M_read();
	else
	  _M_buf = {};
      }

      template<class _Iter2, class _Sent2>
	requires convertible_to<_Iter2, _Iter> && convertible_to<_Sent2, _Sent>
	constexpr
	_Utf_iterator(const _Utf_iterator<_FromFmt, _ToFmt, _Iter2, _Sent2,
					  _ErrorHandler>& __other)
	: _M_buf(__other._M_buf), _M_first_and_curr(__other._M_first_and_curr),
	  _M_buf_index(__other._M_buf_index), _M_buf_last(__other._M_buf_last),
	  _M_last(__other._M_last)
	{ }

      [[nodiscard]]
      constexpr _Iter
      begin() const requires bidirectional_iterator<_Iter>
      { return _M_first(); }

      [[nodiscard]]
      constexpr _Sent
      end() const { return _M_last; }

      [[nodiscard]]
      constexpr _Iter
      base() const requires forward_iterator<_Iter>
      { return _M_curr(); }

      [[nodiscard]]
      constexpr value_type
      operator*() const { return _M_buf[_M_buf_index]; }

      constexpr _Utf_iterator&
      operator++()
      {
	if (_M_buf_index + 1 == _M_buf_last && _M_curr() != _M_last)
	  {
	    if constexpr (forward_iterator<_Iter>)
	      std::advance(_M_curr(), _M_to_increment);
	    if (_M_curr() == _M_last)
	      _M_buf_index = 0;
	    else
	      _M_read();
	  }
	else if (_M_buf_index + 1 < _M_buf_last)
	  ++_M_buf_index;
	return *this;
      }

      constexpr _Utf_iterator
      operator++(int)
      {
	auto __tmp = *this;
	++*this;
	return __tmp;
      }

      constexpr _Utf_iterator&
      operator--() requires bidirectional_iterator<_Iter>
      {
	if (!_M_buf_index && _M_curr() != _M_first())
	  _M_read_reverse();
	else if (_M_buf_index)
	  --_M_buf_index;
	return *this;
      }

      constexpr _Utf_iterator
      operator--(int)
      {
	auto __tmp = *this;
	--*this;
	return __tmp;
      }

      [[nodiscard]]
      friend constexpr bool
      operator==(_Utf_iterator __lhs, _Utf_iterator __rhs)
      requires forward_iterator<_Iter> || requires (_Iter __i) { __i != __i; }
      {
	if constexpr (forward_iterator<_Iter>)
	  return __lhs._M_curr() == __rhs._M_curr()
		   && __lhs._M_buf_index == __rhs._M_buf_index;
	else if (__lhs._M_curr() != __rhs._M_curr())
	  return false;
	else if (__lhs._M_buf_index == __rhs._M_buf_index
		   && __lhs._M_buf_last == __rhs._M_buf_last)
	  return true;
	else
	  return __lhs._M_buf_index == __lhs._M_buf_last
		   && __rhs._M_buf_index == __rhs._M_buf_last;
      }

      [[nodiscard]]
      friend constexpr bool
      operator==(_Utf_iterator __lhs, _Sent __rhs)
      {
	if constexpr (forward_iterator<_Iter>)
	  return __lhs._M_curr() == __rhs;
	else
	  return __lhs._M_curr() == __rhs
		   && __lhs._M_buf_index == __lhs._M_buf_last;
      }

    private:
      constexpr void
      _M_read()
      {
	if constexpr (sizeof(_FromFmt) == sizeof(uint8_t))
	  _M_read_utf8();
	else if constexpr (sizeof(_FromFmt) == sizeof(uint16_t))
	  _M_read_utf16();
	else
	  {
	    static_assert(sizeof(_FromFmt) == sizeof(uint32_t));
	    _M_read_utf32();
	  }
      }

      constexpr void
      _M_read_reverse(); // TODO

      template<typename>
	struct _Guard
	{
	  _Guard(void*, _Iter&) { }
	};

      template<typename _It> requires forward_iterator<_It>
	struct _Guard<_It>
	{
	  constexpr ~_Guard() { _M_this->_M_curr() = std::move(_M_orig); }
	  _Utf_iterator* _M_this;
	  _It _M_orig;
	};

      constexpr void
      _M_read_utf8()
      {
	_Guard<_Iter> __g{this, _M_curr()};
	char32_t __c{};
	const uint8_t __lo_bound = 0x80, __hi_bound = 0xBF;
	uint8_t __u = *_M_curr()++;
	uint8_t __to_incr = 1;
	auto __incr = [&, this] {
	  ++__to_incr;
	  return ++_M_curr();
	};

	if (__u <= 0x7F) [[likely]]      // 0x00 to 0x7F
	  __c = __u;
	else if (__u < 0xC2) [[unlikely]]
	  __c = _S_error();
	else if (_M_curr() == _M_last) [[unlikely]]
	  __c = _S_error();
	else if (__u <= 0xDF) // 0xC2 to 0xDF
	  {
	    __c = __u & 0x1F;
	    __u = *_M_curr();

	    if (__u < __lo_bound || __u > __hi_bound) [[unlikely]]
	      __c = _S_error();
	    else
	      {
		__c = (__c << 6) | (__u & 0x3F);
		__incr();
	      }
	  }
	else if (__u <= 0xEF) // 0xE0 to 0xEF
	  {
	    const uint8_t __lo_bound_2 = __u == 0xE0 ? 0xA0 : __lo_bound;
	    const uint8_t __hi_bound_2 = __u == 0xED ? 0x9F : __hi_bound;

	    __c = __u & 0x0F;
	    __u = *_M_curr();

	    if (__u < __lo_bound_2 || __u > __hi_bound_2) [[unlikely]]
	      __c = _S_error();
	    else if (__incr() == _M_last) [[unlikely]]
	      __c = _S_error();
	    else
	      {
		__c = (__c << 6) | (__u & 0x3F);
		__u = *_M_curr();

		if (__u < __lo_bound || __u > __hi_bound) [[unlikely]]
		  __c = _S_error();
		else
		  {
		    __c = (__c << 6) | (__u & 0x3F);
		    __incr();
		  }
	      }
	  }
	else if (__u <= 0xF4) // 0xF0 to 0xF4
	  {
	    const uint8_t __lo_bound_2 = __u == 0xF0 ? 0x90 : __lo_bound;
	    const uint8_t __hi_bound_2 = __u == 0xF4 ? 0x8F : __hi_bound;

	    __c = __u & 0x07;
	    __u = *_M_curr();

	    if (__u < __lo_bound_2 || __u > __hi_bound_2) [[unlikely]]
	      __c = _S_error();
	    else if (__incr() == _M_last) [[unlikely]]
	      __c = _S_error();
	    else
	      {
		__c = (__c << 6) | (__u & 0x3F);
		__u = *_M_curr();

		if (__u < __lo_bound || __u > __hi_bound) [[unlikely]]
		  __c = _S_error();
		else if (__incr() == _M_last) [[unlikely]]
		  __c = _S_error();
		else
		  {
		    __c = (__c << 6) | (__u & 0x3F);
		    __u = *_M_curr();

		    if (__u < __lo_bound || __u > __hi_bound) [[unlikely]]
		      __c = _S_error();
		    else
		      {
			__c = (__c << 6) | (__u & 0x3F);
			__incr();
		      }
		  }
	      }
	  }
	else [[unlikely]]
	  __c = _S_error();

	_M_update(__c, __to_incr);
      }

      constexpr void
      _M_read_utf16()
      {
	_Guard<_Iter> __g{this, _M_curr()};
	char32_t __c{};
	uint16_t __u = *_M_curr()++;
	uint8_t __to_incr = 1;

	if (__u < 0xD800 || __u > 0xDFFF) [[likely]]
	  __c = __u;
	else if (__u < 0xDC00 && _M_curr() != _M_last)
	  {
	    uint16_t __u2 = *_M_curr();
	    if (__u2 < 0xDC00 || __u2 > 0xDFFF) [[unlikely]]
	      __c = _S_error();
	    else
	      {
		++_M_curr();
		__to_incr = 2;
		uint32_t __x = (__u & 0x3F) << 10 | (__u2 & 0x3FF);
		uint32_t __w = (__u >> 6) & 0x1F;
		__c = (__w + 1) << 16 | __x;
	      }
	  }
	else
	  __c = _S_error();

	_M_update(__c, __to_incr);
      }

      constexpr void
      _M_read_utf32()
      {
	_Guard<_Iter> __g{this, _M_curr()};
	char32_t __c = *_M_curr()++;
	if (!__is_scalar_value(__c)) [[unlikely]]
	  __c = _S_error();
	_M_update(__c, 1);
      }

      // Encode the code point __c as one or more code units in _M_buf.
      constexpr void
      _M_update(char32_t __c, uint8_t __to_incr)
      {
	_M_to_increment = __to_incr;
	_M_buf_index = 0;
	if constexpr (sizeof(_ToFmt) == sizeof(uint32_t))
	  {
	    _M_buf[0] = __c;
	    _M_buf_last = 1;
	  }
	else if constexpr (sizeof(_ToFmt) == sizeof(uint16_t))
	  {
	    if (__is_single_code_unit<_ToFmt>(__c))
	      {
		_M_buf[0] = __c;
		_M_buf[1] = 0;
		_M_buf_last = 1;
	      }
	    else
	      {
		// From http://www.unicode.org/faq/utf_bom.html#utf16-4
		const char32_t __lead_offset = 0xD800 - (0x10000 >> 10);
		char16_t __lead = __lead_offset + (__c >> 10);
		char16_t __trail = 0xDC00 + (__c & 0x3FF);
		_M_buf[0] = __lead;
		_M_buf[1] = __trail;
		_M_buf_last = 2;
	      }
	  }
	else
	  {
	    static_assert(sizeof(_ToFmt) == 1);
	    int __bits = std::bit_width((uint32_t)__c);
	    if (__bits <= 7) [[likely]]
	      {
		_M_buf[0] = __c;
		_M_buf[1] = _M_buf[2] = _M_buf[3] = 0;
		_M_buf_last = 1;
	      }
	    else if (__bits <= 11)
	      {
		_M_buf[0] = 0xC0 | (__c >> 6);
		_M_buf[1] = 0x80 | (__c & 0x3F);
		_M_buf[2] = _M_buf[3] = 0;
		_M_buf_last = 2;
	      }
	    else if (__bits <= 16)
	      {
		_M_buf[0] = 0xE0 | (__c >> 12);
		_M_buf[1] = 0x80 | ((__c >> 6) & 0x3F);
		_M_buf[2] = 0x80 | (__c & 0x3F);
		_M_buf[3] = 0;
		_M_buf_last = 3;
	      }
	    else
	      {
		_M_buf[0] = 0xF0 | ((__c >> 18) & 0x07);
		_M_buf[1] = 0x80 | ((__c >> 12) & 0x3F);
		_M_buf[2] = 0x80 | ((__c >> 6) & 0x3F);
		_M_buf[3] = 0x80 | (__c & 0x3F);
		_M_buf_last = 4;
	      }
	  }
      }

      constexpr char32_t
      _S_error()
      {
	char32_t __c = _ErrorHandler()();
	__glibcxx_assert(__is_scalar_value(__c));
	return __c;
      }

      constexpr _Iter
      _M_first() const requires bidirectional_iterator<_Iter>
      { return _M_first_and_curr._M_first; }

      constexpr _Iter&
      _M_curr() { return _M_first_and_curr._M_curr; }

      constexpr _Iter
      _M_curr() const { return _M_first_and_curr._M_curr; }

      array<value_type, 4 / sizeof(_ToFmt)> _M_buf;

      template<typename _It>
	struct _First_and_curr
	{
	  _First_and_curr() = default;

	  constexpr
	  _First_and_curr(_It __curr) : _M_curr(__curr) { }

	  template<convertible_to<_It> _It2>
	    constexpr
	    _First_and_curr(const _First_and_curr<_It2>& __other)
	    : _M_curr(__other._M_curr) { }

	  _It _M_curr;
	};

      template<typename _It> requires bidirectional_iterator<_It>
	struct _First_and_curr<_It>
	{
	  _First_and_curr() = default;

	  constexpr
	  _First_and_curr(_It __first, _It __curr)
	  : _M_first(__first), _M_curr(__curr) { }

	  template<convertible_to<_It> _It2>
	    constexpr
	    _First_and_curr(const _First_and_curr<_It2>& __other)
	    : _M_first(__other._M_first), _M_curr(__other._M_curr) { }

	  _It _M_first;
	  _It _M_curr;
	};

      _First_and_curr<_Iter> _M_first_and_curr;

      uint8_t _M_buf_index = 0;
      uint8_t _M_buf_last = 0;
      uint8_t _M_to_increment = 0;

      [[no_unique_address]] _Sent _M_last;

      template<typename _FromFmt2, typename _ToFmt2,
	       input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	       typename _ErrHandler>
	requires convertible_to<iter_value_t<_Iter2>, _FromFmt2>
	friend class _Utf_iterator;
    };

  template<typename _ToFormat, ranges::input_range _Range>
    class _Utf_view
    : public ranges::view_interface<_Utf_view<_ToFormat, _Range>>
    {
      using _Iterator = _Utf_iterator<ranges::range_value_t<_Range>,
				      _ToFormat, ranges::iterator_t<_Range>,
				      ranges::sentinel_t<_Range>>;

      template<typename _Iter, typename _Sent>
	constexpr auto
	_M_begin(_Iter __first, _Sent __last)
	{
	  if constexpr (bidirectional_iterator<_Iter>)
	    return _Iterator(__first, __first, __last);
	  else
	    return _Iterator(__first, __last);
	}

      template<typename _Iter, typename _Sent>
	constexpr auto
	_M_end(_Iter __first, _Sent __last)
	{
	  if constexpr (!is_same_v<_Iter, _Sent>)
	    return __last;
	  else if constexpr (bidirectional_iterator<_Iter>)
	    return _Iterator(__first, __last, __last);
	  else
	    return _Iterator(__last, __last);
	}

      _Range _M_base;

    public:
      constexpr explicit
      _Utf_view(_Range&& __r) : _M_base(std::forward<_Range>(__r)) { }

      constexpr auto begin()
      { return _M_begin(ranges::begin(_M_base), ranges::end(_M_base)); }

      constexpr auto end()
      { return _M_end(ranges::begin(_M_base), ranges::end(_M_base)); }

      constexpr bool empty() const { return ranges::empty(_M_base); }
    };

#ifdef __cpp_char8_t
  template<typename _View>
    using _Utf8_view = _Utf_view<char8_t, _View>;
#else
  template<typename _View>
    using _Utf8_view = _Utf_view<char, _View>;
#endif
  template<typename _View>
    using _Utf16_view = _Utf_view<char16_t, _View>;
  template<typename _View>
    using _Utf32_view = _Utf_view<char32_t, _View>;

inline namespace __v15_1_0
{
#define _GLIBCXX_GET_UNICODE_DATA 150100
#include "unicode-data.h"
#ifdef _GLIBCXX_GET_UNICODE_DATA
# error "Invalid unicode data"
#endif

  // The field width of a code point.
  constexpr int
  __field_width(char32_t __c) noexcept
  {
    if (__c < __width_edges[0]) [[likely]]
      return 1;

    auto* __p = std::upper_bound(__width_edges, std::end(__width_edges), __c);
    return (__p - __width_edges) % 2 + 1;
  }

  // @pre c <= 0x10FFFF
  constexpr _Gcb_property
  __grapheme_cluster_break_property(char32_t __c) noexcept
  {
    constexpr uint32_t __mask = (1 << __gcb_shift_bits) - 1;
    auto* __end = std::end(__gcb_edges);
    auto* __p = std::lower_bound(__gcb_edges, __end,
				 (__c << __gcb_shift_bits) | __mask);
    return _Gcb_property(__p[-1] & __mask);
  }

  constexpr bool
  __is_incb_linker(char32_t __c) noexcept
  {
    const auto __end = std::end(__incb_linkers);
    // Array is small enough that linear search is faster than binary search.
    return std::find(__incb_linkers, __end, __c) != __end;
  }

  // @pre c <= 0x10FFFF
  constexpr _InCB
  __incb_property(char32_t __c) noexcept
  {
    if ((__c << 2) < __incb_edges[0]) [[likely]]
      return _InCB(0);

    constexpr uint32_t __mask = 0x3;
    auto* __end = std::end(__incb_edges);
    auto* __p = std::lower_bound(__incb_edges, __end, (__c << 2) | __mask);
    return _InCB(__p[-1] & __mask);
  }

  constexpr bool
  __is_extended_pictographic(char32_t __c)
  {
    if (__c < __xpicto_edges[0]) [[likely]]
      return 0;

    auto* __p = std::upper_bound(__xpicto_edges, std::end(__xpicto_edges), __c);
    return (__p - __xpicto_edges) % 2;
  }

  struct _Grapheme_cluster_iterator_base
  {
    char32_t _M_c; // First code point in the cluster.
    _Gcb_property _M_prop; // GCB property of _M_c.
    enum class _XPicto : unsigned char { _Init, _Zwj, _Matched, _Failed };
    _XPicto _M_xpicto_seq_state = _XPicto::_Init;
    unsigned char _M_RI_count = 0;
    bool _M_incb_linker_seen = false;

    constexpr void
    _M_reset(char32_t __c, _Gcb_property __p)
    {
      _M_c = __c;
      _M_prop = __p;
      _M_xpicto_seq_state = _XPicto::_Init;
      _M_RI_count = 0;
      _M_incb_linker_seen = false;
    }

    constexpr void
    _M_update_xpicto_seq_state(char32_t __c, _Gcb_property __p)
    {
      if (_M_xpicto_seq_state == _XPicto::_Failed)
	return;

      auto __next_state = _XPicto::_Failed;
      if (_M_xpicto_seq_state != _XPicto::_Zwj) // i.e. Init or Matched
	{
	  if (__p == _Gcb_property::_Gcb_ZWJ)
	    {
	      if (_M_xpicto_seq_state == _XPicto::_Matched)
		__next_state = _XPicto::_Zwj;
	      // We check _M_c here so that we do the lookup at most once,
	      // and only for clusters containing at least one ZWJ.
	      else if (__is_extended_pictographic(_M_c))
		__next_state = _XPicto::_Zwj;
	    }
	  else if (__p == _Gcb_property::_Gcb_Extend)
	    __next_state = _M_xpicto_seq_state; // no change
	}
      else // Zwj
	{
	  // This assumes that all \p{Extended_Pictographic} emoji have
	  // Grapheme_Cluster_Break=Other.
	  if (__p == _Gcb_property::_Gcb_Other
		&& __is_extended_pictographic(__c))
	    __next_state = _XPicto::_Matched;
	}
      _M_xpicto_seq_state = __next_state;
    }

    constexpr void
    _M_update_ri_count(_Gcb_property __p)
    {
      if (__p == _Gcb_property::_Gcb_Regional_Indicator)
	++_M_RI_count;
      else
	_M_RI_count = 0;
    }

    constexpr void
    _M_update_incb_state(char32_t __c, _Gcb_property)
    {
      if (__is_incb_linker(__c))
	_M_incb_linker_seen = true;
    }
  };

  // Split a range into extended grapheme clusters.
  template<ranges::forward_range _View> requires ranges::view<_View>
    class _Grapheme_cluster_view
    : public ranges::view_interface<_Grapheme_cluster_view<_View>>
    {
    public:

      constexpr
      _Grapheme_cluster_view(_View __v)
      : _M_begin(_Utf32_view<_View>(std::move(__v)).begin())
      { }

      constexpr auto begin() const { return _M_begin; }
      constexpr auto end() const { return _M_begin.end(); }

    private:
      struct _Iterator : private _Grapheme_cluster_iterator_base
      {
      private:
	// Iterator over the underlying code points.
	using _U32_iterator = ranges::iterator_t<_Utf32_view<_View>>;

      public:
	// TODO: Change value_type to be subrange<_U32_iterator> instead?
	// Alternatively, value_type could be _Utf32_view<iterator_t<_View>>.
	// That would be the whole cluster, not just the first code point.
	// Would need to store two iterators and find end of current cluster
	// on increment, so operator* returns value_type(_M_base, _M_next).
	using value_type = char32_t;
	using iterator_concept = forward_iterator_tag;
	using difference_type = ptrdiff_t;

	constexpr
	_Iterator(_U32_iterator __i)
	: _M_base(__i)
	{
	  if (__i != __i.end())
	    {
	      _M_c = *__i;
	      _M_prop = __grapheme_cluster_break_property(_M_c);
	    }
	}

	// The first code point of the current extended grapheme cluster.
	constexpr value_type
	operator*() const
	{ return _M_c; }

	constexpr auto
	operator->() const
	{ return &_M_c; }

	// Move to the next extended grapheme cluster.
	constexpr _Iterator&
	operator++()
	{
	  const auto __end = _M_base.end();
	  if (_M_base != __end)
	    {
	      auto __p_prev = _M_prop;
	      auto __it = _M_base;
	      while (++__it != __end)
		{
		  char32_t __c = *__it;
		  auto __p = __grapheme_cluster_break_property(*__it);
		  _M_update_xpicto_seq_state(__c, __p);
		  _M_update_ri_count(__p);
		  _M_update_incb_state(__c, __p);
		  if (_M_is_break(__p_prev, __p, __it))
		    {
		      // Found a grapheme cluster break
		      _M_reset(__c, __p);
		      break;
		    }
		  __p_prev = __p;
		}
	      _M_base = __it;
	    }
	  return *this;
	}

	constexpr _Iterator
	operator++(int)
	{
	  auto __tmp = *this;
	  ++*this;
	  return __tmp;
	}

	constexpr bool
	operator==(const _Iterator& __i) const
	{ return _M_base == __i._M_base; }

	// This supports iter != iter.end()
	constexpr bool
	operator==(const ranges::sentinel_t<_View>& __i) const
	{ return _M_base == __i; }

	// Iterator to the start of the current cluster.
	constexpr auto base() const { return _M_base.base(); }

	// The end of the underlying view (not the end of the current cluster!)
	constexpr auto end() const { return _M_base.end(); }

	// Field width of the first code point in the cluster.
	constexpr int
	width() const noexcept
	{ return __field_width(_M_c); }

      private:
	_U32_iterator _M_base;

	// Implement the Grapheme Cluster Boundary Rules from Unicode Annex #29
	// http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules
	// This implements the rules from TR29 revision 43 in Unicode 15.1.0.
	// Return true if there is a break between code point with property p1
	// and code point with property p2.
	constexpr bool
	_M_is_break(_Gcb_property __p1, _Gcb_property __p2,
		    _U32_iterator __curr) const
	{
	  using enum _Gcb_property;

	  if (__p1 == _Gcb_Control || __p1 == _Gcb_LF)
	    return true; // Break after Control or LF.

	  if (__p1 == _Gcb_CR)
	    return __p2 != _Gcb_LF; // Do not break between a CR and LF.

	  // Rule GB5
	  if (__p2 == _Gcb_Control || __p2 == _Gcb_CR || __p2 == _Gcb_LF)
	    return true; // Break before Control, CR or LF.

	  // Rule GB6
	  if (__p1 == _Gcb_L)
	    switch (__p2)
	    {
	      case _Gcb_L:
	      case _Gcb_V:
	      case _Gcb_LV:
	      case _Gcb_LVT:
		return false; // Do not break Hangul syllable sequences.
	      default:
		return true;
	      }

	  // Rule GB7
	  if (__p1 == _Gcb_LV || __p1 == _Gcb_V)
	    switch (__p2)
	    {
	      case _Gcb_V:
	      case _Gcb_T:
		return false; // Do not break Hangul syllable sequences.
	      default:
		return true;
	      }

	  // Rule GB8
	  if (__p1 == _Gcb_LVT || __p1 == _Gcb_T)
	    return __p2 != _Gcb_T; // Do not break Hangul syllable sequences.

	  // Rule GB9
	  if (__p2 == _Gcb_Extend || __p2 == _Gcb_ZWJ)
	    return false; // Do not break before extending characters or ZWJ.

	  // The following GB9x rules only apply to extended grapheme clusters,
	  // which is what the C++ standard uses (not legacy grapheme clusters).

	  // Rule GB9a
	  if (__p2 == _Gcb_SpacingMark)
	    return false; // Do not break before SpacingMarks,
	  // Rule GB9b
	  if (__p1 == _Gcb_Prepend)
	    return false; // or after Prepend characters.

	  // Rule GB9c (Unicode 15.1.0)
	  // Do not break within certain combinations with
	  // Indic_Conjunct_Break (InCB)=Linker.
	  if (_M_incb_linker_seen
		&& __incb_property(_M_c) == _InCB::_Consonant
		&& __incb_property(*__curr) == _InCB::_Consonant)
	    {
	      // Match [_M_base, __curr] against regular expression
	      // Consonant ([Extend Linker]* Linker [Extend Linker]* Consonant)+
	      bool __have_linker = false;
	      auto __it = _M_base;
	      while (++__it != __curr)
		{
		  if (__is_incb_linker(*__it))
		    __have_linker = true;
		  else
		    {
		      auto __incb = __incb_property(*__it);
		      if (__incb == _InCB::_Consonant)
			__have_linker = false;
		      else if (__incb != _InCB::_Extend)
			break;
		    }
		}
	      if (__it == __curr && __have_linker)
		return false;
	    }

	  // Rule GB11
	  // Do not break within emoji modifier sequences
	  // or emoji zwj sequences.
	  if (__p1 == _Gcb_ZWJ && _M_xpicto_seq_state == _XPicto::_Matched)
	    return false;

	  // Rules GB12 and GB13
	  // Do not break within emoji flag sequences. That is, do not break
	  // between regional indicator (RI) symbols if there is an odd number
	  // of RI characters before the break point.
	  if (__p1 == _Gcb_property::_Gcb_Regional_Indicator && __p1 == __p2)
	    return (_M_RI_count & 1) == 0;

	  // Rule GB999
	  return true; // Otherwise, break everywhere.
	}
      };

      _Iterator _M_begin;
    };

} // namespace __v15_1_0

  // Return the field width of a string.
  template<typename _CharT>
    constexpr size_t
    __field_width(basic_string_view<_CharT> __s)
    {
      if (__s.empty()) [[unlikely]]
	return 0;
      _Grapheme_cluster_view<basic_string_view<_CharT>> __gc(__s);
      auto __it = __gc.begin();
      const auto __end = __gc.end();
      size_t __n = __it.width();
      while (++__it != __end)
	__n += __it.width();
      return __n;
    }

  // Truncate a string to at most `__max` field width units, and return the
  // resulting field width.
  template<typename _CharT>
    constexpr size_t
    __truncate(basic_string_view<_CharT>& __s, size_t __max)
    {
      if (__s.empty()) [[unlikely]]
	return 0;

      _Grapheme_cluster_view<basic_string_view<_CharT>> __gc(__s);
      auto __it = __gc.begin();
      const auto __end = __gc.end();
      size_t __n = __it.width();
      if (__n > __max)
	{
	  __s = {};
	  return 0;
	}
      while (++__it != __end)
	{
	  size_t __n2 = __n + __it.width();
	  if (__n2 > __max)
	    {
	      __s = basic_string_view<_CharT>(__s.begin(), __it.base());
	      return __n;
	    }
	  __n = __n2;
	}
      return __n;
    }

  template<typename _CharT>
    consteval bool
    __literal_encoding_is_unicode()
    {
      if constexpr (is_same_v<_CharT, char16_t>)
	return true;
      else if constexpr (is_same_v<_CharT, char32_t>)
	  return true;
#ifdef __cpp_char8_t
      else if constexpr (is_same_v<_CharT, char8_t>)
	return true;
#endif

      const char* __enc = "";

#ifdef __GNUC_EXECUTION_CHARSET_NAME
      auto __remove_iso10646_prefix = [](const char* __s) {
	// GNU iconv allows "ISO-10646/" prefix (case-insensitive).
	if (__s[0] == 'I' || __s[0] == 'i')
	  if (__s[1] == 'S' || __s[1] == 's')
	    if (__s[2] == 'O' || __s[2] == 'o')
	      if (string_view(__s + 3).starts_with("-10646/"))
		return __s + 10;
	return __s;
      };

      if constexpr (is_same_v<_CharT, char>)
	__enc = __remove_iso10646_prefix(__GNUC_EXECUTION_CHARSET_NAME);
# if defined _GLIBCXX_USE_WCHAR_T && defined __GNUC_WIDE_EXECUTION_CHARSET_NAME
      else
	__enc = __remove_iso10646_prefix(__GNUC_WIDE_EXECUTION_CHARSET_NAME);
# endif

      if ((__enc[0] == 'U' || __enc[0] == 'u')
	    && (__enc[1] == 'T' || __enc[1] == 't')
	    && (__enc[2] == 'F' || __enc[2] == 'f'))
	{
	  __enc += 3;
	  if (__enc[0] == '-')
	    ++__enc;
	  if (__enc[0] == '8')
	    return __enc[1] == '\0' || string_view(__enc + 1) == "//";
	  else if constexpr (!is_same_v<_CharT, char>)
	    {
	      string_view __s(__enc);
	      if (__s.ends_with("//"))
		__s.remove_suffix(2);
	      return __s == "16" || __s == "32";
	    }
	}
#elif defined __clang_literal_encoding__
      if constexpr (is_same_v<_CharT, char>)
	__enc = __clang_literal_encoding__;
# if defined _GLIBCXX_USE_WCHAR_T && defined __clang_wide_literal_encoding__
      else
	__enc = __clang_wide_literal_encoding__;
# endif
      // Clang accepts "-fexec-charset=utf-8" but the macro is still uppercase.
      string_view __s(__enc);
      if (__s == "UTF-8")
	return true;
      else if constexpr (!is_same_v<_CharT, char>)
	return __s == "UTF-16" || __s == "UTF-32";
#endif

      return false;
    }

  consteval bool
  __literal_encoding_is_utf8()
  { return __literal_encoding_is_unicode<char>(); }

  consteval bool
  __literal_encoding_is_extended_ascii()
  {
    return '0' == 0x30 && 'A' == 0x41 && 'Z' == 0x5a
	     && 'a' == 0x61 && 'z' == 0x7a;
  }

  // https://www.unicode.org/reports/tr22/tr22-8.html#Charset_Alias_Matching
  constexpr bool
  __charset_alias_match(string_view __a, string_view __b)
  {
    // Map alphanumeric chars to their base 64 value, everything else to 127.
    auto __map = [](char __c, bool& __num) -> unsigned char {
      if (__c == '0') [[unlikely]]
	return __num ? 0 : 127;
      const auto __v = __detail::__from_chars_alnum_to_val(__c);
      __num = __v < 10;
      return __v;
    };

    auto __ptr_a = __a.begin(), __end_a = __a.end();
    auto __ptr_b = __b.begin(), __end_b = __b.end();
    bool __num_a = false, __num_b = false;

    while (true)
      {
	// Find the value of the next alphanumeric character in each string.
	unsigned char __val_a{}, __val_b{};
	while (__ptr_a != __end_a
		 && (__val_a = __map(*__ptr_a, __num_a)) == 127)
	  ++__ptr_a;
	while (__ptr_b != __end_b
		 && (__val_b = __map(*__ptr_b, __num_b)) == 127)
	  ++__ptr_b;
	// Stop when we reach the end of a string, or get a mismatch.
	if (__ptr_a == __end_a)
	  return __ptr_b == __end_b;
	else if (__ptr_b == __end_b)
	  return false;
	else if (__val_a != __val_b)
	  return false; // Found non-matching characters.
	++__ptr_a;
	++__ptr_b;
      }
    return true;
  }

} // namespace __unicode

namespace ranges
{
  template<typename _To, typename _Range>
    inline constexpr bool
    enable_borrowed_range<std::__unicode::_Utf_view<_To, _Range>>
      = enable_borrowed_range<_Range>;

  template<typename _Range>
    inline constexpr bool
    enable_borrowed_range<std::__unicode::_Grapheme_cluster_view<_Range>>
      = enable_borrowed_range<_Range>;
} // namespace ranges

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // C++20
#endif // _GLIBCXX_UNICODE_H

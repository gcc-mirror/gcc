// Components for compile-time parsing of numbers -*- C++ -*-

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

/** @file bits/parse_numbers.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{chrono}
 */

#ifndef _PARSE_NUMBERS_H
#define _PARSE_NUMBERS_H 1

#pragma GCC system_header

// From n3642.pdf except I added binary literals and digit separator '\''.

#if __cplusplus > 201103L

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace __parse_int {

  template<unsigned _Base, char _Dig>
    struct _Digit;

  template<unsigned _Base>
    struct _Digit<_Base, '0'>
    {
      static constexpr bool valid{true};
      static constexpr unsigned value{0};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '1'>
    {
      static constexpr bool valid{true};
      static constexpr unsigned value{1};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '2'>
    {
      static_assert(_Base > 2, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{2};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '3'>
    {
      static_assert(_Base > 3, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{3};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '4'>
    {
      static_assert(_Base > 4, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{4};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '5'>
    {
      static_assert(_Base > 5, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{5};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '6'>
    {
      static_assert(_Base > 6, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{6};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '7'>
    {
      static_assert(_Base > 7, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{7};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '8'>
    {
      static_assert(_Base > 8, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{8};
    };

  template<unsigned _Base>
    struct _Digit<_Base, '9'>
    {
      static_assert(_Base > 9, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{9};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'a'>
    {
      static_assert(_Base > 0xa, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xa};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'A'>
    {
      static_assert(_Base > 0xa, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xa};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'b'>
    {
      static_assert(_Base > 0xb, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xb};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'B'>
    {
      static_assert(_Base > 0xb, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xb};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'c'>
    {
      static_assert(_Base > 0xc, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xc};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'C'>
    {
      static_assert(_Base > 0xc, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xc};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'd'>
    {
      static_assert(_Base > 0xd, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xd};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'D'>
    {
      static_assert(_Base > 0xd, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xd};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'e'>
    {
      static_assert(_Base > 0xe, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xe};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'E'>
    {
      static_assert(_Base > 0xe, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xe};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'f'>
    {
      static_assert(_Base > 0xf, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xf};
    };

  template<unsigned _Base>
    struct _Digit<_Base, 'F'>
    {
      static_assert(_Base > 0xf, "invalid digit");
      static constexpr bool valid{true};
      static constexpr unsigned value{0xf};
    };

  //  Digit separator
  template<unsigned _Base>
    struct _Digit<_Base, '\''>
    {
      static constexpr bool valid{false};
      static constexpr unsigned value{0};
    };


//------------------------------------------------------------------------------

  template<unsigned _Base, char _Dig, char... _Digs>
    struct _Digits_help
    {
      static constexpr unsigned
	value{_Digit<_Base, _Dig>::valid ?
	      1U + _Digits_help<_Base, _Digs...>::value :
	      _Digits_help<_Base, _Digs...>::value};
    };

  template<unsigned _Base, char _Dig>
    struct _Digits_help<_Base, _Dig>
    {
      static constexpr unsigned value{_Digit<_Base, _Dig>::valid ? 1U : 0U};
    };

  template<unsigned _Base, char... _Digs>
    struct _Digits
    {
      static constexpr unsigned value{_Digits_help<_Base, _Digs...>::value};
    };

  template<unsigned _Base>
    struct _Digits<_Base>
    {
      static constexpr unsigned value{0U};
    };

//------------------------------------------------------------------------------

  template<unsigned _Base, char _Dig, char... _Digs>
    struct _Power_help
    {
      static constexpr unsigned
	value{_Digit<_Base, _Dig>::valid ?
	      _Base * _Power_help<_Base, _Digs...>::value :
	      _Power_help<_Base, _Digs...>::value};
    };

  template<unsigned _Base, char _Dig>
    struct _Power_help<_Base, _Dig>
    {
      static constexpr unsigned value{_Digit<_Base, _Dig>::valid ? 1U : 0U};
    };

  template<unsigned _Base, char... _Digs>
    struct _Power
    {
      static constexpr unsigned value{_Power_help<_Base, _Digs...>::value};
    };

  template<unsigned _Base>
    struct _Power<_Base>
    {
      static constexpr unsigned value{0U};
    };

//------------------------------------------------------------------------------

  template<unsigned _Base, unsigned _Pow, char _Dig, char... _Digs>
    struct _Number_help
    {
      static constexpr unsigned
	value{_Digit<_Base, _Dig>::valid ?
	      _Pow * _Digit<_Base, _Dig>::value
	      + _Number_help<_Base, _Pow / _Base, _Digs...>::value :
	      _Number_help<_Base, _Pow, _Digs...>::value};
    };

  template<unsigned _Base, unsigned _Pow, char _Dig>
    struct _Number_help<_Base, _Pow, _Dig>
    {
      //static_assert(_Pow == 1U, "power should be one");
      static constexpr unsigned
	value{_Digit<_Base, _Dig>::valid ? _Digit<_Base, _Dig>::value : 0U};
    };

  template<unsigned _Base, char... _Digs>
    struct _Number
    {
      static constexpr unsigned
	value{_Number_help<_Base, _Power<_Base, _Digs...>::value,
			   _Digs...>::value};
    };

  template<unsigned _Base>
    struct _Number<_Base>
    {
      static constexpr unsigned value{0U};
    };

//------------------------------------------------------------------------------
//  This _Parse_int is the same 'level' as the old _Base_dispatch.

  template<char... _Digs>
    struct _Parse_int;

  template<char... _Digs>
    struct _Parse_int<'0', 'b', _Digs...>
    {
      static constexpr unsigned long long
	value{_Number<2U, _Digs...>::value};
    };

  template<char... _Digs>
    struct _Parse_int<'0', 'B', _Digs...>
    {
      static constexpr unsigned long long
	value{_Number<2U, _Digs...>::value};
    };

  template<char... _Digs>
    struct _Parse_int<'0', 'x', _Digs...>
    {
      static constexpr unsigned long long
	value{_Number<16U, _Digs...>::value};
    };

  template<char... _Digs>
    struct _Parse_int<'0', 'X', _Digs...>
    {
      static constexpr unsigned long long
	value{_Number<16U, _Digs...>::value};
    };

  template<char... _Digs>
    struct _Parse_int<'0', _Digs...>
    {
      static constexpr unsigned long long
	value{_Number<8U, _Digs...>::value};
    };

  template<char... _Digs>
    struct _Parse_int
    {
      static constexpr unsigned long long
	value{_Number<10U, _Digs...>::value};
    };

} // namespace __parse_int


namespace __select_int {

  template<unsigned long long _Val, typename... _Ints>
    struct _Select_int_base;

  template<unsigned long long _Val, typename _IntType, typename... _Ints>
    struct _Select_int_base<_Val, _IntType, _Ints...>
    : integral_constant
      <
	typename conditional
	<
	  _Val <= static_cast<unsigned long long>
		    (std::numeric_limits<_IntType>::max()),
	  _IntType,
	  typename _Select_int_base<_Val, _Ints...>::value_type
	>::type,
	_Val
      >
    { };

  template<unsigned long long _Val>
    struct _Select_int_base<_Val> : integral_constant<unsigned long long, _Val>
    { };

  template<char... _Digs>
    struct _Select_int
    : _Select_int_base<
	__parse_int::_Parse_int<_Digs...>::value,
	unsigned char,
	unsigned short,
	unsigned int,
	unsigned long,
	unsigned long long
      >
    { };

} // namespace __select_int

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // __cplusplus > 201103L

#endif // _PARSE_NUMBERS_H

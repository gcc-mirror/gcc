// Default predicates for internal use -*- C++ -*-

// Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

/** @file predefined_ops.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly. @headername{algorithm}
 */

#ifndef _GLIBCXX_PREDEFINED_OPS_H
#define _GLIBCXX_PREDEFINED_OPS_H	1

#include <bits/stl_function.h> // less<void>, equal_to<void>
#if __cplusplus >= 201103L
# include <type_traits>        // is_empty, is_scalar, __conditional_t, __or_
#else
# include <ext/type_traits.h>  // __conditional_type
#endif

namespace __gnu_cxx
{
namespace __ops
{
  // These two explicit specializations are always defined by libstdc++,
  // even when __cpp_lib_transparent_operators is not defined.
  typedef std::equal_to<void> equal_to;
  typedef std::less<void> less;

#if __cplusplus >= 201103L

  template<typename _Fn>
    using __by_ref_or_value_fn
      = std::__conditional_t<std::__or_<std::is_empty<_Fn>,
					std::is_scalar<_Fn>>::value,
			     _Fn, _Fn&>;

  // More generic replacements for the deprecated utilities
  // std::bind1st, std::bind2nd, and std::not1.
  // These aren't fully "transparent" like std::less<void> because they
  // do not use perfect forwarding, everything is treated as an lvalue.

  template<typename _Func, typename _Value, bool _Val_2nd = false>
    struct _Comp_with_val
    {
      using _Fn = __by_ref_or_value_fn<_Func>;

      explicit constexpr
      _Comp_with_val(_Fn __f, const _Value& __v)
      : _M_f(__f), _M_val(__v) { }

      [[__no_unique_address__]] _Fn _M_f;
      const _Value& _M_val;

      template<typename _Tp>
	_GLIBCXX14_CONSTEXPR bool
	operator()(_Tp&& __arg)
	{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
	  if constexpr (_Val_2nd)
	    return _M_f(__arg, _M_val);
	  else
	    return _M_f(_M_val, __arg);
#pragma GCC diagnostic pop
	}
    };

  template<typename _Func, typename _Value>
    using _Comp_with_val_1st = _Comp_with_val<_Func, _Value, false>;
  template<typename _Func, typename _Value>
    using _Comp_with_val_2nd = _Comp_with_val<_Func, _Value, true>;

    template<typename _Func>
    struct _Unary_negate
    {
      using _Fn = __by_ref_or_value_fn<_Func>;

      explicit constexpr
      _Unary_negate(_Fn __f) : _M_f(__f) { }

      [[__no_unique_address__]] _Fn _M_f;

      template<typename _Tp>
	_GLIBCXX14_CONSTEXPR bool
	operator()(_Tp&& __arg) { return !_M_f(__arg); }
    };

  template<typename _Func>
    constexpr _Unary_negate<_Func>
    not1(_Func& __f)
    { return _Unary_negate<_Func>(__f); }

#else // <= C++11

  template<typename _Fn>
    struct __by_ref_or_value_fn
    : __conditional_type<__is_empty(_Fn), _Fn, _Fn&>
    { };

  template<typename _Fn>
    struct __by_ref_or_value_fn<_Fn*>
    { typedef _Fn* __type; };

  // We don't use std::binder1st, std::binder2nd, or std::unary_negate here
  // because they require adaptable function objects, i.e. types with nested
  // result_type and argument_type/first_argument_type/second_argument_type.

  template<typename _Func, typename _Value>
    struct _Comp_with_val_1st
    {
      typedef typename __by_ref_or_value_fn<_Func>::__type _Fn;

      explicit
      _Comp_with_val_1st(_Fn __f, const _Value& __v)
      : _M_f(__f), _M_val(__v) { }

      _Fn _M_f;
      const _Value& _M_val;

      template<typename _Tp>
	bool operator()(_Tp& __arg) { return _M_f(_M_val, __arg); }
      template<typename _Tp>
	bool operator()(const _Tp& __arg) { return _M_f(_M_val, __arg); }
    };

  template<typename _Func, typename _Value>
    struct _Comp_with_val_2nd
    {
      typedef typename __by_ref_or_value_fn<_Func>::__type _Fn;

      explicit
      _Comp_with_val_2nd(_Fn __f, const _Value& __v)
      : _M_f(__f), _M_val(__v) { }

      _Fn _M_f;
      const _Value& _M_val;

      template<typename _Tp>
	bool operator()(_Tp& __arg) { return _M_f(__arg, _M_val); }
      template<typename _Tp>
	bool operator()(const _Tp& __arg) { return _M_f(__arg, _M_val); }
    };

  template<typename _Func>
    struct _Unary_negate_1 // N.B. different name for C++98 to satisfy ODR
    {
      typedef typename __by_ref_or_value_fn<_Func>::__type _Fn;

      explicit _Unary_negate_1(_Fn __f) : _M_f(__f) { }

      _Fn _M_f;

      template<typename _Tp>
	bool
	operator()(_Tp& __arg) { return !_M_f(__arg); }
      template<typename _Tp>
	bool
	operator()(const _Tp& __arg) { return !_M_f(__arg); }
    };

  template<typename _Func>
    inline _Unary_negate_1<_Func>
    not1(_Func& __f)
    { return _Unary_negate_1<_Func>(__f); }
#endif

  // N.B. these functions take lvalue references because we want to avoid
  // returning a call wrapper that has a dangling reference to a prvalue.

  template<typename _Func, typename _Value>
    _GLIBCXX_CONSTEXPR inline _Comp_with_val_1st<_Func, _Value>
    bind1st(_Func& __f, const _Value& __val)
    { return _Comp_with_val_1st<_Func, _Value>(__f, __val); }

  template<typename _Func, typename _Value>
    _GLIBCXX_CONSTEXPR inline _Comp_with_val_2nd<_Func, _Value>
    bind2nd(_Func& __f, const _Value& __val)
    { return _Comp_with_val_2nd<_Func, _Value>(__f, __val); }

  // Equivalent to bind2nd(equal_to{}, val)
  template<typename _Value>
    _GLIBCXX_CONSTEXPR inline _Comp_with_val_2nd<equal_to, _Value>
    __equal_to(const _Value& __val)
    { return _Comp_with_val_2nd<equal_to, _Value>(equal_to(), __val); }

} // namespace __ops
} // namespace __gnu_cxx

#endif

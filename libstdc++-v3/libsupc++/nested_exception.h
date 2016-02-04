// Nested Exception support header (nested_exception class) for -*- C++ -*-

// Copyright (C) 2009-2016 Free Software Foundation, Inc.
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

/** @file bits/nested_exception.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{exception}
 */

#ifndef _GLIBCXX_NESTED_EXCEPTION_H
#define _GLIBCXX_NESTED_EXCEPTION_H 1

#pragma GCC visibility push(default)

#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else

#include <bits/c++config.h>
#include <bits/move.h>

#if ATOMIC_INT_LOCK_FREE < 2
#  error This platform does not support exception propagation.
#endif

extern "C++" {

namespace std
{
  /**
   * @addtogroup exceptions
   * @{
   */

  /// Exception class with exception_ptr data member.
  class nested_exception
  {
    exception_ptr _M_ptr;

  public:
    nested_exception() noexcept : _M_ptr(current_exception()) { }

    nested_exception(const nested_exception&) noexcept = default;

    nested_exception& operator=(const nested_exception&) noexcept = default;

    virtual ~nested_exception() noexcept;

    [[noreturn]]
    void
    rethrow_nested() const
    {
      if (_M_ptr)
	rethrow_exception(_M_ptr);
      std::terminate();
    }

    exception_ptr
    nested_ptr() const noexcept
    { return _M_ptr; }
  };

  template<typename _Except>
    struct _Nested_exception : public _Except, public nested_exception
    {
      explicit _Nested_exception(const _Except& __ex)
      : _Except(__ex)
      { }

      explicit _Nested_exception(_Except&& __ex)
      : _Except(static_cast<_Except&&>(__ex))
      { }
    };

  template<typename _Tp,
	   bool __with_nested = !__is_base_of(nested_exception, _Tp)>
    struct _Throw_with_nested_impl
    {
      template<typename _Up>
	static void _S_throw(_Up&& __t)
	{ throw _Nested_exception<_Tp>{static_cast<_Up&&>(__t)}; }
    };

  template<typename _Tp>
    struct _Throw_with_nested_impl<_Tp, false>
    {
      template<typename _Up>
	static void _S_throw(_Up&& __t)
	{ throw static_cast<_Up&&>(__t); }
    };

  template<typename _Tp, bool = __is_class(_Tp) && !__is_final(_Tp)>
    struct _Throw_with_nested_helper : _Throw_with_nested_impl<_Tp>
    { };

  template<typename _Tp>
    struct _Throw_with_nested_helper<_Tp, false>
    : _Throw_with_nested_impl<_Tp, false>
    { };

  template<typename _Tp>
    struct _Throw_with_nested_helper<_Tp&, false>
    : _Throw_with_nested_helper<_Tp>
    { };

  template<typename _Tp>
    struct _Throw_with_nested_helper<_Tp&&, false>
    : _Throw_with_nested_helper<_Tp>
    { };

  /// If @p __t is derived from nested_exception, throws @p __t.
  /// Else, throws an implementation-defined object derived from both.
  template<typename _Tp>
    [[noreturn]]
    inline void
    throw_with_nested(_Tp&& __t)
    {
      _Throw_with_nested_helper<_Tp>::_S_throw(static_cast<_Tp&&>(__t));
    }

  template<typename _Tp, bool = __is_polymorphic(_Tp)>
    struct _Rethrow_if_nested_impl
    {
      static void _S_rethrow(const _Tp& __t)
      {
	if (auto __tp =
            dynamic_cast<const nested_exception*>(std::__addressof(__t)))
	  __tp->rethrow_nested();
      }
    };

  template<typename _Tp>
    struct _Rethrow_if_nested_impl<_Tp, false>
    {
      static void _S_rethrow(const _Tp&) { }
    };

  /// If @p __ex is derived from nested_exception, @p __ex.rethrow_nested().
  template<typename _Ex>
    inline void
    rethrow_if_nested(const _Ex& __ex)
    {
      _Rethrow_if_nested_impl<_Ex>::_S_rethrow(__ex);
    }

  // @} group exceptions
} // namespace std

} // extern "C++"

#endif // C++11

#pragma GCC visibility pop

#endif // _GLIBCXX_NESTED_EXCEPTION_H

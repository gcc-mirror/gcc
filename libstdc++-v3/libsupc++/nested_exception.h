// Nested Exception support header (nested_exception class) for -*- C++ -*-

// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else

#include <bits/move.h>
#include <bits/exception_ptr.h>

extern "C++" {

namespace std _GLIBCXX_VISIBILITY(default)
{
  /**
   * @addtogroup exceptions
   * @{
   */

  /** Mixin class that stores the current exception.
   *
   * This type can be used via `std::throw_with_nested` to store
   * the current exception nested within another exception.
   *
   * @headerfile exception
   * @since C++11
   * @see std::throw_with_nested
   * @ingroup exceptions
   */
  class nested_exception
  {
    exception_ptr _M_ptr;

  public:
    /// The default constructor stores the current exception (if any).
    nested_exception() noexcept : _M_ptr(current_exception()) { }

    nested_exception(const nested_exception&) noexcept = default;

    nested_exception& operator=(const nested_exception&) noexcept = default;

    virtual ~nested_exception() noexcept;

    /// Rethrow the stored exception, or terminate if none was stored.
    [[noreturn]]
    void
    rethrow_nested() const
    {
      if (_M_ptr)
	rethrow_exception(_M_ptr);
      std::terminate();
    }

    /// Access the stored exception.
    exception_ptr
    nested_ptr() const noexcept
    { return _M_ptr; }
  };

  /// @cond undocumented

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

#if __cplusplus < 201703L || ! defined __cpp_if_constexpr
  // [except.nested]/8
  // Throw an exception of unspecified type that is publicly derived from
  // both remove_reference_t<_Tp> and nested_exception.
  template<typename _Tp>
    [[noreturn]]
    inline void
    __throw_with_nested_impl(_Tp&& __t, true_type)
    {
      throw _Nested_exception<__remove_cvref_t<_Tp>>{std::forward<_Tp>(__t)};
    }

  template<typename _Tp>
    [[noreturn]]
    inline void
    __throw_with_nested_impl(_Tp&& __t, false_type)
    { throw std::forward<_Tp>(__t); }
#endif

  /// @endcond

  /** Throw an exception that also stores the currently active exception.
   *
   * If `_Tp` is derived from `std::nested_exception` or is not usable
   * as a base-class, throws a copy of `__t`.
   * Otherwise, throws an object of an implementation-defined type derived
   * from both `_Tp` and `std::nested_exception`, containing a copy of `__t`
   * and the result of `std::current_exception()`.
   *
   * In other words, throws the argument as a new exception that contains
   * the currently active exception nested within it. This is intended for
   * use in a catch handler to replace the caught exception with a different
   * type, while still preserving the original exception. When the new
   * exception is caught, the nested exception can be rethrown by using
   * `std::rethrow_if_nested`.
   *
   * This can be used at API boundaries, for example to catch a library's
   * internal exception type and rethrow it nested with a `std::runtime_error`,
   * or vice versa.
   *
   * @since C++11
   */
  template<typename _Tp>
    [[noreturn]]
    inline void
    throw_with_nested(_Tp&& __t)
    {
      using _Up = typename decay<_Tp>::type;
      using _CopyConstructible
	= __and_<is_copy_constructible<_Up>, is_move_constructible<_Up>>;
      static_assert(_CopyConstructible::value,
	  "throw_with_nested argument must be CopyConstructible");

#if __cplusplus >= 201703L && __cpp_if_constexpr
      if constexpr (is_class_v<_Up>)
	if constexpr (!is_final_v<_Up>)
	  if constexpr (!is_base_of_v<nested_exception, _Up>)
	    throw _Nested_exception<_Up>{std::forward<_Tp>(__t)};
      throw std::forward<_Tp>(__t);
#else
      using __nest = __and_<is_class<_Up>, __bool_constant<!__is_final(_Up)>,
			    __not_<is_base_of<nested_exception, _Up>>>;
      std::__throw_with_nested_impl(std::forward<_Tp>(__t), __nest{});
#endif
    }

#if __cplusplus < 201703L || ! defined __cpp_if_constexpr
  /// @cond undocumented

  // Attempt dynamic_cast to nested_exception and call rethrow_nested().
  template<typename _Ex>
    inline void
    __rethrow_if_nested_impl(const _Ex* __ptr, true_type)
    {
      if (auto __ne_ptr = dynamic_cast<const nested_exception*>(__ptr))
	__ne_ptr->rethrow_nested();
    }

  // Otherwise, no effects.
  inline void
  __rethrow_if_nested_impl(const void*, false_type)
  { }

  /// @endcond
#endif

  /** Rethrow a nested exception
   *
   * If `__ex` contains a `std::nested_exception` object, call its
   * `rethrow_nested()` member to rethrow the stored exception.
   *
   * After catching an exception thrown by a call to `std::throw_with_nested`
   * this function can be used to rethrow the exception that was active when
   * `std::throw_with_nested` was called.
   *
   * @since C++11
   */
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 2484. rethrow_if_nested() is doubly unimplementable
  // 2784. Resolution to LWG 2484 is missing "otherwise, no effects" and [...]
  template<typename _Ex>
# if ! __cpp_rtti
    [[__gnu__::__always_inline__]]
#endif
    inline void
    rethrow_if_nested(const _Ex& __ex)
    {
      const _Ex* __ptr = __builtin_addressof(__ex);
#if __cplusplus < 201703L || ! defined __cpp_if_constexpr
# if __cpp_rtti
      using __cast = __and_<is_polymorphic<_Ex>,
			    __or_<__not_<is_base_of<nested_exception, _Ex>>,
				  is_convertible<_Ex*, nested_exception*>>>;
# else
      using __cast = __and_<is_polymorphic<_Ex>,
			    is_base_of<nested_exception, _Ex>,
			    is_convertible<_Ex*, nested_exception*>>;
# endif
      std::__rethrow_if_nested_impl(__ptr, __cast{});
#else
      if constexpr (!is_polymorphic_v<_Ex>)
	return;
      else if constexpr (is_base_of_v<nested_exception, _Ex>
			 && !is_convertible_v<_Ex*, nested_exception*>)
	return; // nested_exception base class is inaccessible or ambiguous.
# if ! __cpp_rtti
      else if constexpr (!is_base_of_v<nested_exception, _Ex>)
	return; // Cannot do polymorphic casts without RTTI.
# endif
      else if (auto __ne_ptr = dynamic_cast<const nested_exception*>(__ptr))
	__ne_ptr->rethrow_nested();
#endif
    }

  /// @} group exceptions
} // namespace std

} // extern "C++"

#endif // C++11
#endif // _GLIBCXX_NESTED_EXCEPTION_H

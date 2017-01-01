// Implementation of std::reference_wrapper -*- C++ -*-

// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

/** @file include/bits/bind.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */

#ifndef _GLIBCXX_REFWRAP_H
#define _GLIBCXX_REFWRAP_H 1

#pragma GCC system_header

#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else

#include <bits/move.h>
#include <bits/invoke.h>
#include <bits/stl_function.h> // for unary_function and binary_function

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /// If we have found a result_type, extract it.
  template<typename _Functor, typename = __void_t<>>
    struct _Maybe_get_result_type
    { };

  template<typename _Functor>
    struct _Maybe_get_result_type<_Functor,
				  __void_t<typename _Functor::result_type>>
    { typedef typename _Functor::result_type result_type; };

  /**
   *  Base class for any function object that has a weak result type, as
   *  defined in 20.8.2 [func.require] of C++11.
  */
  template<typename _Functor>
    struct _Weak_result_type_impl
    : _Maybe_get_result_type<_Functor>
    { };

  /// Retrieve the result type for a function type.
  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes...)>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes......)>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes...) const>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes......) const>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes...) volatile>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes......) volatile>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes...) const volatile>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(_ArgTypes......) const volatile>
    { typedef _Res result_type; };

  /// Retrieve the result type for a function reference.
  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(&)(_ArgTypes...)>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(&)(_ArgTypes......)>
    { typedef _Res result_type; };

  /// Retrieve the result type for a function pointer.
  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(*)(_ArgTypes...)>
    { typedef _Res result_type; };

  template<typename _Res, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res(*)(_ArgTypes......)>
    { typedef _Res result_type; };

  /// Retrieve result type for a member function pointer.
  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes...)>
    { typedef _Res result_type; };

  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes......)>
    { typedef _Res result_type; };

  /// Retrieve result type for a const member function pointer.
  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes...) const>
    { typedef _Res result_type; };

  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes......) const>
    { typedef _Res result_type; };

  /// Retrieve result type for a volatile member function pointer.
  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes...) volatile>
    { typedef _Res result_type; };

  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes......) volatile>
    { typedef _Res result_type; };

  /// Retrieve result type for a const volatile member function pointer.
  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes...)
				  const volatile>
    { typedef _Res result_type; };

  template<typename _Res, typename _Class, typename... _ArgTypes>
    struct _Weak_result_type_impl<_Res (_Class::*)(_ArgTypes......)
				  const volatile>
    { typedef _Res result_type; };

  /**
   *  Strip top-level cv-qualifiers from the function object and let
   *  _Weak_result_type_impl perform the real work.
  */
  template<typename _Functor>
    struct _Weak_result_type
    : _Weak_result_type_impl<typename remove_cv<_Functor>::type>
    { };

  // Detect nested argument_type.
  template<typename _Tp, typename = __void_t<>>
    struct _Refwrap_base_arg1
    { };

  // Nested argument_type.
  template<typename _Tp>
    struct _Refwrap_base_arg1<_Tp,
			      __void_t<typename _Tp::argument_type>>
    {
      typedef typename _Tp::argument_type argument_type;
    };

  // Detect nested first_argument_type and second_argument_type.
  template<typename _Tp, typename = __void_t<>>
    struct _Refwrap_base_arg2
    { };

  // Nested first_argument_type and second_argument_type.
  template<typename _Tp>
    struct _Refwrap_base_arg2<_Tp,
			      __void_t<typename _Tp::first_argument_type,
				       typename _Tp::second_argument_type>>
    {
      typedef typename _Tp::first_argument_type first_argument_type;
      typedef typename _Tp::second_argument_type second_argument_type;
    };

  /**
   *  Derives from unary_function or binary_function when it
   *  can. Specializations handle all of the easy cases. The primary
   *  template determines what to do with a class type, which may
   *  derive from both unary_function and binary_function.
  */
  template<typename _Tp>
    struct _Reference_wrapper_base
    : _Weak_result_type<_Tp>, _Refwrap_base_arg1<_Tp>, _Refwrap_base_arg2<_Tp>
    { };

  // - a function type (unary)
  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res(_T1)>
    : unary_function<_T1, _Res>
    { };

  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res(_T1) const>
    : unary_function<_T1, _Res>
    { };

  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res(_T1) volatile>
    : unary_function<_T1, _Res>
    { };

  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res(_T1) const volatile>
    : unary_function<_T1, _Res>
    { };

  // - a function type (binary)
  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res(_T1, _T2)>
    : binary_function<_T1, _T2, _Res>
    { };

  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res(_T1, _T2) const>
    : binary_function<_T1, _T2, _Res>
    { };

  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res(_T1, _T2) volatile>
    : binary_function<_T1, _T2, _Res>
    { };

  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res(_T1, _T2) const volatile>
    : binary_function<_T1, _T2, _Res>
    { };

  // - a function pointer type (unary)
  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res(*)(_T1)>
    : unary_function<_T1, _Res>
    { };

  // - a function pointer type (binary)
  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res(*)(_T1, _T2)>
    : binary_function<_T1, _T2, _Res>
    { };

  // - a pointer to member function type (unary, no qualifiers)
  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res (_T1::*)()>
    : unary_function<_T1*, _Res>
    { };

  // - a pointer to member function type (binary, no qualifiers)
  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res (_T1::*)(_T2)>
    : binary_function<_T1*, _T2, _Res>
    { };

  // - a pointer to member function type (unary, const)
  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res (_T1::*)() const>
    : unary_function<const _T1*, _Res>
    { };

  // - a pointer to member function type (binary, const)
  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res (_T1::*)(_T2) const>
    : binary_function<const _T1*, _T2, _Res>
    { };

  // - a pointer to member function type (unary, volatile)
  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res (_T1::*)() volatile>
    : unary_function<volatile _T1*, _Res>
    { };

  // - a pointer to member function type (binary, volatile)
  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res (_T1::*)(_T2) volatile>
    : binary_function<volatile _T1*, _T2, _Res>
    { };

  // - a pointer to member function type (unary, const volatile)
  template<typename _Res, typename _T1>
    struct _Reference_wrapper_base<_Res (_T1::*)() const volatile>
    : unary_function<const volatile _T1*, _Res>
    { };

  // - a pointer to member function type (binary, const volatile)
  template<typename _Res, typename _T1, typename _T2>
    struct _Reference_wrapper_base<_Res (_T1::*)(_T2) const volatile>
    : binary_function<const volatile _T1*, _T2, _Res>
    { };

  /**
   *  @brief Primary class template for reference_wrapper.
   *  @ingroup functors
   *  @{
   */
  template<typename _Tp>
    class reference_wrapper
    : public _Reference_wrapper_base<typename remove_cv<_Tp>::type>
    {
      _Tp* _M_data;

    public:
      typedef _Tp type;

      reference_wrapper(_Tp& __indata) noexcept
      : _M_data(std::__addressof(__indata))
      { }

      reference_wrapper(_Tp&&) = delete;

      reference_wrapper(const reference_wrapper&) = default;

      reference_wrapper&
      operator=(const reference_wrapper&) = default;

      operator _Tp&() const noexcept
      { return this->get(); }

      _Tp&
      get() const noexcept
      { return *_M_data; }

      template<typename... _Args>
	typename result_of<_Tp&(_Args&&...)>::type
	operator()(_Args&&... __args) const
	{
	  return std::__invoke(get(), std::forward<_Args>(__args)...);
	}
    };


  /// Denotes a reference should be taken to a variable.
  template<typename _Tp>
    inline reference_wrapper<_Tp>
    ref(_Tp& __t) noexcept
    { return reference_wrapper<_Tp>(__t); }

  /// Denotes a const reference should be taken to a variable.
  template<typename _Tp>
    inline reference_wrapper<const _Tp>
    cref(const _Tp& __t) noexcept
    { return reference_wrapper<const _Tp>(__t); }

  template<typename _Tp>
    void ref(const _Tp&&) = delete;

  template<typename _Tp>
    void cref(const _Tp&&) = delete;

  /// Partial specialization.
  template<typename _Tp>
    inline reference_wrapper<_Tp>
    ref(reference_wrapper<_Tp> __t) noexcept
    { return ref(__t.get()); }

  /// Partial specialization.
  template<typename _Tp>
    inline reference_wrapper<const _Tp>
    cref(reference_wrapper<_Tp> __t) noexcept
    { return cref(__t.get()); }

  // @} group functors

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // C++11

#endif // _GLIBCXX_REFWRAP_H

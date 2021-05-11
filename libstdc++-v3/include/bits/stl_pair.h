// Pair implementation -*- C++ -*-

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996,1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file bits/stl_pair.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{utility}
 */

#ifndef _STL_PAIR_H
#define _STL_PAIR_H 1

#include <bits/move.h> // for std::move / std::forward, and std::swap

#if __cplusplus >= 201103L
# include <type_traits> // for std::__decay_and_strip, std::is_reference_v
#endif
#if __cplusplus > 201703L
# include <compare>
# define __cpp_lib_constexpr_utility 201811L
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @addtogroup utilities
   *  @{
   */

#if __cplusplus >= 201103L
  /// Tag type for piecewise construction of std::pair objects.
  struct piecewise_construct_t { explicit piecewise_construct_t() = default; };

  /// Tag for piecewise construction of std::pair objects.
  _GLIBCXX17_INLINE constexpr piecewise_construct_t piecewise_construct =
    piecewise_construct_t();

  /// @cond undocumented

  // Forward declarations.
  template<typename...>
    class tuple;

  template<size_t...>
    struct _Index_tuple;

#if ! __cpp_lib_concepts
  // Concept utility functions, reused in conditionally-explicit
  // constructors.
  // See PR 70437, don't look at is_constructible or
  // is_convertible if the types are the same to
  // avoid querying those properties for incomplete types.
  template <bool, typename _T1, typename _T2>
    struct _PCC
    {
      template <typename _U1, typename _U2>
      static constexpr bool _ConstructiblePair()
      {
	return __and_<is_constructible<_T1, const _U1&>,
		      is_constructible<_T2, const _U2&>>::value;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyConvertiblePair()
      {
	return __and_<is_convertible<const _U1&, _T1>,
		      is_convertible<const _U2&, _T2>>::value;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _MoveConstructiblePair()
      {
	return __and_<is_constructible<_T1, _U1&&>,
		      is_constructible<_T2, _U2&&>>::value;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyMoveConvertiblePair()
      {
	return __and_<is_convertible<_U1&&, _T1>,
		      is_convertible<_U2&&, _T2>>::value;
      }


      template <bool __implicit, typename _U1, typename _U2>
      static constexpr bool _DeprConsPair()
      {
	using __do_converts = __and_<is_convertible<_U1&&, _T1>,
				     is_convertible<_U2&&, _T2>>;
	using __converts = typename conditional<__implicit,
						__do_converts,
						__not_<__do_converts>>::type;
	return __and_<is_constructible<_T1, _U1&&>,
		      is_constructible<_T2, _U2&&>,
		      __converts
		     >::value;
      }
    };

  template <typename _T1, typename _T2>
    struct _PCC<false, _T1, _T2>
    {
      template <typename _U1, typename _U2>
      static constexpr bool _ConstructiblePair()
      {
	return false;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyConvertiblePair()
      {
	return false;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _MoveConstructiblePair()
      {
	return false;
      }

      template <typename _U1, typename _U2>
      static constexpr bool _ImplicitlyMoveConvertiblePair()
      {
	return false;
      }
    };
#endif // lib concepts
#endif // C++11

  template<typename _U1, typename _U2> class __pair_base
  {
#if __cplusplus >= 201103L && ! __cpp_lib_concepts
    template<typename _T1, typename _T2> friend struct pair;
    __pair_base() = default;
    ~__pair_base() = default;
    __pair_base(const __pair_base&) = default;
    __pair_base& operator=(const __pair_base&) = delete;
#endif // C++11
  };

  /// @endcond

 /**
   *  @brief Struct holding two objects of arbitrary type.
   *
   *  @tparam _T1  Type of first object.
   *  @tparam _T2  Type of second object.
   *
   *  <https://gcc.gnu.org/onlinedocs/libstdc++/manual/utilities.html>
   */
  template<typename _T1, typename _T2>
    struct pair
    : public __pair_base<_T1, _T2>
    {
      typedef _T1 first_type;    ///< The type of the `first` member
      typedef _T2 second_type;   ///< The type of the `second` member

      _T1 first;                 ///< The first member
      _T2 second;                ///< The second member

#if __cplusplus >= 201103L
      constexpr pair(const pair&) = default;	///< Copy constructor
      constexpr pair(pair&&) = default;		///< Move constructor

      template<typename... _Args1, typename... _Args2>
	_GLIBCXX20_CONSTEXPR
	pair(piecewise_construct_t, tuple<_Args1...>, tuple<_Args2...>);

      /// Swap the first members and then the second members.
      _GLIBCXX20_CONSTEXPR void
      swap(pair& __p)
      noexcept(__and_<__is_nothrow_swappable<_T1>,
		      __is_nothrow_swappable<_T2>>::value)
      {
	using std::swap;
	swap(first, __p.first);
	swap(second, __p.second);
      }

    private:
      template<typename... _Args1, size_t... _Indexes1,
	       typename... _Args2, size_t... _Indexes2>
	_GLIBCXX20_CONSTEXPR
	pair(tuple<_Args1...>&, tuple<_Args2...>&,
	     _Index_tuple<_Indexes1...>, _Index_tuple<_Indexes2...>);
    public:

#if __cpp_lib_concepts
      // C++20 implementation using concepts, explicit(bool), fully constexpr.

      /// Default constructor
      constexpr
      explicit(__not_<__and_<__is_implicitly_default_constructible<_T1>,
			     __is_implicitly_default_constructible<_T2>>>())
      pair()
      requires is_default_constructible_v<_T1>
	       && is_default_constructible_v<_T2>
      : first(), second()
      { }

    private:

      /// @cond undocumented
      template<typename _U1, typename _U2>
	static constexpr bool
	_S_constructible()
	{
	  if constexpr (is_constructible_v<_T1, _U1>)
	    return is_constructible_v<_T2, _U2>;
	  return false;
	}

      template<typename _U1, typename _U2>
	static constexpr bool
	_S_nothrow_constructible()
	{
	  if constexpr (is_nothrow_constructible_v<_T1, _U1>)
	    return is_nothrow_constructible_v<_T2, _U2>;
	  return false;
	}

      template<typename _U1, typename _U2>
	static constexpr bool
	_S_convertible()
	{
	  if constexpr (is_convertible_v<_U1, _T1>)
	    return is_convertible_v<_U2, _T2>;
	  return false;
	}
      /// @endcond

    public:

      /// Constructor accepting lvalues of `first_type` and `second_type`
      constexpr explicit(!_S_convertible<const _T1&, const _T2&>())
      pair(const _T1& __x, const _T2& __y)
      noexcept(_S_nothrow_constructible<const _T1&, const _T2&>())
      requires (_S_constructible<const _T1&, const _T2&>())
      : first(__x), second(__y)
      { }

      /// Constructor accepting two values of arbitrary types
      template<typename _U1, typename _U2>
	requires (_S_constructible<_U1, _U2>())
	constexpr explicit(!_S_convertible<_U1, _U2>())
	pair(_U1&& __x, _U2&& __y)
	noexcept(_S_nothrow_constructible<_U1, _U2>())
	: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y))
	{ }

      /// Converting constructor from a `pair<U1, U2>` lvalue
      template<typename _U1, typename _U2>
	requires (_S_constructible<const _U1&, const _U2&>())
	constexpr explicit(!_S_convertible<const _U1&, const _U2&>())
	pair(const pair<_U1, _U2>& __p)
	noexcept(_S_nothrow_constructible<const _U1&, const _U2&>())
	: first(__p.first), second(__p.second)
	{ }

      /// Converting constructor from a `pair<U1, U2>` rvalue
      template<typename _U1, typename _U2>
	requires (_S_constructible<_U1, _U2>())
	constexpr explicit(!_S_convertible<_U1, _U2>())
	pair(pair<_U1, _U2>&& __p)
	noexcept(_S_nothrow_constructible<_U1, _U2>())
	: first(std::forward<_U1>(__p.first)),
	  second(std::forward<_U2>(__p.second))
	{ }

  private:
      /// @cond undocumented
      template<typename _U1, typename _U2>
	static constexpr bool
	_S_assignable()
	{
	  if constexpr (is_assignable_v<_T1&, _U1>)
	    return is_assignable_v<_T2&, _U2>;
	  return false;
	}

      template<typename _U1, typename _U2>
	static constexpr bool
	_S_nothrow_assignable()
	{
	  if constexpr (is_nothrow_assignable_v<_T1&, _U1>)
	    return is_nothrow_assignable_v<_T2&, _U2>;
	  return false;
	}
      /// @endcond

  public:

      pair& operator=(const pair&) = delete;

      /// Copy assignment operator
      constexpr pair&
      operator=(const pair& __p)
      noexcept(_S_nothrow_assignable<const _T1&, const _T2&>())
      requires (_S_assignable<const _T1&, const _T2&>())
      {
	first = __p.first;
	second = __p.second;
	return *this;
      }

      /// Move assignment operator
      constexpr pair&
      operator=(pair&& __p)
      noexcept(_S_nothrow_assignable<_T1, _T2>())
      requires (_S_assignable<_T1, _T2>())
      {
	first = std::forward<first_type>(__p.first);
	second = std::forward<second_type>(__p.second);
	return *this;
      }

      /// Converting assignment from a `pair<U1, U2>` lvalue
      template<typename _U1, typename _U2>
	constexpr pair&
	operator=(const pair<_U1, _U2>& __p)
	noexcept(_S_nothrow_assignable<const _U1&, const _U2&>())
	requires (_S_assignable<const _U1&, const _U2&>())
	{
	  first = __p.first;
	  second = __p.second;
	  return *this;
	}

      /// Converting assignment from a `pair<U1, U2>` rvalue
      template<typename _U1, typename _U2>
	constexpr pair&
	operator=(pair<_U1, _U2>&& __p)
	noexcept(_S_nothrow_assignable<_U1, _U2>())
	requires (_S_assignable<_U1, _U2>())
	{
	  first = std::forward<_U1>(__p.first);
	  second = std::forward<_U2>(__p.second);
	  return *this;
	}
#else
      // C++11/14/17 implementation using enable_if, partially constexpr.

      /** The default constructor creates @c first and @c second using their
       *  respective default constructors.  */
      template <typename _U1 = _T1,
                typename _U2 = _T2,
                typename enable_if<__and_<
                                     __is_implicitly_default_constructible<_U1>,
                                     __is_implicitly_default_constructible<_U2>>
                                   ::value, bool>::type = true>
      constexpr pair()
      : first(), second() { }

      template <typename _U1 = _T1,
                typename _U2 = _T2,
                typename enable_if<__and_<
                       is_default_constructible<_U1>,
                       is_default_constructible<_U2>,
                       __not_<
                         __and_<__is_implicitly_default_constructible<_U1>,
                                __is_implicitly_default_constructible<_U2>>>>
                                   ::value, bool>::type = false>
      explicit constexpr pair()
      : first(), second() { }

      // Shortcut for constraining the templates that don't take pairs.
      /// @cond undocumented
      using _PCCP = _PCC<true, _T1, _T2>;
      /// @endcond

      /// Construct from two const lvalues, allowing implicit conversions.
      template<typename _U1 = _T1, typename _U2=_T2, typename
	       enable_if<_PCCP::template
			   _ConstructiblePair<_U1, _U2>()
	                 && _PCCP::template
			   _ImplicitlyConvertiblePair<_U1, _U2>(),
                         bool>::type=true>
      constexpr pair(const _T1& __a, const _T2& __b)
      : first(__a), second(__b) { }

      /// Construct from two const lvalues, disallowing implicit conversions.
       template<typename _U1 = _T1, typename _U2=_T2, typename
		enable_if<_PCCP::template
			    _ConstructiblePair<_U1, _U2>()
	                  && !_PCCP::template
			    _ImplicitlyConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
      explicit constexpr pair(const _T1& __a, const _T2& __b)
      : first(__a), second(__b) { }

      // Shortcut for constraining the templates that take pairs.
      /// @cond undocumented
      template <typename _U1, typename _U2>
        using _PCCFP = _PCC<!is_same<_T1, _U1>::value
			    || !is_same<_T2, _U2>::value,
			    _T1, _T2>;
      /// @endcond

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _ConstructiblePair<_U1, _U2>()
	                 && _PCCFP<_U1, _U2>::template
			   _ImplicitlyConvertiblePair<_U1, _U2>(),
			  bool>::type=true>
        constexpr pair(const pair<_U1, _U2>& __p)
        : first(__p.first), second(__p.second) { }

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _ConstructiblePair<_U1, _U2>()
			 && !_PCCFP<_U1, _U2>::template
			   _ImplicitlyConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
	explicit constexpr pair(const pair<_U1, _U2>& __p)
	: first(__p.first), second(__p.second) { }

#if _GLIBCXX_USE_DEPRECATED
    private:
      /// @cond undocumented

      // A type which can be constructed from literal zero, but not nullptr
      struct __null_ptr_constant
      {
	__null_ptr_constant(int __null_ptr_constant::*) { }
	template<typename _Tp,
		 typename = __enable_if_t<is_null_pointer<_Tp>::value>>
	__null_ptr_constant(_Tp) = delete;
      };

      // True if type _Up is one of _Tp& or const _Tp&
      template<typename _Up, typename _Tp>
	using __is_lvalue_of
	  = __or_<is_same<_Up, const _Tp&>, is_same<_Up, _Tp&>>;

      /// @endcond
    public:

      // Deprecated extensions to DR 811.
      template<typename _U1,
	       __enable_if_t<!__is_lvalue_of<_U1, _T1>::value
			     && _PCCP::template
			       _DeprConsPair<true, _U1, nullptr_t>(),
			     bool> = true>
       _GLIBCXX_DEPRECATED_SUGGEST("nullptr")
       constexpr pair(_U1&& __x, __null_ptr_constant)
       : first(std::forward<_U1>(__x)), second(nullptr) { }

      template<typename _U1,
	       __enable_if_t<!__is_lvalue_of<_U1, _T1>::value
			     && _PCCP::template
			       _DeprConsPair<false, _U1, nullptr_t>(),
			     bool> = false>
       _GLIBCXX_DEPRECATED_SUGGEST("nullptr")
       explicit constexpr pair(_U1&& __x, __null_ptr_constant)
       : first(std::forward<_U1>(__x)), second(nullptr) { }

      template<typename _U2,
	       __enable_if_t<!__is_lvalue_of<_U2, _T2>::value
			     && _PCCP::template
			       _DeprConsPair<true, nullptr_t, _U2>(),
			     bool> = true>
       _GLIBCXX_DEPRECATED_SUGGEST("nullptr")
       constexpr pair(__null_ptr_constant, _U2&& __y)
       : first(nullptr), second(std::forward<_U2>(__y)) { }

      template<typename _U2,
	       __enable_if_t<!__is_lvalue_of<_U2, _T2>::value
			     && _PCCP::template
			       _DeprConsPair<false, nullptr_t, _U2>(),
			     bool> = false>
       _GLIBCXX_DEPRECATED_SUGGEST("nullptr")
       explicit pair(__null_ptr_constant, _U2&& __y)
       : first(nullptr), second(std::forward<_U2>(__y)) { }
#endif

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCP::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && _PCCP::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=true>
	constexpr pair(_U1&& __x, _U2&& __y)
	: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y)) { }

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCP::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && !_PCCP::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
	explicit constexpr pair(_U1&& __x, _U2&& __y)
	: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y)) { }


      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && _PCCFP<_U1, _U2>::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=true>
	constexpr pair(pair<_U1, _U2>&& __p)
	: first(std::forward<_U1>(__p.first)),
	  second(std::forward<_U2>(__p.second)) { }

      template<typename _U1, typename _U2, typename
	       enable_if<_PCCFP<_U1, _U2>::template
			   _MoveConstructiblePair<_U1, _U2>()
			  && !_PCCFP<_U1, _U2>::template
			   _ImplicitlyMoveConvertiblePair<_U1, _U2>(),
                         bool>::type=false>
	explicit constexpr pair(pair<_U1, _U2>&& __p)
	: first(std::forward<_U1>(__p.first)),
	  second(std::forward<_U2>(__p.second)) { }

      pair&
      operator=(typename conditional<
		__and_<is_copy_assignable<_T1>,
		       is_copy_assignable<_T2>>::value,
		const pair&, const __nonesuch&>::type __p)
      {
	first = __p.first;
	second = __p.second;
	return *this;
      }

      pair&
      operator=(typename conditional<
		__and_<is_move_assignable<_T1>,
		       is_move_assignable<_T2>>::value,
		pair&&, __nonesuch&&>::type __p)
      noexcept(__and_<is_nothrow_move_assignable<_T1>,
		      is_nothrow_move_assignable<_T2>>::value)
      {
	first = std::forward<first_type>(__p.first);
	second = std::forward<second_type>(__p.second);
	return *this;
      }

      template<typename _U1, typename _U2>
	typename enable_if<__and_<is_assignable<_T1&, const _U1&>,
				  is_assignable<_T2&, const _U2&>>::value,
			   pair&>::type
	operator=(const pair<_U1, _U2>& __p)
	{
	  first = __p.first;
	  second = __p.second;
	  return *this;
	}

      template<typename _U1, typename _U2>
	typename enable_if<__and_<is_assignable<_T1&, _U1&&>,
				  is_assignable<_T2&, _U2&&>>::value,
			   pair&>::type
	operator=(pair<_U1, _U2>&& __p)
	{
	  first = std::forward<_U1>(__p.first);
	  second = std::forward<_U2>(__p.second);
	  return *this;
	}
#endif // lib concepts
#else
      // C++03 implementation

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 265.  std::pair::pair() effects overly restrictive
      /** The default constructor creates @c first and @c second using their
       *  respective default constructors.  */
      pair() : first(), second() { }

      /// Two objects may be passed to a `pair` constructor to be copied.
      pair(const _T1& __a, const _T2& __b)
      : first(__a), second(__b) { }

      /// Templated constructor to convert from other pairs.
      template<typename _U1, typename _U2>
	pair(const pair<_U1, _U2>& __p)
	: first(__p.first), second(__p.second) { }
#endif // C++11
    };

  /// @relates pair @{

#if __cpp_deduction_guides >= 201606
  template<typename _T1, typename _T2> pair(_T1, _T2) -> pair<_T1, _T2>;
#endif

  /// Two pairs of the same type are equal iff their members are equal.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator==(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __x.first == __y.first && __x.second == __y.second; }

#if __cpp_lib_three_way_comparison && __cpp_lib_concepts
  template<typename _T1, typename _T2>
    constexpr common_comparison_category_t<__detail::__synth3way_t<_T1>,
					   __detail::__synth3way_t<_T2>>
    operator<=>(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    {
      if (auto __c = __detail::__synth3way(__x.first, __y.first); __c != 0)
	return __c;
      return __detail::__synth3way(__x.second, __y.second);
    }
#else
  /** Defines a lexicographical order for pairs.
   *
   * For two pairs of the same type, `P` is ordered before `Q` if
   * `P.first` is less than `Q.first`, or if `P.first` and `Q.first`
   * are equivalent (neither is less than the other) and `P.second` is less
   * than `Q.second`.
  */
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator<(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __x.first < __y.first
	     || (!(__y.first < __x.first) && __x.second < __y.second); }

  /// Uses @c operator== to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator!=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__x == __y); }

  /// Uses @c operator< to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator>(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __y < __x; }

  /// Uses @c operator< to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator<=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__y < __x); }

  /// Uses @c operator< to find the result.
  template<typename _T1, typename _T2>
    inline _GLIBCXX_CONSTEXPR bool
    operator>=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__x < __y); }
#endif // !(three_way_comparison && concepts)

#if __cplusplus >= 201103L
  /** Swap overload for pairs. Calls std::pair::swap().
   *
   * @note This std::swap overload is not declared in C++03 mode,
   * which has performance implications, e.g. see https://gcc.gnu.org/PR38466
  */
  template<typename _T1, typename _T2>
    _GLIBCXX20_CONSTEXPR inline
#if __cplusplus > 201402L || !defined(__STRICT_ANSI__) // c++1z or gnu++11
    // Constrained free swap overload, see p0185r1
    typename enable_if<__and_<__is_swappable<_T1>,
                              __is_swappable<_T2>>::value>::type
#else
    void
#endif
    swap(pair<_T1, _T2>& __x, pair<_T1, _T2>& __y)
    noexcept(noexcept(__x.swap(__y)))
    { __x.swap(__y); }

#if __cplusplus > 201402L || !defined(__STRICT_ANSI__) // c++1z or gnu++11
  template<typename _T1, typename _T2>
    typename enable_if<!__and_<__is_swappable<_T1>,
			       __is_swappable<_T2>>::value>::type
    swap(pair<_T1, _T2>&, pair<_T1, _T2>&) = delete;
#endif
#endif // __cplusplus >= 201103L

  /// @} relates pair

  /**
   *  @brief A convenience wrapper for creating a pair from two objects.
   *  @param  __x  The first object.
   *  @param  __y  The second object.
   *  @return   A newly-constructed pair<> object of the appropriate type.
   *
   *  The C++98 standard says the objects are passed by reference-to-const,
   *  but C++03 says they are passed by value (this was LWG issue #181).
   *
   *  Since C++11 they have been passed by forwarding reference and then
   *  forwarded to the new members of the pair. To create a pair with a
   *  member of reference type, pass a `reference_wrapper` to this function.
   */
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 181.  make_pair() unintended behavior
#if __cplusplus >= 201103L
  // NB: DR 706.
  template<typename _T1, typename _T2>
    constexpr pair<typename __decay_and_strip<_T1>::__type,
                   typename __decay_and_strip<_T2>::__type>
    make_pair(_T1&& __x, _T2&& __y)
    {
      typedef typename __decay_and_strip<_T1>::__type __ds_type1;
      typedef typename __decay_and_strip<_T2>::__type __ds_type2;
      typedef pair<__ds_type1, __ds_type2> 	      __pair_type;
      return __pair_type(std::forward<_T1>(__x), std::forward<_T2>(__y));
    }
#else
  template<typename _T1, typename _T2>
    inline pair<_T1, _T2>
    make_pair(_T1 __x, _T2 __y)
    { return pair<_T1, _T2>(__x, __y); }
#endif

  /// @}

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif /* _STL_PAIR_H */

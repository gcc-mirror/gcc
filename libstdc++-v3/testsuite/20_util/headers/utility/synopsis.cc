// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <utility>

#if __cplusplus < 201103L
# define CONSTEXPR
#else
# define CONSTEXPR constexpr
#endif

#if __cplusplus < 201402L && ! defined(_GLIBCXX_RELEASE)
# define CONSTEXPR11x
#else
# define CONSTEXPR11x constexpr
#endif

#if __cplusplus < 201402L
# define CONSTEXPR14
#else
# define CONSTEXPR14 constexpr
#endif

#if __cplusplus < 201703L
# define CONSTEXPR17
#else
# define CONSTEXPR17 constexpr
#endif

#if __cplusplus < 202002L
# define CONSTEXPR20
#else
# define CONSTEXPR20 constexpr
#endif

namespace std {
  //  lib.operators, operators:
  namespace rel_ops {
    template<class T> bool operator!=(const T&, const T&);
    template<class T> bool operator> (const T&, const T&);
    template<class T> bool operator<=(const T&, const T&);
    template<class T> bool operator>=(const T&, const T&);
  }

#if __cplusplus >= 201103L
#if 0
  // N.B. our std::swap doesn't actually match this due to constraints on
  // the template parameter.
  template<class T>
    CONSTEXPR20
    void swap(T&, T&) noexcept(is_nothrow_move_constructible<T>::value
			       && is_nothrow_move_assignable<T>::value);
#endif

  template<class T, size_t N>
    CONSTEXPR20
    void swap(T (&a)[N], T (&b)[N]) noexcept(noexcept(swap(*a, *b)));

#if __cplusplus >= 201703L
  template <class T, class U /* = T */>
    CONSTEXPR20
    T exchange(T& obj, U&& new_val)
#if defined _GLIBCXX_RELEASE // This noexcept is a libstdc++ extension.
    noexcept(__and_<is_nothrow_move_constructible<T>,
		    is_nothrow_assignable<T&, U>>::value)
#endif
    ;
#endif

  template<class T>
    CONSTEXPR11x
    T&& forward(typename remove_reference<T>::type& t) noexcept;
  template<class T>
    CONSTEXPR11x
    T&& forward(typename remove_reference<T>::type&& t) noexcept;

  template<class T>
    CONSTEXPR11x
    typename remove_reference<T>::type&& move(T&& t) noexcept;

  template<class T>
    CONSTEXPR17
    typename conditional< ! is_nothrow_move_constructible<T>::value
			  && is_copy_constructible<T>::value,
			  const T&, T&&>::type
    move_if_noexcept(T& x) noexcept;

#if __cplusplus >= 201703L
  template<class T>
    constexpr add_const_t<T>& as_const(T& t) noexcept;
#endif

  template <class T>
    typename add_rvalue_reference<T>::type declval() noexcept;

#if __cplusplus >= 201402L
  template<class T, T...> struct integer_sequence;
#endif

#endif // C++11

  //  lib.pairs, pairs:
  template <class T1, class T2> struct pair;
  template <class T1, class T2>
  CONSTEXPR bool operator==(const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  CONSTEXPR bool operator< (const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  CONSTEXPR bool operator!=(const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  CONSTEXPR bool operator> (const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  CONSTEXPR bool operator>=(const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  CONSTEXPR bool operator<=(const pair<T1,T2>&, const pair<T1,T2>&);

#if __cplusplus >= 201103L
  struct piecewise_construct_t;
#if __cplusplus >= 201703L
  struct in_place_t;
  template<class> struct in_place_type_t;
  template<size_t> struct in_place_index_t;

#if __cplusplus > 202302L
  struct monostate;
  template<> struct hash<monostate>;
#endif
#endif
#endif
}

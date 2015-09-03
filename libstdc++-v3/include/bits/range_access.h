// <range_access.h> -*- C++ -*-

// Copyright (C) 2010-2015 Free Software Foundation, Inc.
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

/** @file bits/range_access.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iterator}
 */

#ifndef _GLIBCXX_RANGE_ACCESS_H
#define _GLIBCXX_RANGE_ACCESS_H 1

#pragma GCC system_header

#if __cplusplus >= 201103L
#include <initializer_list>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @brief  Return an iterator pointing to the first element of
   *          the container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    begin(_Container& __cont) -> decltype(__cont.begin())
    { return __cont.begin(); }

  /**
   *  @brief  Return an iterator pointing to the first element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    begin(const _Container& __cont) -> decltype(__cont.begin())
    { return __cont.begin(); }

  /**
   *  @brief  Return an iterator pointing to one past the last element of
   *          the container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    end(_Container& __cont) -> decltype(__cont.end())
    { return __cont.end(); }

  /**
   *  @brief  Return an iterator pointing to one past the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    end(const _Container& __cont) -> decltype(__cont.end())
    { return __cont.end(); }

  /**
   *  @brief  Return an iterator pointing to the first element of the array.
   *  @param  __arr  Array.
   */
  template<class _Tp, size_t _Nm>
    inline _GLIBCXX14_CONSTEXPR _Tp*
    begin(_Tp (&__arr)[_Nm])
    { return __arr; }

  /**
   *  @brief  Return an iterator pointing to one past the last element
   *          of the array.
   *  @param  __arr  Array.
   */
  template<class _Tp, size_t _Nm>
    inline _GLIBCXX14_CONSTEXPR _Tp*
    end(_Tp (&__arr)[_Nm])
    { return __arr + _Nm; }

#if __cplusplus >= 201402L

  template<typename _Tp> class valarray;
  // These overloads must be declared for cbegin and cend to use them.
  template<typename _Tp> _Tp* begin(valarray<_Tp>&);
  template<typename _Tp> const _Tp* begin(const valarray<_Tp>&);
  template<typename _Tp> _Tp* end(valarray<_Tp>&);
  template<typename _Tp> const _Tp* end(const valarray<_Tp>&);

  /**
   *  @brief  Return an iterator pointing to the first element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline constexpr auto
    cbegin(const _Container& __cont) noexcept(noexcept(std::begin(__cont)))
      -> decltype(std::begin(__cont))
    { return std::begin(__cont); }

  /**
   *  @brief  Return an iterator pointing to one past the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline constexpr auto
    cend(const _Container& __cont) noexcept(noexcept(std::end(__cont)))
      -> decltype(std::end(__cont))
    { return std::end(__cont); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    rbegin(_Container& __cont) -> decltype(__cont.rbegin())
    { return __cont.rbegin(); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    rbegin(const _Container& __cont) -> decltype(__cont.rbegin())
    { return __cont.rbegin(); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    rend(_Container& __cont) -> decltype(__cont.rend())
    { return __cont.rend(); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    rend(const _Container& __cont) -> decltype(__cont.rend())
    { return __cont.rend(); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the array.
   *  @param  __arr  Array.
   */
  template<class _Tp, size_t _Nm>
    inline reverse_iterator<_Tp*>
    rbegin(_Tp (&__arr)[_Nm])
    { return reverse_iterator<_Tp*>(__arr + _Nm); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the array.
   *  @param  __arr  Array.
   */
  template<class _Tp, size_t _Nm>
    inline reverse_iterator<_Tp*>
    rend(_Tp (&__arr)[_Nm])
    { return reverse_iterator<_Tp*>(__arr); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the initializer_list.
   *  @param  __il  initializer_list.
   */
  template<class _Tp>
    inline reverse_iterator<const _Tp*>
    rbegin(initializer_list<_Tp> __il)
    { return reverse_iterator<const _Tp*>(__il.end()); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the initializer_list.
   *  @param  __il  initializer_list.
   */
  template<class _Tp>
    inline reverse_iterator<const _Tp*>
    rend(initializer_list<_Tp> __il)
    { return reverse_iterator<const _Tp*>(__il.begin()); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    crbegin(const _Container& __cont) -> decltype(std::rbegin(__cont))
    { return std::rbegin(__cont); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<class _Container>
    inline auto
    crend(const _Container& __cont) -> decltype(std::rend(__cont))
    { return std::rend(__cont); }

#endif // C++14

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // C++11

#endif // _GLIBCXX_RANGE_ACCESS_H

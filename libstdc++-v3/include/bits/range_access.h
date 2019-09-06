// <range_access.h> -*- C++ -*-

// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    begin(_Container& __cont) -> decltype(__cont.begin())
    { return __cont.begin(); }

  /**
   *  @brief  Return an iterator pointing to the first element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    begin(const _Container& __cont) -> decltype(__cont.begin())
    { return __cont.begin(); }

  /**
   *  @brief  Return an iterator pointing to one past the last element of
   *          the container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    end(_Container& __cont) -> decltype(__cont.end())
    { return __cont.end(); }

  /**
   *  @brief  Return an iterator pointing to one past the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    end(const _Container& __cont) -> decltype(__cont.end())
    { return __cont.end(); }

  /**
   *  @brief  Return an iterator pointing to the first element of the array.
   *  @param  __arr  Array.
   */
  template<typename _Tp, size_t _Nm>
    inline _GLIBCXX14_CONSTEXPR _Tp*
    begin(_Tp (&__arr)[_Nm])
    { return __arr; }

  /**
   *  @brief  Return an iterator pointing to one past the last element
   *          of the array.
   *  @param  __arr  Array.
   */
  template<typename _Tp, size_t _Nm>
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
  template<typename _Container>
    inline constexpr auto
    cbegin(const _Container& __cont) noexcept(noexcept(std::begin(__cont)))
      -> decltype(std::begin(__cont))
    { return std::begin(__cont); }

  /**
   *  @brief  Return an iterator pointing to one past the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline constexpr auto
    cend(const _Container& __cont) noexcept(noexcept(std::end(__cont)))
      -> decltype(std::end(__cont))
    { return std::end(__cont); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    rbegin(_Container& __cont) -> decltype(__cont.rbegin())
    { return __cont.rbegin(); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    rbegin(const _Container& __cont) -> decltype(__cont.rbegin())
    { return __cont.rbegin(); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    rend(_Container& __cont) -> decltype(__cont.rend())
    { return __cont.rend(); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    rend(const _Container& __cont) -> decltype(__cont.rend())
    { return __cont.rend(); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the array.
   *  @param  __arr  Array.
   */
  template<typename _Tp, size_t _Nm>
    inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Tp*>
    rbegin(_Tp (&__arr)[_Nm])
    { return reverse_iterator<_Tp*>(__arr + _Nm); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the array.
   *  @param  __arr  Array.
   */
  template<typename _Tp, size_t _Nm>
    inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Tp*>
    rend(_Tp (&__arr)[_Nm])
    { return reverse_iterator<_Tp*>(__arr); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the initializer_list.
   *  @param  __il  initializer_list.
   */
  template<typename _Tp>
    inline _GLIBCXX17_CONSTEXPR reverse_iterator<const _Tp*>
    rbegin(initializer_list<_Tp> __il)
    { return reverse_iterator<const _Tp*>(__il.end()); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the initializer_list.
   *  @param  __il  initializer_list.
   */
  template<typename _Tp>
    inline _GLIBCXX17_CONSTEXPR reverse_iterator<const _Tp*>
    rend(initializer_list<_Tp> __il)
    { return reverse_iterator<const _Tp*>(__il.begin()); }

  /**
   *  @brief  Return a reverse iterator pointing to the last element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    crbegin(const _Container& __cont) -> decltype(std::rbegin(__cont))
    { return std::rbegin(__cont); }

  /**
   *  @brief  Return a reverse iterator pointing one past the first element of
   *          the const container.
   *  @param  __cont  Container.
   */
  template<typename _Container>
    inline _GLIBCXX17_CONSTEXPR auto
    crend(const _Container& __cont) -> decltype(std::rend(__cont))
    { return std::rend(__cont); }

#endif // C++14

#if __cplusplus >= 201703L
#define __cpp_lib_nonmember_container_access 201411

  /**
   *  @brief  Return the size of a container.
   *  @param  __cont  Container.
   */
  template <typename _Container>
    constexpr auto
    size(const _Container& __cont) noexcept(noexcept(__cont.size()))
    -> decltype(__cont.size())
    { return __cont.size(); }

  /**
   *  @brief  Return the size of an array.
   */
  template <typename _Tp, size_t _Nm>
    constexpr size_t
    size(const _Tp (&)[_Nm]) noexcept
    { return _Nm; }

  /**
   *  @brief  Return whether a container is empty.
   *  @param  __cont  Container.
   */
  template <typename _Container>
    [[nodiscard]] constexpr auto
    empty(const _Container& __cont) noexcept(noexcept(__cont.empty()))
    -> decltype(__cont.empty())
    { return __cont.empty(); }

  /**
   *  @brief  Return whether an array is empty (always false).
   */
  template <typename _Tp, size_t _Nm>
    [[nodiscard]] constexpr bool
    empty(const _Tp (&)[_Nm]) noexcept
    { return false; }

  /**
   *  @brief  Return whether an initializer_list is empty.
   *  @param  __il  Initializer list.
   */
  template <typename _Tp>
    [[nodiscard]] constexpr bool
    empty(initializer_list<_Tp> __il) noexcept
    { return __il.size() == 0;}

  /**
   *  @brief  Return the data pointer of a container.
   *  @param  __cont  Container.
   */
  template <typename _Container>
    constexpr auto
    data(_Container& __cont) noexcept(noexcept(__cont.data()))
    -> decltype(__cont.data())
    { return __cont.data(); }

  /**
   *  @brief  Return the data pointer of a const container.
   *  @param  __cont  Container.
   */
  template <typename _Container>
    constexpr auto
    data(const _Container& __cont) noexcept(noexcept(__cont.data()))
    -> decltype(__cont.data())
    { return __cont.data(); }

  /**
   *  @brief  Return the data pointer of an array.
   *  @param  __array  Array.
   */
  template <typename _Tp, size_t _Nm>
    constexpr _Tp*
    data(_Tp (&__array)[_Nm]) noexcept
    { return __array; }

  /**
   *  @brief  Return the data pointer of an initializer list.
   *  @param  __il  Initializer list.
   */
  template <typename _Tp>
    constexpr const _Tp*
    data(initializer_list<_Tp> __il) noexcept
    { return __il.begin(); }

#endif // C++17

#if __cplusplus > 201703L
  template<typename _Container>
    constexpr auto
    ssize(const _Container& __cont)
    noexcept(noexcept(__cont.size()))
    -> common_type_t<ptrdiff_t, make_signed_t<decltype(__cont.size())>>
    {
      using type = make_signed_t<decltype(__cont.size())>;
      return static_cast<common_type_t<ptrdiff_t, type>>(__cont.size());
    }

  template<typename _Tp, ptrdiff_t _Num>
    constexpr ptrdiff_t
    ssize(const _Tp (&)[_Num]) noexcept
    { return _Num; }

  // "why are these in namespace std:: and not __gnu_cxx:: ?"
  // because if we don't put them here it's impossible to
  // have implicit ADL with "using std::begin/end/size/data;".
  template <typename _Container>
    constexpr auto
    __adl_begin(_Container& __cont) noexcept(noexcept(begin(__cont)))
    { return begin(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_end(_Container& __cont) noexcept(noexcept(end(__cont)))
    { return end(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_cbegin(_Container& __cont) noexcept(noexcept(cbegin(__cont)))
    { return cbegin(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_cend(_Container& __cont) noexcept(noexcept(cend(__cont)))
    { return cend(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_rbegin(_Container& __cont) noexcept(noexcept(rbegin(__cont)))
    { return rbegin(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_rend(_Container& __cont) noexcept(noexcept(rend(__cont)))
    { return rend(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_crbegin(_Container& __cont) noexcept(noexcept(crbegin(__cont)))
    { return crbegin(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_crend(_Container& __cont) noexcept(noexcept(crend(__cont)))
    { return crend(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_data(_Container& __cont) noexcept(noexcept(data(__cont)))
    { return data(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_cdata(_Container& __cont) noexcept(noexcept(cdata(__cont)))
    { return cdata(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_size(_Container& __cont) noexcept(noexcept(size(__cont)))
    { return size(__cont); }

  template <typename _Container>
    constexpr auto
    __adl_empty(_Container& __cont) noexcept(noexcept(empty(__cont)))
    { return empty(__cont); }

#if defined(_GLIBCXX_P1394) && _GLIBCXX_P1394
  template <typename _Container>
    constexpr auto
    __adl_to_address(_Container& __cont) noexcept(noexcept(to_address(__cont)))
    { return to_address(__cont); }
#endif // P1394 and new contiguous_iterator requirements [iterator.concept.contiguous]
#endif // C++20

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // C++11

#endif // _GLIBCXX_RANGE_ACCESS_H

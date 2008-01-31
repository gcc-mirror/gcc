// <algorithm> declarations  -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

/** @file bits/algorithmfwd.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

/*
  adjacent_find
  binary_search
  copy
  copy_backward
  count
  count_if
  equal
  equal_range
  fill
  fill_n
  find
  find_end
  find_first_of
  find_if
  for_each
  generate
  generate_n
  includes
  inplace_merge
  is_heap (C++0x)
  is_heap_until (C++0x)
  is_sorted (C++0x)
  is_sorted_until (C++0x)
  iter_swap
  lexicographical_compare
  lower_bound
  make_heap
  max
  max_element
  merge
  min
  min_element
  minmax (C++0x)
  minmax_element (C++0x)
  mismatch
  next_permutation
  nth_element
  partial_sort
  partial_sort_copy
  partition
  pop_heap
  prev_permutation
  push_heap
  random_shuffle
  remove
  remove_copy
  remove_copy_if
  remove_if
  replace
  replace_copy
  replace_copy_if
  replace_if
  reverse
  reverse_copy
  rotate
  rotate_copy
  search
  search_n
  set_difference
  set_intersection
  set_symmetric_difference
  set_union
  sort
  sort_heap
  stable_partition
  stable_sort
  swap
  swap_ranges
  transform
  unique
  unique_copy
  upper_bound
*/

#ifndef _GLIBCXX_ALGORITHMFWD_H
#define _GLIBCXX_ALGORITHMFWD_H 1

#pragma GCC system_header

#include <bits/c++config.h>
#include <bits/stl_pair.h>
#include <bits/stl_iterator_base_types.h>

_GLIBCXX_BEGIN_NAMESPACE(std)

  // adjacent_find

  template<typename _FIter, typename _Tp>
    bool 
    binary_search(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    bool 
    binary_search(_FIter, _FIter, const _Tp&, _Compare);

  template<typename _IIter, typename _OIter>
    _OIter 
    copy(_IIter, _IIter, _OIter);

  template<typename _BIter1, typename _BIter2>
    _BIter2
    copy_backward(_BIter1, _BIter1, _BIter2);

  // count
  // count_if

  template<typename _FIter, typename _Tp>
    pair<_FIter, _FIter>
    equal_range(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    pair<_FIter, _FIter>
    equal_range(_FIter, _FIter, const _Tp&, _Compare);

  template<typename _FIter, typename _Tp>
    void 
    fill(_FIter, _FIter, const _Tp&);

/*
  XXX NB: return type different from ISO C++.
  template<typename _OIter, typename _Size, typename _Tp>
    void 
    fill_n(_OIter, _Size, const _Tp&);
*/

  template<typename _OIter, typename _Size, typename _Tp>
    _OIter
    fill_n(_OIter, _Size, const _Tp&);

  // find

  template<typename _FIter1, typename _FIter2>
    _FIter1
    find_end(_FIter1, _FIter1, _FIter2, _FIter2);

  template<typename _FIter1, typename _FIter2, typename _BinaryPredicate>
    _FIter1
    find_end(_FIter1, _FIter1, _FIter2, _FIter2, _BinaryPredicate);

  // find_first_of
  // find_if
  // for_each
  // generate
  // generate_n

  template<typename _IIter1, typename _IIter2>
    bool 
    includes(_IIter1, _IIter1, _IIter2, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _Compare>
    bool 
    includes(_IIter1, _IIter1, _IIter2, _IIter2, _Compare);

  template<typename _BIter>
    void 
    inplace_merge(_BIter, _BIter, _BIter);

  template<typename _BIter, typename _Compare>
    void 
    inplace_merge(_BIter, _BIter, _BIter, _Compare);

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  template<typename _RAIter>
    bool 
    is_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    bool 
    is_heap(_RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    _RAIter 
    is_heap_until(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    _RAIter 
    is_heap_until(_RAIter, _RAIter, _Compare);

  template<typename _FIter>
    bool 
    is_sorted(_FIter, _FIter);

  template<typename _FIter, typename _Compare>
    bool 
    is_sorted(_FIter, _FIter, _Compare);

  template<typename _FIter>
    _FIter 
    is_sorted_until(_FIter, _FIter);

  template<typename _FIter, typename _Compare>
    _FIter 
    is_sorted_until(_FIter, _FIter, _Compare);
#endif

  template<typename _FIter1, typename _FIter2>
    void 
    iter_swap(_FIter1, _FIter2);

  template<typename _FIter, typename _Tp>
    _FIter 
    lower_bound(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    _FIter 
    lower_bound(_FIter, _FIter, const _Tp&, _Compare);

  template<typename _RAIter>
    void 
    make_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    make_heap(_RAIter, _RAIter, _Compare);

  template<typename _Tp> 
    const _Tp& 
    max(const _Tp&, const _Tp&);

  template<typename _Tp, typename _Compare>
    const _Tp& 
    max(const _Tp&, const _Tp&, _Compare);

  // max_element
  // merge

  template<typename _Tp> 
    const _Tp& 
    min(const _Tp&, const _Tp&);

  template<typename _Tp, typename _Compare>
    const _Tp& 
    min(const _Tp&, const _Tp&, _Compare);

  // min_element

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  template<typename _Tp>
    pair<const _Tp&, const _Tp&> 
    minmax(const _Tp&, const _Tp&);

  template<typename _Tp, typename _Compare>
    pair<const _Tp&, const _Tp&>
    minmax(const _Tp&, const _Tp&, _Compare);

  template<typename _FIter>
    pair<_FIter, _FIter>
    minmax_element(_FIter, _FIter);

  template<typename _FIter, typename _Compare>
    pair<_FIter, _FIter>
    minmax_element(_FIter, _FIter, _Compare);
#endif

  // mismatch

  template<typename _BIter>
    bool 
    next_permutation(_BIter, _BIter);

  template<typename _BIter, typename _Compare>
    bool 
    next_permutation(_BIter, _BIter, _Compare);

  // nth_element
  // partial_sort

  template<typename _IIter, typename _RAIter>
    _RAIter
    partial_sort_copy(_IIter, _IIter, _RAIter, _RAIter);

  template<typename _IIter, typename _RAIter, typename _Compare>
    _RAIter
    partial_sort_copy(_IIter, _IIter, _RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void 
    pop_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    pop_heap(_RAIter, _RAIter, _Compare);

  template<typename _BIter>
    bool 
    prev_permutation(_BIter, _BIter);

  template<typename _BIter, typename _Compare>
    bool 
    prev_permutation(_BIter, _BIter, _Compare);

  template<typename _RAIter>
    void 
    push_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    push_heap(_RAIter, _RAIter, _Compare);

  // random_shuffle

  template<typename _FIter, typename _Tp>
    _FIter 
    remove(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Predicate>
    _FIter 
    remove_if(_FIter, _FIter, _Predicate);

  template<typename _IIter, typename _OIter, typename _Tp>
    _OIter 
    remove_copy(_IIter, _IIter, _OIter, const _Tp&);

  template<typename _IIter, typename _OIter, typename _Predicate>
    _OIter 
    remove_copy_if(_IIter, _IIter, _OIter, _Predicate);

  // replace

  template<typename _IIter, typename _OIter, typename _Tp>
    _OIter 
    replace_copy(_IIter, _IIter, _OIter, const _Tp&, const _Tp&);

  template<typename _Iter, typename _OIter, typename _Predicate, typename _Tp>
    _OIter 
    replace_copy_if(_Iter, _Iter, _OIter, _Predicate, const _Tp&);

  // replace_if

  template<typename _BIter>
    void 
    reverse(_BIter, _BIter);

  template<typename _BIter, typename _OIter>
    _OIter 
    reverse_copy(_BIter, _BIter, _OIter);

  template<typename _FIter>
    void 
    rotate(_FIter, _FIter, _FIter);

  template<typename _FIter, typename _OIter>
    _OIter 
    rotate_copy(_FIter, _FIter, _FIter, _OIter);

  // search
  // search_n
  // set_difference
  // set_intersection
  // set_symmetric_difference
  // set_union

  template<typename _RAIter>
    void 
    sort_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    sort_heap(_RAIter, _RAIter, _Compare);

  template<typename _BIter, typename _Predicate>
    _BIter 
    stable_partition(_BIter, _BIter, _Predicate);

  template<typename _Tp> 
    void 
    swap(_Tp&, _Tp&);

  template<typename _FIter1, typename _FIter2>
    _FIter2 
    swap_ranges(_FIter1, _FIter1, _FIter2);

  // transform

  template<typename _FIter>
    _FIter 
    unique(_FIter, _FIter);

  template<typename _FIter, typename _BinaryPredicate>
    _FIter 
    unique(_FIter, _FIter, _BinaryPredicate);

  // unique_copy

  template<typename _FIter, typename _Tp>
    _FIter 
    upper_bound(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    _FIter 
    upper_bound(_FIter, _FIter, const _Tp&, _Compare);

_GLIBCXX_END_NAMESPACE

_GLIBCXX_BEGIN_NESTED_NAMESPACE(std, _GLIBCXX_STD_P)

  template<typename _FIter>
    _FIter 
    adjacent_find(_FIter, _FIter);

  template<typename _FIter, typename _BinaryPredicate>
    _FIter 
    adjacent_find(_FIter, _FIter, _BinaryPredicate);

  template<typename _IIter, typename _Tp>
    typename iterator_traits<_IIter>::difference_type
    count(_IIter, _IIter, const _Tp&);

  template<typename _IIter, typename _Predicate>
    typename iterator_traits<_IIter>::difference_type
    count_if(_IIter, _IIter, _Predicate);

  template<typename _IIter1, typename _IIter2>
    bool 
    equal(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    bool 
    equal(_IIter1, _IIter1, _IIter2, _BinaryPredicate);

  template<typename _IIter, typename _Tp>
    _IIter 
    find(_IIter, _IIter, const _Tp&);

  template<typename _FIter1, typename _FIter2>
    _FIter1
    find_first_of(_FIter1, _FIter1, _FIter2, _FIter2);

  template<typename _FIter1, typename _FIter2, typename _BinaryPredicate>
    _FIter1
    find_first_of(_FIter1, _FIter1, _FIter2, _FIter2, _BinaryPredicate);

  template<typename _IIter, typename _Predicate>
    _IIter 
    find_if(_IIter, _IIter, _Predicate);

  template<typename _IIter, typename _Funct>
    _Funct 
    for_each(_IIter, _IIter, _Funct);

  template<typename _FIter, typename _Generator>
    void 
    generate(_FIter, _FIter, _Generator);

/*
  XXX NB: return type different from ISO C++.
  template<typename _OIter, typename _Size, typename _Tp>
    void
    generate_n(_OIter, _Size, _Generator);
*/

  template<typename _OIter, typename _Size, typename _Generator>
    _OIter
    generate_n(_OIter, _Size, _Generator);

  template<typename _IIter1, typename _IIter2>
    bool 
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _Compare>
    bool 
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2, _Compare);

  template<typename _FIter>
    _FIter 
    max_element(_FIter, _FIter);

  template<typename _FIter, typename _Compare>
    _FIter 
    max_element(_FIter, _FIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _Compare>
    _OIter 
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _FIter>
    _FIter 
    min_element(_FIter, _FIter);

  template<typename _FIter, typename _Compare>
    _FIter 
    min_element(_FIter, _FIter, _Compare);

  template<typename _IIter1, typename _IIter2>
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2, _BinaryPredicate);

  template<typename _RAIter>
    void 
    nth_element(_RAIter, _RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    nth_element(_RAIter, _RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void 
    partial_sort(_RAIter, _RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    partial_sort(_RAIter, _RAIter, _RAIter, _Compare);

  template<typename _BIter, typename _Predicate>
    _BIter 
    partition(_BIter, _BIter, _Predicate);

  template<typename _RAIter>
    void 
    random_shuffle(_RAIter, _RAIter);

  template<typename _RAIter, typename _Generator>
    void 
    random_shuffle(_RAIter, _RAIter, _Generator&);

  template<typename _FIter, typename _Tp>
    void 
    replace(_FIter, _FIter, const _Tp&, const _Tp&);

  template<typename _FIter, typename _Predicate, typename _Tp>
    void 
    replace_if(_FIter, _FIter, _Predicate, const _Tp&);

  template<typename _FIter1, typename _FIter2>
    _FIter1 
    search(_FIter1, _FIter1, _FIter2, _FIter2);

  template<typename _FIter1, typename _FIter2, typename _BinaryPredicate>
    _FIter1 
    search(_FIter1, _FIter1, _FIter2, _FIter2, _BinaryPredicate);

  template<typename _FIter, typename _Size, typename _Tp>
    _FIter 
    search_n(_FIter, _FIter, _Size, const _Tp&);

  template<typename _FIter, typename _Size, typename _Tp, 
	   typename _BinaryPredicate>
    _FIter 
    search_n(_FIter, _FIter, _Size, const _Tp&, _BinaryPredicate);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _Compare>
    _OIter 
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare>
    _OIter 
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _Compare>
    _OIter
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, 
			     _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare>
    _OIter 
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _RAIter>
    void 
    sort(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    sort(_RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void 
    stable_sort(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    stable_sort(_RAIter, _RAIter, _Compare);

  template<typename _IIter, typename _OIter, typename _UnaryOperation>
    _OIter 
    transform(_IIter, _IIter, _OIter, _UnaryOperation);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _BinaryOperation>
    _OIter 
    transform(_IIter1, _IIter1, _IIter2, _OIter, _BinaryOperation);

  template<typename _IIter, typename _OIter>
    _OIter 
    unique_copy(_IIter, _IIter, _OIter);

  template<typename _IIter, typename _OIter, typename _BinaryPredicate>
    _OIter 
    unique_copy(_IIter, _IIter, _OIter, _BinaryPredicate);

_GLIBCXX_END_NESTED_NAMESPACE

#ifdef _GLIBCXX_NAMESPACE_ASSOCIATION_PARALLEL
# include <parallel/algorithmfwd.h>
#endif

#endif


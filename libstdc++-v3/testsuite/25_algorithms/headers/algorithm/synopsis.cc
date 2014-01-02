// { dg-do compile }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

#include <algorithm>

namespace std
 {
  // 25.1, non-modifying sequence operations:
  template<typename _IIter, typename _Funct>
    _Funct 
    for_each(_IIter, _IIter, _Funct);

  template<typename _IIter, typename _Tp>
    _IIter 
    find(_IIter, _IIter, const _Tp&);

  template<typename _IIter, typename _Predicate>
    _IIter
    find_if(_IIter, _IIter, _Predicate);

#if __cplusplus >= 201103L
  template<typename _IIter, typename _Predicate>
    bool
    all_of(_IIter, _IIter, _Predicate);

  template<typename _IIter, typename _Predicate>
    bool
    any_of(_IIter, _IIter, _Predicate);

  template<typename _IIter, typename _Predicate>
    bool
    none_of(_IIter, _IIter, _Predicate);

  template<typename _IIter, typename _Predicate>
    _IIter
    find_if_not(_IIter, _IIter, _Predicate);

  template<typename _IIter, typename _Predicate>
    bool
    is_partitioned(_IIter, _IIter, _Predicate);

  template<typename _FIter, typename _Predicate>
    _FIter
    partition_point(_FIter, _FIter, _Predicate);
#endif

  template<typename _FIter1, typename _FIter2>
    _FIter1
    find_end(_FIter1, _FIter1, _FIter2, _FIter2);

  template<typename _FIter1, typename _FIter2, typename _BinaryPredicate>
    _FIter1
    find_end(_FIter1, _FIter1, _FIter2, _FIter2, _BinaryPredicate);

  template<typename _FIter1, typename _FIter2>
    _FIter1
    find_first_of(_FIter1, _FIter1, _FIter2, _FIter2);

  template<typename _FIter1, typename _FIter2, typename _BinaryPredicate>
    _FIter1
    find_first_of(_FIter1, _FIter1, _FIter2, _FIter2, _BinaryPredicate);

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
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2, _BinaryPredicate);

  template<typename _IIter1, typename _IIter2>
    bool 
    equal(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    bool 
    equal(_IIter1, _IIter1, _IIter2, _BinaryPredicate);

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

  // 25.2, modifying sequence operations:
  // 25.2.1, copy:
  template<typename _IIter, typename _OIter>
    _OIter 
    copy(_IIter, _IIter, _OIter);

  template<typename _BIter1, typename _BIter2>
    _BIter2
    copy_backward (_BIter1, _BIter1, _BIter2);

  // 25.2.2, swap:
  template<typename _Tp> 
    void 
    swap(_Tp&, _Tp& b);

#if __cplusplus >= 201103L
  template<typename _Tp, size_t _Nm>
    void
    swap(_Tp (&)[_Nm], _Tp (&)[_Nm]);
#endif

  template<typename _FIter1, typename _FIter2>
    _FIter2 
    swap_ranges(_FIter1 first1, _FIter1, _FIter2);

  template<typename _FIter1, typename _FIter2>
    void 
    iter_swap(_FIter1, _FIter2 b);

  template<typename _IIter, typename _OIter, typename _UnaryOperation>
    _OIter 
    transform(_IIter, _IIter, _OIter, _UnaryOperation op);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _BinaryOperation>
    _OIter 
    transform(_IIter1, _IIter1, _IIter2, _OIter, _BinaryOperation);

  template<typename _FIter, typename _Tp>
    void 
    replace(_FIter, _FIter, const _Tp&, const _Tp&);

  template<typename _FIter, typename _Predicate, typename _Tp>
    void 
    replace_if(_FIter, _FIter, _Predicate, const _Tp&);

  template<typename _IIter, typename _OIter, typename _Tp>
    _OIter 
    replace_copy(_IIter, _IIter, _OIter, const _Tp&, const _Tp&);

  template<typename _Iter, typename _OIter, typename _Predicate, typename _Tp>
    _OIter 
    replace_copy_if(_Iter, _Iter, _OIter, _Predicate, const _Tp&);

  template<typename _FIter, typename _Tp>
    void 
    fill(_FIter, _FIter, const _Tp&);

  template<typename _OIter, typename _Size, typename _Tp>
    void 
    fill_n(_OIter, _Size n, const _Tp&);

  template<typename _FIter, typename _Generator>
    void 
    generate(_FIter, _FIter, _Generator);

  template<typename _OIter, typename _Size, typename _Generator>
    void 
    generate_n(_OIter, _Size, _Generator);

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

#if __cplusplus >= 201103L
  template<typename _IIter, typename _OIter, typename _Predicate>
    _OIter 
    copy_if(_IIter, _IIter, _OIter, _Predicate);

  template<typename _IIter, typename _Size, typename _OIter>
    _OIter
    copy_n(_IIter, _Size, _OIter);

  template<typename _IIter, typename _OIter1,
	   typename _OIter2, typename _Predicate>
    pair<_OIter1, _OIter2>
    partition_copy(_IIter, _IIter, _OIter1, _OIter2, _Predicate);
#endif

  template<typename _FIter>
    _FIter 
    unique(_FIter, _FIter);

  template<typename _FIter, typename _BinaryPredicate>
    _FIter 
    unique(_FIter, _FIter, _BinaryPredicate);

  template<typename _IIter, typename _OIter>
    _OIter 
    unique_copy(_IIter, _IIter, _OIter);

  template<typename _IIter, typename _OIter, typename _BinaryPredicate>
    _OIter 
    unique_copy(_IIter, _IIter, _OIter, _BinaryPredicate);

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
    rotate_copy (_FIter, _FIter, _FIter, _OIter);

  template<typename _RAIter>
    void 
    random_shuffle(_RAIter, _RAIter);

  template<typename _RAIter, typename _Generator>
    void 
    random_shuffle(_RAIter, _RAIter, _Generator&);

  // 25.2.12, partitions:
  template<typename _BIter, typename _Predicate>
    _BIter 
    partition(_BIter, _BIter, _Predicate);

  template<typename _BIter, typename _Predicate>
    _BIter 
    stable_partition(_BIter, _BIter, _Predicate);

  // 25.3, sorting and related operations:
  // 25.3.1, sorting:
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

  template<typename _RAIter>
    void 
    partial_sort(_RAIter, _RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    partial_sort(_RAIter, _RAIter, _RAIter, _Compare);

  template<typename _IIter, typename _RAIter>
    _RAIter
    partial_sort_copy(_IIter, _IIter, _RAIter, _RAIter);

  template<typename _IIter, typename _RAIter, typename _Compare>
    _RAIter
    partial_sort_copy(_IIter, _IIter, _RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void 
    nth_element(_RAIter, _RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    nth_element(_RAIter, _RAIter, _RAIter, _Compare);

  // 25.3.3, binary search:
  template<typename _FIter, typename _Tp>
    _FIter 
    lower_bound(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    _FIter 
    lower_bound(_FIter, _FIter, const _Tp&, _Compare);

  template<typename _FIter, typename _Tp>
    _FIter 
    upper_bound(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    _FIter 
    upper_bound(_FIter, _FIter, const _Tp&, _Compare);

  template<typename _FIter, typename _Tp>
    pair<_FIter, _FIter>
    equal_range(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    pair<_FIter, _FIter>
    equal_range(_FIter, _FIter, const _Tp&, _Compare);

  template<typename _FIter, typename _Tp>
    bool 
    binary_search(_FIter, _FIter, const _Tp&);

  template<typename _FIter, typename _Tp, typename _Compare>
    bool 
    binary_search(_FIter, _FIter, const _Tp&, _Compare);

  // 25.3.4, merge:
  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _Compare>
    _OIter 
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _BIter>
    void 
    inplace_merge(_BIter, _BIter, _BIter);

  template<typename _BIter, typename _Compare>
    void 
    inplace_merge(_BIter, _BIter, _BIter, _Compare);

  // 25.3.5, set operations:
  template<typename _IIter1, typename _IIter2>
    bool 
    includes(_IIter1, _IIter1, _IIter2, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _Compare>
    bool 
    includes(_IIter1, _IIter1, _IIter2, _IIter2, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare>
    _OIter 
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare>
    _OIter 
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _Compare>
    _OIter 
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, 
	   typename _Compare>
    _OIter
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, 
			     _OIter, _Compare);

  // 25.3.6, heap operations:
  template<typename _RAIter>
    void 
    push_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    push_heap(_RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void 
    pop_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    pop_heap(_RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void 
    make_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    make_heap(_RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void 
    sort_heap(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void 
    sort_heap(_RAIter, _RAIter, _Compare);

#if __cplusplus >= 201103L
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

  // 25.3.7, minimum and maximum:
  template<typename _Tp> 
    const _Tp& 
    min(const _Tp&, const _Tp&);

  template<typename _Tp, typename _Compare>
    const _Tp& 
    min(const _Tp&, const _Tp&, _Compare);

  template<typename _Tp> 
    const _Tp& 
    max(const _Tp&, const _Tp&);

  template<typename _Tp, typename _Compare>
    const _Tp& 
    max(const _Tp&, const _Tp&, _Compare);

  template<typename _FIter>
    _FIter 
    min_element(_FIter, _FIter);

  template<typename _FIter, typename _Compare>
    _FIter 
    min_element(_FIter, _FIter, _Compare);

  template<typename _FIter>
    _FIter 
    max_element(_FIter, _FIter);

  template<typename _FIter, typename _Compare>
    _FIter 
    max_element(_FIter, _FIter, _Compare);

#if __cplusplus >= 201103L
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

  template<typename _Tp>
    _Tp
    min(initializer_list<_Tp>);

  template<typename _Tp, typename _Compare>
    _Tp
    min(initializer_list<_Tp>, _Compare);

  template<typename _Tp>
    _Tp
    max(initializer_list<_Tp>);

  template<typename _Tp, typename _Compare>
    _Tp
    max(initializer_list<_Tp>, _Compare);

  template<typename _Tp>
    pair<_Tp, _Tp>
    minmax(initializer_list<_Tp>);

  template<typename _Tp, typename _Compare>
    pair<_Tp, _Tp>
    minmax(initializer_list<_Tp>, _Compare);
#endif

  template<typename _IIter1, typename _IIter2>
    bool 
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _Compare>
    bool 
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2, _Compare);

  // 25.3.9, permutations
  template<typename _BIter>
    bool 
    next_permutation(_BIter, _BIter);

  template<typename _BIter, typename _Compare>
    bool 
    next_permutation(_BIter, _BIter, _Compare);

  template<typename _BIter>
    bool 
    prev_permutation(_BIter, _BIter);

  template<typename _BIter, typename _Compare>
    bool 
    prev_permutation(_BIter, _BIter, _Compare);
}

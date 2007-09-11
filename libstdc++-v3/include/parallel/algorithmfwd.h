// <algorithm> parallel extensions -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

/** @file parallel/algorithmfwd.h
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_PARALLEL_ALGORITHMFWD_H
#define _GLIBCXX_PARALLEL_ALGORITHMFWD_H 1

#pragma GCC system_header

#include <parallel/tags.h>
#include <parallel/settings.h>

namespace std
{
namespace __parallel
{
  template<typename _FIter>
  inline _FIter
  adjacent_find(_FIter, _FIter, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename BinaryPredicate>
  inline _FIter
  adjacent_find(_FIter, _FIter, BinaryPredicate, __gnu_parallel::sequential_tag);

  template<typename _FIter>
  inline _FIter
  adjacent_find(_FIter, _FIter);

  template<typename _FIter, typename BinaryPredicate>
  inline _FIter
  adjacent_find(_FIter, _FIter, BinaryPredicate);

  template<typename _RAIter>
  _RAIter
  adjacent_find_switch(_RAIter, _RAIter, random_access_iterator_tag);

  template<typename _FIter, typename IteratorTag>
  inline _FIter
  adjacent_find_switch(_FIter, _FIter, IteratorTag);

  template<typename _FIter, typename BinaryPredicate, typename IteratorTag>
  inline _FIter
  adjacent_find_switch(_FIter, _FIter, BinaryPredicate, IteratorTag);

  template<typename _RAIter, typename BinaryPredicate>
  _RAIter
  adjacent_find_switch(_RAIter, _RAIter, BinaryPredicate, random_access_iterator_tag);


  template<typename _IIter, typename T>
  inline typename iterator_traits<_IIter>::difference_type
  count(_IIter, _IIter, const T& value, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename T>
  inline typename iterator_traits<_IIter>::difference_type
  count(_IIter, _IIter, const T& value, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_unbalanced);

  template<typename _RAIter, typename T>
  typename iterator_traits<_RAIter>::difference_type
  count_switch(_RAIter, _RAIter, const T& value, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _IIter, typename T, typename IteratorTag>
  typename iterator_traits<_IIter>::difference_type
  count_switch(_IIter, _IIter, const T& value, IteratorTag, __gnu_parallel::parallelism);


  template<typename _IIter, typename Predicate>
  inline typename iterator_traits<_IIter>::difference_type
  count_if(_IIter, _IIter, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename Predicate>
  inline typename iterator_traits<_IIter>::difference_type
  count_if(_IIter, _IIter, Predicate, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_unbalanced);

  template<typename _RAIter, typename Predicate>
  typename iterator_traits<_RAIter>::difference_type
  count_if_switch(_RAIter, _RAIter, Predicate, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _IIter, typename Predicate, typename IteratorTag>
  typename iterator_traits<_IIter>::difference_type
  count_if_switch(_IIter, _IIter, Predicate, IteratorTag, __gnu_parallel::parallelism);

  // algobase.h
  template<typename _IIter1, typename _IIter2>
  inline bool
  equal(_IIter1, _IIter1, _IIter2, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename Predicate>
  inline bool
  equal(_IIter1, _IIter1, _IIter2, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2>
  inline bool
  equal(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename Predicate>
  inline bool
  equal(_IIter1, _IIter1, _IIter2, Predicate);

  template<typename _IIter, typename T>
  inline _IIter
  find(_IIter, _IIter, const T&, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename T>
  inline _IIter
  find(_IIter, _IIter, const T& val);

  template<typename _IIter, typename T, typename IteratorTag>
  inline _IIter
  find_switch(_IIter, _IIter, const T&, IteratorTag);

  template<typename _RAIter, typename T>
  _RAIter
  find_switch(_RAIter, _RAIter, const T&, random_access_iterator_tag);

  template<typename _IIter, typename Predicate>
  inline _IIter
  find_if(_IIter, _IIter, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename Predicate>
  inline _IIter
  find_if (_IIter, _IIter, Predicate);

  template<typename _IIter, typename Predicate, typename IteratorTag>
  inline _IIter
  find_if_switch(_IIter, _IIter, Predicate, IteratorTag);

  template<typename _RAIter, typename Predicate>
  _RAIter
  find_if_switch(_RAIter, _RAIter, Predicate, random_access_iterator_tag);

  template<typename _IIter, typename _FIter>
  inline _IIter
  find_first_of(_IIter, _IIter, _FIter, _FIter, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _FIter, typename BinaryPredicate>
  inline _IIter
  find_first_of(_IIter, _IIter, _FIter, _FIter, BinaryPredicate, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _FIter, typename BinaryPredicate>
  inline _IIter
  find_first_of(_IIter, _IIter, _FIter, _FIter, BinaryPredicate);

  template<typename _IIter, typename _FIter>
  _IIter
  find_first_of(_IIter, _IIter, _FIter, _FIter);

  template<typename _IIter, typename _FIter, typename IteratorTag1, typename IteratorTag2>
  inline _IIter
  find_first_of_switch(_IIter, _IIter, _FIter, _FIter, IteratorTag1, IteratorTag2);

  template<typename _RAIter, typename _FIter, typename BinaryPredicate, typename IteratorTag>
  inline _RAIter
  find_first_of_switch(_RAIter, _RAIter, _FIter, _FIter, BinaryPredicate, random_access_iterator_tag, IteratorTag);

  template<typename _IIter, typename _FIter, typename BinaryPredicate, typename IteratorTag1, typename IteratorTag2>
  inline _IIter
  find_first_of_switch(_IIter, _IIter, _FIter, _FIter, BinaryPredicate, IteratorTag1, IteratorTag2);


  template<typename _IIter, typename Function>
  inline Function
  for_each(_IIter, _IIter, Function f, __gnu_parallel::sequential_tag);

  template<typename Iterator, typename Function>
  inline Function
  for_each(Iterator, Iterator, Function f, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _IIter, typename Function, typename IteratorTag>
  Function
  for_each_switch(_IIter, _IIter, Function f, IteratorTag, __gnu_parallel::parallelism);

  template<typename _RAIter, typename Function>
  Function
  for_each_switch(_RAIter, _RAIter, Function f, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _FIter, typename Generator>
  inline void
  generate(_FIter, _FIter, Generator, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename Generator>
  inline void
  generate(_FIter, _FIter, Generator, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _FIter, typename Generator, typename IteratorTag>
  void
  generate_switch(_FIter, _FIter, Generator, IteratorTag, __gnu_parallel::parallelism);

  template<typename _RAIter, typename Generator>
  void
  generate_switch(_RAIter, _RAIter, Generator, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _OIter, typename Size, typename Generator>
  inline _OIter
  generate_n(_OIter, Size, Generator, __gnu_parallel::sequential_tag);

  template<typename _OIter, typename Size, typename Generator>
  inline _OIter
  generate_n(_OIter, Size, Generator, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _OIter, typename Size, typename Generator, typename IteratorTag>
  _OIter
  generate_n_switch(_OIter, Size, Generator, IteratorTag, __gnu_parallel::parallelism);

  template<typename _RAIter, typename Size, typename Generator>
  _RAIter
  generate_n_switch(_RAIter, Size, Generator, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _IIter1, typename _IIter2>
  inline bool
  lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename Predicate>
  inline bool
  lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2>
  inline bool
  lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2);

  template<typename _IIter1, typename _IIter2, typename Predicate>
  inline bool
  lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2, Predicate);

  template<typename _IIter1, typename _IIter2, typename Predicate, typename IteratorTag1, typename IteratorTag2>
  inline bool
  lexicographical_compare_switch(_IIter1, _IIter1, _IIter2, _IIter2, Predicate, IteratorTag1, IteratorTag2);

  template<typename _RAIter1, typename _RAIter2, typename Predicate>
  bool
  lexicographical_compare_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, Predicate, random_access_iterator_tag, random_access_iterator_tag);

  // algo.h
  template<typename _IIter1, typename _IIter2>
  inline pair<_IIter1, _IIter2>
  mismatch(_IIter1, _IIter1, _IIter2, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename Predicate>
  inline pair<_IIter1, _IIter2>
  mismatch(_IIter1, _IIter1, _IIter2, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2>
  inline pair<_IIter1, _IIter2>
  mismatch(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename Predicate>
  inline pair<_IIter1, _IIter2>
  mismatch(_IIter1, _IIter1, _IIter2, Predicate);

  template<typename _IIter1, typename _IIter2, typename Predicate, typename IteratorTag1, typename IteratorTag2>
  inline pair<_IIter1, _IIter2>
  mismatch_switch(_IIter1, _IIter1, _IIter2, Predicate, IteratorTag1, IteratorTag2);

  template<typename _RAIter1, typename _RAIter2, typename Predicate>
  pair<_RAIter1, _RAIter2>
  mismatch_switch(_RAIter1, _RAIter1, _RAIter2, Predicate, random_access_iterator_tag, random_access_iterator_tag);

  template<typename _FIter1, typename _FIter2>
  inline _FIter1
  search(_FIter1, _FIter1, _FIter2, _FIter2, __gnu_parallel::sequential_tag);

  template<typename _FIter1, typename _FIter2>
  inline _FIter1
  search(_FIter1, _FIter1, _FIter2, _FIter2);

  template<typename _FIter1, typename _FIter2, typename BinaryPredicate>
  inline _FIter1
  search(_FIter1, _FIter1, _FIter2, _FIter2, BinaryPredicate, __gnu_parallel::sequential_tag);

  template<typename _FIter1, typename _FIter2, typename BinaryPredicate>
  inline _FIter1
  search(_FIter1, _FIter1, _FIter2, _FIter2, BinaryPredicate);

  template<typename _RAIter1, typename _RAIter2>
  _RAIter1
  search_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, random_access_iterator_tag, random_access_iterator_tag);

  template<typename _FIter1, typename _FIter2, typename IteratorTag1, typename IteratorTag2>
  inline _FIter1
  search_switch(_FIter1, _FIter1, _FIter2, _FIter2, IteratorTag1, IteratorTag2);

  template<typename _RAIter1, typename _RAIter2, typename BinaryPredicate>
  _RAIter1
  search_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, BinaryPredicate , random_access_iterator_tag, random_access_iterator_tag);

  template<typename _FIter1, typename _FIter2, typename BinaryPredicate, typename IteratorTag1, typename IteratorTag2>
  inline _FIter1
  search_switch(_FIter1, _FIter1, _FIter2, _FIter2, BinaryPredicate, IteratorTag1, IteratorTag2);

  template<typename _FIter, typename Integer, typename T>
  inline _FIter
  search_n(_FIter, _FIter, Integer, const T&, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename Integer, typename T, typename BinaryPredicate>
  inline _FIter
  search_n(_FIter, _FIter, Integer, const T&, BinaryPredicate, __gnu_parallel::sequential_tag);
    
  template<typename _FIter, typename Integer, typename T>
  inline _FIter
  search_n(_FIter, _FIter, Integer, const T& val);

  template<typename _FIter, typename Integer, typename T, typename BinaryPredicate>
  inline _FIter
  search_n(_FIter, _FIter, Integer, const T&, BinaryPredicate);

  template<typename _RAIter, typename Integer, typename T, typename BinaryPredicate>
  _RAIter
  search_n_switch(_RAIter, _RAIter, Integer, const T&, BinaryPredicate, random_access_iterator_tag);

  template<typename _FIter, typename Integer, typename T, typename BinaryPredicate, typename IteratorTag>
  inline _FIter
  search_n_switch(_FIter, _FIter, Integer, const T&, BinaryPredicate, IteratorTag);


  template<typename _IIter, typename _OIter, typename UnaryOperation>
  inline _OIter
  transform(_IIter, _IIter, _OIter, UnaryOperation, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename BinaryOperation>
  inline _OIter
  transform(_IIter1, _IIter1, _IIter2, _OIter, BinaryOperation binary_op, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename UnaryOperation>
  inline _OIter
  transform(_IIter, _IIter, _OIter, UnaryOperation, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename BinaryOperation>
  inline _OIter
  transform(_IIter1, _IIter1, _IIter2, _OIter, BinaryOperation binary_op, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _RAIter1, typename _RAIter3, typename UnaryOperation>
  _RAIter3
  transform1_switch(_RAIter1, _RAIter1, _RAIter3, UnaryOperation, random_access_iterator_tag, random_access_iterator_tag, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _RAIter1, typename _RAIter3, typename UnaryOperation, typename IteratorTag1, typename IteratorTag2>
  inline _RAIter3
  transform1_switch(_RAIter1, _RAIter1, _RAIter3, UnaryOperation, IteratorTag1, IteratorTag2, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);
    
  template<typename _RAIter1, typename _RAIter2, typename _RAIter3, typename BinaryOperation>
  _RAIter3
  transform2_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter3, BinaryOperation binary_op, random_access_iterator_tag, random_access_iterator_tag, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _RAIter1, typename _RAIter2, typename _RAIter3, typename BinaryOperation, typename tag1, typename tag2, typename tag3>
  inline _RAIter3
  transform2_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter3, BinaryOperation binary_op, tag1, tag2, tag3, __gnu_parallel::parallelism);

  template<typename _FIter, typename T>
  inline void
  replace(_FIter, _FIter, const T&, const T&, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename T>
  inline void
  replace(_FIter, _FIter, const T&, const T&, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _FIter, typename T, typename IteratorTag>
  void
  replace_switch(_FIter, _FIter, const T&, const T&, IteratorTag, __gnu_parallel::parallelism);

  template<typename _RAIter, typename T>
  void
  replace_switch(_RAIter, _RAIter, const T&, const T&, random_access_iterator_tag, __gnu_parallel::parallelism);


  template<typename _FIter, typename Predicate, typename T>
  inline void
  replace_if(_FIter, _FIter, Predicate, const T&, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename Predicate, typename T>
  inline void
  replace_if(_FIter, _FIter, Predicate, const T&, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _FIter, typename Predicate, typename T, typename IteratorTag>
  void
  replace_if_switch(_FIter, _FIter, Predicate, const T&, IteratorTag, __gnu_parallel::parallelism);

  template<typename _RAIter, typename Predicate, typename T>
  void
  replace_if_switch(_RAIter, _RAIter, Predicate, const T&, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _FIter>
  inline _FIter
  max_element(_FIter, _FIter, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Compare>
  inline _FIter
  max_element(_FIter, _FIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _FIter>
  inline _FIter
  max_element(_FIter, _FIter, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _FIter, typename _Compare>
  inline _FIter
  max_element(_FIter, _FIter, _Compare, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _FIter, typename _Compare, typename IteratorTag>
  _FIter
  max_element_switch(_FIter, _FIter, _Compare, IteratorTag, __gnu_parallel::parallelism);

  template<typename _RAIter, typename _Compare>
  _RAIter
  max_element_switch(_RAIter, _RAIter, _Compare, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter
  merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename _Compare>
  inline _OIter
  merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename _Compare>
  inline _OIter
  merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter
  merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename _Compare, typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
  inline _OIter
  merge_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare, IteratorTag1, IteratorTag2, IteratorTag3);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename _Compare>
  _OIter
  merge_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare, random_access_iterator_tag, random_access_iterator_tag, random_access_iterator_tag);

  template<typename _FIter>
  inline _FIter
  min_element(_FIter, _FIter, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Compare>
  inline _FIter
  min_element(_FIter, _FIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _FIter>
  inline _FIter
  min_element(_FIter, _FIter, __gnu_parallel::parallelism parallelism_tag  = __gnu_parallel::parallel_balanced);

  template<typename _FIter, typename _Compare>
  inline _FIter
  min_element(_FIter, _FIter, _Compare, __gnu_parallel::parallelism parallelism_tag = __gnu_parallel::parallel_balanced);

  template<typename _FIter, typename _Compare, typename IteratorTag>
  _FIter
  min_element_switch(_FIter, _FIter, _Compare, IteratorTag, __gnu_parallel::parallelism);

  template<typename _RAIter, typename _Compare>
  _RAIter
  min_element_switch(_RAIter, _RAIter, _Compare, random_access_iterator_tag, __gnu_parallel::parallelism);

  template<typename _RAIter>
  inline void
  nth_element(_RAIter, _RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
  void
  nth_element(_RAIter, _RAIter, _RAIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
  inline void
  nth_element(_RAIter, _RAIter, _RAIter, _Compare);

  template<typename _RAIter>
  void
  nth_element(_RAIter, _RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
  void
  partial_sort(_RAIter, _RAIter, _RAIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _RAIter>
  void
  partial_sort(_RAIter, _RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
  void
  partial_sort(_RAIter, _RAIter, _RAIter, _Compare);

  template<typename _RAIter>
  void
  partial_sort(_RAIter, _RAIter, _RAIter);

  template<typename _FIter, typename Predicate>
  inline _FIter
  partition(_FIter, _FIter, Predicate, __gnu_parallel::sequential_tag);
    
  template<typename _FIter, typename Predicate>
  inline _FIter
  partition(_FIter, _FIter, Predicate);

  template<typename _FIter, typename Predicate, typename IteratorTag>
  inline _FIter
  partition_switch(_FIter, _FIter, Predicate, IteratorTag);
    
  template<typename _RAIter, typename Predicate>
  _RAIter
  partition_switch(_RAIter, _RAIter, Predicate, random_access_iterator_tag);

  template<typename _RAIter>
  inline void
  random_shuffle(_RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename RandomNumberGenerator>
  inline void
  random_shuffle(_RAIter, _RAIter, RandomNumberGenerator& rand, __gnu_parallel::sequential_tag);

  template<typename _RAIter>
  inline void
  random_shuffle(_RAIter, _RAIter);

  template<typename _RAIter, typename RandomNumberGenerator>
  void
  random_shuffle(_RAIter, _RAIter, RandomNumberGenerator& rand);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter
  set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter
  set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter 
  set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter 
  set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate);

  template<typename _IIter1, typename _IIter2, typename Predicate, typename _OIter, typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
  inline _OIter 
  set_union_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, IteratorTag1, IteratorTag2, IteratorTag3);

  template<typename _RAIter1, typename _RAIter2, typename Output_RAIter, typename Predicate>
  Output_RAIter 
  set_union_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, Output_RAIter, Predicate, random_access_iterator_tag, random_access_iterator_tag, random_access_iterator_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter
  set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter
  set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter 
  set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter 
  set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate);

  template<typename _IIter1, typename _IIter2, typename Predicate, typename _OIter, typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
  inline _OIter 
  set_intersection_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, IteratorTag1, IteratorTag2, IteratorTag3);

  template<typename _RAIter1, typename _RAIter2, typename Output_RAIter, typename Predicate>
  Output_RAIter 
  set_intersection_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, Output_RAIter, Predicate, random_access_iterator_tag, random_access_iterator_tag, random_access_iterator_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter
  set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter
  set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter 
  set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter 
  set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate);

  template<typename _IIter1, typename _IIter2, typename Predicate, typename _OIter, typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
  inline _OIter 
  set_symmetric_difference_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, IteratorTag1, IteratorTag2, IteratorTag3);

  template<typename _RAIter1, typename _RAIter2, typename Output_RAIter, typename Predicate>
  Output_RAIter 
  set_symmetric_difference_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, Output_RAIter, Predicate, random_access_iterator_tag, random_access_iterator_tag, random_access_iterator_tag);


  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter
  set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter
  set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
  inline _OIter
  set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter, typename Predicate>
  inline _OIter
  set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate);

  template<typename _IIter1, typename _IIter2, typename Predicate, typename _OIter, typename IteratorTag1, typename IteratorTag2, typename IteratorTag3>
  inline _OIter
  set_difference_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate, IteratorTag1, IteratorTag2, IteratorTag3);

  template<typename _RAIter1, typename _RAIter2, typename Output_RAIter, typename Predicate>
  Output_RAIter
  set_difference_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, Output_RAIter, Predicate, random_access_iterator_tag, random_access_iterator_tag, random_access_iterator_tag);


  template<typename _RAIter>
  inline void
  sort(_RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
  inline void
  sort(_RAIter, _RAIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _RAIter>
  inline void
  sort(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
  void
  sort(_RAIter, _RAIter, _Compare);

  template<typename _RAIter>
  inline void
  stable_sort(_RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
  inline void
  stable_sort(_RAIter, _RAIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _RAIter>
  void
  stable_sort(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
  void
  stable_sort(_RAIter, _RAIter, _Compare);

  template<typename _IIter, typename _OIter>
  inline _OIter
  unique_copy(_IIter, _IIter, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename Predicate>
  inline _OIter
  unique_copy(_IIter, _IIter, _OIter, Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter>
  inline _OIter
  unique_copy(_IIter, _IIter, _OIter);

  template<typename _IIter, typename _OIter, typename Predicate>
  inline _OIter
  unique_copy(_IIter, _IIter, _OIter, Predicate);

  template<typename _IIter, typename _OIter, typename Predicate, typename IteratorTag1, typename IteratorTag2>
  inline _OIter
  unique_copy_switch(_IIter, _IIter, _OIter, Predicate, IteratorTag1, IteratorTag2);

  template<typename _RAIter, typename RandomAccess_OIter, typename Predicate>
  RandomAccess_OIter
  unique_copy_switch(_RAIter, _RAIter, RandomAccess_OIter, Predicate, random_access_iterator_tag, random_access_iterator_tag);
} // end namespace __parallel
} // end namespace std

// NB: cannot use _GLIBCXX_STD_P directly here, as it is both scoped
// (std::__norm) and unscoped (std::).
namespace __gnu_sequential
{
#ifdef _GLIBCXX_PARALLEL
  using std::__norm::partition;
  using std::__norm::sort;
  using std::__norm::stable_sort;
  using std::__norm::random_shuffle;
#else
  using std::partition;
  using std::sort;
  using std::stable_sort;
  using std::random_shuffle;    
#endif    
}

#endif

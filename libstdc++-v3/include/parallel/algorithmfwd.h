// <algorithm> parallel extensions -*- C++ -*-

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
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
    _FIter
    adjacent_find(_FIter, _FIter);

  template<typename _FIter>
    _FIter
    adjacent_find(_FIter, _FIter, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _IterTag>
    _FIter
    adjacent_find_switch(_FIter, _FIter, _IterTag);

  template<typename _RAIter>
    _RAIter
    adjacent_find_switch(_RAIter, _RAIter, random_access_iterator_tag);


  template<typename _FIter, typename _BiPredicate>
    _FIter
    adjacent_find(_FIter, _FIter, _BiPredicate);

  template<typename _FIter, typename _BiPredicate>
    _FIter
    adjacent_find(_FIter, _FIter, _BiPredicate,
		  __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _BiPredicate, typename _IterTag>
    _FIter
    adjacent_find_switch(_FIter, _FIter, _BiPredicate, _IterTag);

  template<typename _RAIter, typename _BiPredicate>
    _RAIter
    adjacent_find_switch(_RAIter, _RAIter, _BiPredicate, 
			 random_access_iterator_tag);


  template<typename _IIter, typename _Tp>
    typename iterator_traits<_IIter>::difference_type
    count(_IIter, _IIter, const _Tp&);

  template<typename _IIter, typename _Tp>
    typename iterator_traits<_IIter>::difference_type
    count(_IIter, _IIter, const _Tp&, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _Tp>
    typename iterator_traits<_IIter>::difference_type
    count(_IIter, _IIter, const _Tp&, __gnu_parallel::_Parallelism);

  template<typename _IIter, typename _Tp, typename _IterTag>
    typename iterator_traits<_IIter>::difference_type
    count_switch(_IIter, _IIter, const _Tp&, _IterTag);

  template<typename _RAIter, typename _Tp>
    typename iterator_traits<_RAIter>::difference_type
    count_switch(_RAIter, _RAIter, const _Tp&, random_access_iterator_tag,
		 __gnu_parallel::_Parallelism);


  template<typename _IIter, typename _Predicate>
    typename iterator_traits<_IIter>::difference_type
    count_if(_IIter, _IIter, _Predicate);

  template<typename _IIter, typename _Predicate>
    typename iterator_traits<_IIter>::difference_type
    count_if(_IIter, _IIter, _Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _Predicate>
    typename iterator_traits<_IIter>::difference_type
    count_if(_IIter, _IIter, _Predicate, __gnu_parallel::_Parallelism);

  template<typename _IIter, typename _Predicate, typename _IterTag>
    typename iterator_traits<_IIter>::difference_type
    count_if_switch(_IIter, _IIter, _Predicate, _IterTag);

  template<typename _RAIter, typename _Predicate>
    typename iterator_traits<_RAIter>::difference_type
    count_if_switch(_RAIter, _RAIter, _Predicate, random_access_iterator_tag,
		    __gnu_parallel::_Parallelism);

  // algobase.h
  template<typename _IIter1, typename _IIter2>
    bool
    equal(_IIter1, _IIter1, _IIter2, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename Predicate>
    bool
    equal(_IIter1, _IIter1, _IIter2, Predicate,
	  __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2>
    bool
    equal(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename Predicate>
    bool
    equal(_IIter1, _IIter1, _IIter2, Predicate);

  template<typename _IIter, typename _Tp>
    _IIter
    find(_IIter, _IIter, const _Tp&, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _Tp>
    _IIter
    find(_IIter, _IIter, const _Tp& val);

  template<typename _IIter, typename _Tp, typename _IterTag>
    _IIter
    find_switch(_IIter, _IIter, const _Tp&, _IterTag);

  template<typename _RAIter, typename _Tp>
    _RAIter
    find_switch(_RAIter, _RAIter, const _Tp&, random_access_iterator_tag);

  template<typename _IIter, typename _Predicate>
    _IIter
    find_if(_IIter, _IIter, _Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _Predicate>
    _IIter
    find_if(_IIter, _IIter, _Predicate);

  template<typename _IIter, typename _Predicate, typename _IterTag>
    _IIter
    find_if_switch(_IIter, _IIter, _Predicate, _IterTag);

  template<typename _RAIter, typename _Predicate>
    _RAIter
    find_if_switch(_RAIter, _RAIter, _Predicate, random_access_iterator_tag);

  template<typename _IIter, typename _FIter>
    _IIter
    find_first_of(_IIter, _IIter, _FIter, _FIter,
		  __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _FIter, typename _BiPredicate>
    _IIter
    find_first_of(_IIter, _IIter, _FIter, _FIter, _BiPredicate,
		  __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _FIter, typename _BiPredicate>
    _IIter
    find_first_of(_IIter, _IIter, _FIter, _FIter, _BiPredicate);

  template<typename _IIter, typename _FIter>
    _IIter
    find_first_of(_IIter, _IIter, _FIter, _FIter);

  template<typename _IIter, typename _FIter,
	   typename _IterTag1, typename _IterTag2>
    _IIter
    find_first_of_switch(_IIter, _IIter, _FIter, _FIter, _IterTag1, _IterTag2);

  template<typename _RAIter, typename _FIter, typename _BiPredicate,
	   typename _IterTag>
    _RAIter
    find_first_of_switch(_RAIter, _RAIter, _FIter, _FIter, _BiPredicate,
			 random_access_iterator_tag, _IterTag);

  template<typename _IIter, typename _FIter, typename _BiPredicate,
	   typename _IterTag1, typename _IterTag2>
    _IIter
    find_first_of_switch(_IIter, _IIter, _FIter, _FIter, _BiPredicate,
			 _IterTag1, _IterTag2);


  template<typename _IIter, typename _Function>
    _Function
    for_each(_IIter, _IIter, _Function);

  template<typename _IIter, typename _Function>
    _Function
    for_each(_IIter, _IIter, _Function, __gnu_parallel::sequential_tag);

  template<typename _Iterator, typename _Function>
    _Function
    for_each(_Iterator, _Iterator, _Function, __gnu_parallel::_Parallelism);

  template<typename _IIter, typename _Function, typename _IterTag>
    _Function
    for_each_switch(_IIter, _IIter, _Function, _IterTag);

  template<typename _RAIter, typename _Function>
    _Function
    for_each_switch(_RAIter, _RAIter, _Function, random_access_iterator_tag, 
		    __gnu_parallel::_Parallelism);


  template<typename _FIter, typename _Generator>
    void
    generate(_FIter, _FIter, _Generator);

  template<typename _FIter, typename _Generator>
    void
    generate(_FIter, _FIter, _Generator, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Generator>
    void
    generate(_FIter, _FIter, _Generator, __gnu_parallel::_Parallelism);

  template<typename _FIter, typename _Generator, typename _IterTag>
    void
    generate_switch(_FIter, _FIter, _Generator, _IterTag);

  template<typename _RAIter, typename _Generator>
    void
    generate_switch(_RAIter, _RAIter, _Generator, random_access_iterator_tag, 
		    __gnu_parallel::_Parallelism);

  template<typename _OIter, typename _Size, typename _Generator>
    _OIter
    generate_n(_OIter, _Size, _Generator);

  template<typename _OIter, typename _Size, typename _Generator>
    _OIter
    generate_n(_OIter, _Size, _Generator, __gnu_parallel::sequential_tag);

  template<typename _OIter, typename _Size, typename _Generator>
    _OIter
    generate_n(_OIter, _Size, _Generator, __gnu_parallel::_Parallelism);

  template<typename _OIter, typename _Size, typename _Generator,
	   typename _IterTag>
    _OIter
    generate_n_switch(_OIter, _Size, _Generator, _IterTag);

  template<typename _RAIter, typename _Size, typename _Generator>
    _RAIter
    generate_n_switch(_RAIter, _Size, _Generator, random_access_iterator_tag, 
		      __gnu_parallel::_Parallelism);

  template<typename _IIter1, typename _IIter2>
    bool
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2,
			    __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _Predicate>
    bool
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2, _Predicate,
			    __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2>
    bool
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _Predicate>
    bool
    lexicographical_compare(_IIter1, _IIter1, _IIter2, _IIter2, _Predicate);

  template<typename _IIter1, typename _IIter2,
	   typename _Predicate, typename _IterTag1, typename _IterTag2>
    bool
    lexicographical_compare_switch(_IIter1, _IIter1, _IIter2, _IIter2,
				   _Predicate, _IterTag1, _IterTag2);

  template<typename _RAIter1, typename _RAIter2, typename _Predicate>
    bool
    lexicographical_compare_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2,
				   _Predicate, random_access_iterator_tag,
				   random_access_iterator_tag);

  // algo.h
  template<typename _IIter1, typename _IIter2>
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _Predicate>
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2, _Predicate,
	     __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2>
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2);

  template<typename _IIter1, typename _IIter2, typename _Predicate>
    pair<_IIter1, _IIter2>
    mismatch(_IIter1, _IIter1, _IIter2, _Predicate);

  template<typename _IIter1, typename _IIter2, typename _Predicate,
	   typename _IterTag1, typename _IterTag2>
    pair<_IIter1, _IIter2>
    mismatch_switch(_IIter1, _IIter1, _IIter2, _Predicate,
		    _IterTag1, _IterTag2);

  template<typename _RAIter1, typename _RAIter2, typename _Predicate>
    pair<_RAIter1, _RAIter2>
    mismatch_switch(_RAIter1, _RAIter1, _RAIter2, _Predicate,
		    random_access_iterator_tag, random_access_iterator_tag);

  template<typename _FIter1, typename _FIter2>
    _FIter1
    search(_FIter1, _FIter1, _FIter2, _FIter2, __gnu_parallel::sequential_tag);

  template<typename _FIter1, typename _FIter2>
    _FIter1
    search(_FIter1, _FIter1, _FIter2, _FIter2);

  template<typename _FIter1, typename _FIter2, typename _BiPredicate>
    _FIter1
    search(_FIter1, _FIter1, _FIter2, _FIter2, _BiPredicate,
	   __gnu_parallel::sequential_tag);

  template<typename _FIter1, typename _FIter2, typename _BiPredicate>
    _FIter1
    search(_FIter1, _FIter1, _FIter2, _FIter2, _BiPredicate);

  template<typename _RAIter1, typename _RAIter2>
    _RAIter1
    search_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2,
		  random_access_iterator_tag, random_access_iterator_tag);

  template<typename _FIter1, typename _FIter2, typename _IterTag1,
	   typename _IterTag2>
    _FIter1
    search_switch(_FIter1, _FIter1, _FIter2, _FIter2, _IterTag1, _IterTag2);

  template<typename _RAIter1, typename _RAIter2, typename _BiPredicate>
    _RAIter1
    search_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, _BiPredicate,
		  random_access_iterator_tag, random_access_iterator_tag);

  template<typename _FIter1, typename _FIter2, typename _BiPredicate,
	   typename _IterTag1, typename _IterTag2>
    _FIter1
    search_switch(_FIter1, _FIter1, _FIter2, _FIter2, _BiPredicate,
		  _IterTag1, _IterTag2);

  template<typename _FIter, typename _Integer, typename _Tp>
    _FIter
    search_n(_FIter, _FIter, _Integer, const _Tp&,
	     __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Integer, typename _Tp,
	   typename _BiPredicate>
    _FIter
    search_n(_FIter, _FIter, _Integer, const _Tp&, _BiPredicate,
	     __gnu_parallel::sequential_tag);
    
  template<typename _FIter, typename _Integer, typename _Tp>
    _FIter
    search_n(_FIter, _FIter, _Integer, const _Tp&);

  template<typename _FIter, typename _Integer, typename _Tp,
	   typename _BiPredicate>
    _FIter
    search_n(_FIter, _FIter, _Integer, const _Tp&, _BiPredicate);

  template<typename _RAIter, typename _Integer, typename _Tp,
	   typename _BiPredicate>
    _RAIter
    search_n_switch(_RAIter, _RAIter, _Integer, const _Tp&,
		    _BiPredicate, random_access_iterator_tag);

  template<typename _FIter, typename _Integer, typename _Tp,
	   typename _BiPredicate, typename _IterTag>
    _FIter
    search_n_switch(_FIter, _FIter, _Integer, const _Tp&,
		    _BiPredicate, _IterTag);


  template<typename _IIter, typename _OIter, typename UnaryOperation>
    _OIter
    transform(_IIter, _IIter, _OIter, UnaryOperation);

  template<typename _IIter, typename _OIter, typename UnaryOperation>
    _OIter
    transform(_IIter, _IIter, _OIter, UnaryOperation, 
	      __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename UnaryOperation>
    _OIter
    transform(_IIter, _IIter, _OIter, UnaryOperation, 
	      __gnu_parallel::_Parallelism);

  template<typename _IIter, typename _OIter, typename UnaryOperation,
	   typename _IterTag1, typename _IterTag2>
    _OIter
    transform1_switch(_IIter, _IIter, _OIter, UnaryOperation, 
		      _IterTag1, _IterTag2);
    

  template<typename _RAIIter, typename _RAOIter, typename UnaryOperation>
    _RAOIter
    transform1_switch(_RAIIter, _RAIIter, _RAOIter, UnaryOperation, 
		      random_access_iterator_tag, random_access_iterator_tag, 
		      __gnu_parallel::_Parallelism);


  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _BiOperation>
    _OIter
    transform(_IIter1, _IIter1, _IIter2, _OIter, _BiOperation);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _BiOperation>
    _OIter
    transform(_IIter1, _IIter1, _IIter2, _OIter, _BiOperation, 
	      __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _BiOperation>
    _OIter
    transform(_IIter1, _IIter1, _IIter2, _OIter, _BiOperation, 
	      __gnu_parallel::_Parallelism);

  template<typename _RAIter1, typename _RAIter2, typename _RAIter3,
	   typename _BiOperation>
    _RAIter3
    transform2_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter3, _BiOperation, 
		      random_access_iterator_tag, random_access_iterator_tag, 
		      random_access_iterator_tag,
		      __gnu_parallel::_Parallelism);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _BiOperation, typename _Tag1,
	   typename _Tag2, typename _Tag3>
    _OIter
    transform2_switch(_IIter1, _IIter1, _IIter2, _OIter, _BiOperation, 
		      _Tag1, _Tag2, _Tag3);


  template<typename _FIter, typename _Tp>
    void
    replace(_FIter, _FIter, const _Tp&, const _Tp&);

  template<typename _FIter, typename _Tp>
    void
    replace(_FIter, _FIter, const _Tp&, const _Tp&, 
	    __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Tp>
    void
    replace(_FIter, _FIter, const _Tp&, const _Tp&,
	    __gnu_parallel::_Parallelism);

  template<typename _FIter, typename _Tp, typename _IterTag>
    void
    replace_switch(_FIter, _FIter, const _Tp&, const _Tp&, _IterTag);

  template<typename _RAIter, typename _Tp>
    void
    replace_switch(_RAIter, _RAIter, const _Tp&, const _Tp&, 
		   random_access_iterator_tag, __gnu_parallel::_Parallelism);


  template<typename _FIter, typename _Predicate, typename _Tp>
    void
    replace_if(_FIter, _FIter, _Predicate, const _Tp&);

  template<typename _FIter, typename _Predicate, typename _Tp>
    void
    replace_if(_FIter, _FIter, _Predicate, const _Tp&,
	       __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Predicate, typename _Tp>
    void
    replace_if(_FIter, _FIter, _Predicate, const _Tp&,
	       __gnu_parallel::_Parallelism);

  template<typename _FIter, typename _Predicate, typename _Tp,
	   typename _IterTag>
    void
    replace_if_switch(_FIter, _FIter, _Predicate, const _Tp&, _IterTag);
 
  template<typename _RAIter, typename _Predicate, typename _Tp>
    void
    replace_if_switch(_RAIter, _RAIter, _Predicate, const _Tp&,
		      random_access_iterator_tag,
		      __gnu_parallel::_Parallelism);


  template<typename _FIter>
    _FIter
    max_element(_FIter, _FIter);

  template<typename _FIter>
    _FIter
    max_element(_FIter, _FIter, __gnu_parallel::sequential_tag);

  template<typename _FIter>
    _FIter
    max_element(_FIter, _FIter, __gnu_parallel::_Parallelism);

  template<typename _FIter, typename _Compare>
    _FIter
    max_element(_FIter, _FIter, _Compare);

  template<typename _FIter, typename _Compare>
    _FIter
    max_element(_FIter, _FIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Compare>
    _FIter
    max_element(_FIter, _FIter, _Compare, __gnu_parallel::_Parallelism);

  template<typename _FIter, typename _Compare, typename _IterTag>
    _FIter
    max_element_switch(_FIter, _FIter, _Compare, _IterTag);

  template<typename _RAIter, typename _Compare>
    _RAIter
    max_element_switch(_RAIter, _RAIter, _Compare, random_access_iterator_tag, 
		       __gnu_parallel::_Parallelism);


  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, 
	  __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare>
    _OIter
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare, 
	  __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare>
    _OIter
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    merge(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare, typename _IterTag1, typename _IterTag2,
	   typename _IterTag3>
    _OIter
    merge_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare, 
		 _IterTag1, _IterTag2, _IterTag3);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Compare>
    _OIter
    merge_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Compare, 
		 random_access_iterator_tag, random_access_iterator_tag, 
		 random_access_iterator_tag);


  template<typename _FIter>
    _FIter
    min_element(_FIter, _FIter);

  template<typename _FIter>
    _FIter
    min_element(_FIter, _FIter, __gnu_parallel::sequential_tag);

  template<typename _FIter>
    _FIter
    min_element(_FIter, _FIter, __gnu_parallel::_Parallelism parallelism_tag);

  template<typename _FIter, typename _Compare>
    _FIter
    min_element(_FIter, _FIter, _Compare);

  template<typename _FIter, typename _Compare>
    _FIter
    min_element(_FIter, _FIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _FIter, typename _Compare>
    _FIter
    min_element(_FIter, _FIter, _Compare, __gnu_parallel::_Parallelism);

  template<typename _FIter, typename _Compare, typename _IterTag>
    _FIter
    min_element_switch(_FIter, _FIter, _Compare, _IterTag);

  template<typename _RAIter, typename _Compare>
    _RAIter
    min_element_switch(_RAIter, _RAIter, _Compare, random_access_iterator_tag, 
		       __gnu_parallel::_Parallelism);

  template<typename _RAIter>
    void
    nth_element(_RAIter, _RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
    void
    nth_element(_RAIter, _RAIter, _RAIter, _Compare,
		__gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
    void
    nth_element(_RAIter, _RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void
    nth_element(_RAIter, _RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void
    partial_sort(_RAIter, _RAIter, _RAIter, _Compare,
		 __gnu_parallel::sequential_tag);

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
    _FIter
    partition(_FIter, _FIter, Predicate, __gnu_parallel::sequential_tag);
    
  template<typename _FIter, typename Predicate>
    _FIter
    partition(_FIter, _FIter, Predicate);

  template<typename _FIter, typename Predicate, typename _IterTag>
    _FIter
    partition_switch(_FIter, _FIter, Predicate, _IterTag);
    
  template<typename _RAIter, typename Predicate>
    _RAIter
    partition_switch(_RAIter, _RAIter, Predicate, random_access_iterator_tag);

  template<typename _RAIter>
    void
    random_shuffle(_RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _RandomNumberGenerator>
    void
    random_shuffle(_RAIter, _RAIter, _RandomNumberGenerator&,
		   __gnu_parallel::sequential_tag);

  template<typename _RAIter>
    void
    random_shuffle(_RAIter, _RAIter);

  template<typename _RAIter, typename _RandomNumberGenerator>
    void
    random_shuffle(_RAIter, _RAIter, _RandomNumberGenerator&);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
	    __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename Predicate>
    _OIter
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, Predicate,
	      __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Predicate>
    _OIter 
    set_union(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Predicate);

  template<typename _IIter1, typename _IIter2, typename _Predicate,
	   typename _OIter, typename _IterTag1, typename _IterTag2,
	   typename _IterTag3>
    _OIter
    set_union_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
		     _Predicate, _IterTag1, _IterTag2, _IterTag3);

  template<typename _RAIter1, typename _RAIter2, typename _Output_RAIter,
	   typename _Predicate>
    _Output_RAIter
    set_union_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2, _Output_RAIter,
		     _Predicate, random_access_iterator_tag,
		     random_access_iterator_tag, random_access_iterator_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
		     __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Predicate>
    _OIter
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Predicate,
		     __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Predicate>
    _OIter 
    set_intersection(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Predicate);

  template<typename _IIter1, typename _IIter2, typename _Predicate,
	   typename _OIter, typename _IterTag1, typename _IterTag2,
	   typename _IterTag3>
    _OIter
    set_intersection_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
			    _Predicate, _IterTag1, _IterTag2, _IterTag3);

  template<typename _RAIter1, typename _RAIter2, typename _Output_RAIter,
	   typename _Predicate>
    _Output_RAIter
    set_intersection_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2,
			    _Output_RAIter, _Predicate,
			    random_access_iterator_tag,
			    random_access_iterator_tag,
			    random_access_iterator_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
			     __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Predicate>
    _OIter
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
			     _Predicate, __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter 
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Predicate>
    _OIter 
    set_symmetric_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
			     _Predicate);

  template<typename _IIter1, typename _IIter2, typename _Predicate,
	   typename _OIter, typename _IterTag1, typename _IterTag2,
	   typename _IterTag3>
    _OIter
    set_symmetric_difference_switch(_IIter1, _IIter1, _IIter2, _IIter2,
				    _OIter, _Predicate, _IterTag1, _IterTag2,
				    _IterTag3);

  template<typename _RAIter1, typename _RAIter2, typename _Output_RAIter,
	   typename _Predicate>
    _Output_RAIter
    set_symmetric_difference_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2,
				    _Output_RAIter, _Predicate,
				    random_access_iterator_tag,
				    random_access_iterator_tag,
				    random_access_iterator_tag);


  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
		   __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Predicate>
    _OIter
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Predicate,
		   __gnu_parallel::sequential_tag);

  template<typename _IIter1, typename _IIter2, typename _OIter>
    _OIter
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter);

  template<typename _IIter1, typename _IIter2, typename _OIter,
	   typename _Predicate>
    _OIter
    set_difference(_IIter1, _IIter1, _IIter2, _IIter2, _OIter, _Predicate);

  template<typename _IIter1, typename _IIter2, typename _Predicate,
	   typename _OIter, typename _IterTag1, typename _IterTag2,
	   typename _IterTag3>
    _OIter
    set_difference_switch(_IIter1, _IIter1, _IIter2, _IIter2, _OIter,
			  _Predicate, _IterTag1, _IterTag2, _IterTag3);

  template<typename _RAIter1, typename _RAIter2, typename _Output_RAIter,
	   typename _Predicate>
    _Output_RAIter
    set_difference_switch(_RAIter1, _RAIter1, _RAIter2, _RAIter2,
			  _Output_RAIter, _Predicate,
			  random_access_iterator_tag,
			  random_access_iterator_tag,
			  random_access_iterator_tag);


  template<typename _RAIter>
    void
    sort(_RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
    void
    sort(_RAIter, _RAIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _RAIter>
    void
    sort(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void
    sort(_RAIter, _RAIter, _Compare);

  template<typename _RAIter>
    void
    stable_sort(_RAIter, _RAIter, __gnu_parallel::sequential_tag);

  template<typename _RAIter, typename _Compare>
    void
    stable_sort(_RAIter, _RAIter, _Compare, __gnu_parallel::sequential_tag);

  template<typename _RAIter>
    void
    stable_sort(_RAIter, _RAIter);

  template<typename _RAIter, typename _Compare>
    void
    stable_sort(_RAIter, _RAIter, _Compare);

  template<typename _IIter, typename _OIter>
    _OIter
    unique_copy(_IIter, _IIter, _OIter, __gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter, typename _Predicate>
    _OIter
    unique_copy(_IIter, _IIter, _OIter, _Predicate,
		__gnu_parallel::sequential_tag);

  template<typename _IIter, typename _OIter>
    _OIter
    unique_copy(_IIter, _IIter, _OIter);

  template<typename _IIter, typename _OIter, typename _Predicate>
    _OIter
    unique_copy(_IIter, _IIter, _OIter, _Predicate);

  template<typename _IIter, typename _OIter, typename _Predicate,
	   typename _IterTag1, typename _IterTag2>
    _OIter
    unique_copy_switch(_IIter, _IIter, _OIter, _Predicate,
		       _IterTag1, _IterTag2);

  template<typename _RAIter, typename _RandomAccess_OIter, typename _Predicate>
    _RandomAccess_OIter
    unique_copy_switch(_RAIter, _RAIter, _RandomAccess_OIter, _Predicate,
		       random_access_iterator_tag, random_access_iterator_tag);
} // end namespace __parallel
} // end namespace std

#endif
